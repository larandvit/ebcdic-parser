"""
Description:
            Convert EBCDIC data to delimited text format
            
            The functionality covers cases
                1) Single schema data
                2) Multi-schema fixed length
                3) Multi-schema variable length
                
            Parameters/Exit code: run application with option -h or --help. There are a set of mandatory and a set of optional parameters
"""

# this is a way to exclude javabridge functionality. The funnctionality is requested by Java encoding.
# If it needs to run the application without installing javabridge library. Java encoding is not available in that case
JAVABRIDGEINCLUDED = True

import os.path as path
import os
import sys
import traceback

import json
import codecs
import uuid
import datetime
import argparse
from argparse import RawTextHelpFormatter
if JAVABRIDGEINCLUDED:
    import javabridge
    import numpy
from enum import IntEnum

import re

import unicodedata

__author__ = "Vitaly Saversky"
__date__ = "2017-10-04"
__credits__ = ["Vitaly Saversky"]
__version__ = "2.1.0"
__maintainer__ = "Vitaly Saversky"
__email__ = "larandvit@hotmail.com"
__status__ = "Development"

class FileFormat(IntEnum):
    SingleSchema = 1
    MultiSchemaFixed = 2
    MultiSchemaVariable = 3
    
NEWLINE = "\n"
LOGFILENAME = "ebcdic_parser.log"

SKIPFIELDTYPENAME = 'skip'

DELIMITER = "\t"
OUTPUTFILEEXTENSION = ".txt"
IGNORECONVERSIONERRORS = False
VERBOSE = True
GROUPRECORDS = False
GROUPRECORDSLEVEL2 = False

PYTHONENCODING = True
INPUTENCODING = "cp037"
OUTPUTENCODING = "utf_8"

LAYOUTELEMENT_DESCRIPTION = "description"
LAYOUTELEMENT_KEYFIELDS = "keyfields"
LAYOUTELEMENT_LAYOUTS = "layouts"
LAYOUTELEMENT_LAYOUTTYPE = "layouttype"
LAYOUTELEMENT_LAYOUTTYPECONDITIONAL = "layouttypeconditional"
LAYOUTELEMENT_LAYOUT = "layout"
LAYOUTELEMENT_VARIABLELAYOUT = "layoutvariable"
LAYOUTELEMENT_TERMINATOR = "terminator"

LAYOUTELEMENT_HEADER = "header"
LAYOUTELEMENT_FOOTER = "footer"

LAYOUTELEMENT_NAME = "name"
LAYOUTELEMENT_TYPE = "type"
LAYOUTELEMENT_SIZE = "size"
LAYOUTELEMENT_SCALE = "scale"
LAYOUTELEMENT_KEYTYPE = "keytype"

LAYOUTVALUE_KEYTYPE_LAYOUTTYPE = "layouttype"
LAYOUTVALUE_KEYTYPE_LAYOUTSIZE = "layoutsize"

LAYOUTLIST_FIELDS = [LAYOUTELEMENT_NAME, LAYOUTELEMENT_TYPE, LAYOUTELEMENT_SIZE, LAYOUTELEMENT_SCALE]
LAYOUTLIST_KEYFIELDS = [LAYOUTELEMENT_NAME, LAYOUTELEMENT_TYPE, LAYOUTELEMENT_SIZE, LAYOUTELEMENT_SCALE, LAYOUTELEMENT_KEYTYPE]

FILEFORMATS = {FileFormat.SingleSchema:"Single schema",
               FileFormat.MultiSchemaFixed:"Multi-schema fixed length",
               FileFormat.MultiSchemaVariable:"Multi-schema variable length",
              }

class KnownIssue(Exception):
    pass

class Logger(object):
    def __init__(self, logFile, verbose):
        self.terminal = sys.stdout
        self.log = open(logFile, "a")
        self.verbose = verbose

    def write(self, message):
        if(self.verbose):
            self.terminal.write(message)
            self.terminal.flush()
        self.log.write(message)
        self.log.flush()

    def flush(self):
        #this flush method is needed for python 3 compatibility.
        pass    

if JAVABRIDGEINCLUDED:
    class JavaEncoder:
        new_fn = javabridge.make_new("java/lang/String", "([BLjava/lang/String;)V")
        def __init__(self, i, s):
            self.new_fn(i, s)
        toString = javabridge.make_method("toString", "()Ljava/lang/String;", "Retrieve the integer value")

class LayoutField():
    def __init__(self):
        self.name = None
        self.type = None
        self.size = None
        self.scale = None
        
        self.start = None
        
class LayoutKeyField(LayoutField):
    def __init__(self):
        LayoutField.__init__(self)
        self.keytype = None
        self.rawValue = None
        self.convertedValue = None

class LayoutFields():
    def __init__(self):
        self.layoutType = None
        self.isLayoutConditional = None
        self.fields = []
        self.layoutSize = None
        self.variableFields = []
        self.variableLayoutSize = None
        self.isVariableFields = None
        self.terminatorSize = None

class LayoutDefinition():
    def __init__(self):
        
        self.isError = True
        self.errorDescription = None
        
        self.description = "Not found"
        
        self.keyFields = []
        
        self.isKeyLayoutType = None
        self.isKeyLayoutSize = None
        self.layouts = []
        
        self.fileFormat = None
        
        self.isVariableFields = None
        self.isMultipleLayouts = None
        
        self.header = None
        self.footer = None
        self.isHeader = None
        self.isFooter = None
        
        self.keyLayoutType = None
        self.keyLayoutSize = None

    def loadLayouts(self, layoutPath):
        try:
            self.layoutPath = layoutPath
            
            with open(layoutPath,'r') as fileLayout:
                
                layouts=json.load(fileLayout)
                
                #read description
                elementName = LAYOUTELEMENT_DESCRIPTION
                if elementName in layouts:
                    self.description = layouts[elementName]
                
                elementName = LAYOUTELEMENT_HEADER
                if elementName in layouts:
                    if len(layouts[elementName])==1:
                        self.header = LayoutField()
                        field = layouts[elementName][0]
                        for elementName in LAYOUTLIST_FIELDS:
                            if elementName in field:
                                setattr(self.header, elementName, field[elementName])
                            else:
                                if elementName not in [LAYOUTELEMENT_SCALE]:
                                    self.errorDescription = "'" + elementName + "' element field is not found: " + str(field)
                                    return not self.isError
                    else:
                        self.errorDescription = "'" + elementName + "' element doesn't include 1 field. It's supported only 1 field in it"
                        return not self.isError
                    
                self.isHeader = not self.header==None
                
                elementName = LAYOUTELEMENT_FOOTER
                if elementName in layouts:
                    if len(layouts[elementName])==1:
                        self.footer = LayoutField()
                        field = layouts[elementName][0]
                        for elementName in LAYOUTLIST_FIELDS:
                                if elementName in field:
                                    setattr(self.footer, elementName, field[elementName])
                                else:
                                    if elementName not in [LAYOUTELEMENT_SCALE]:
                                        self.errorDescription = "'" + elementName + "' element field is not found: " + str(field)
                                        return not self.isError
                    else:
                        self.errorDescription = "'" + elementName + "' element doesn't include 1 field. It's supported only 1 field in it"
                        return not self.isError
                    
                self.isFooter = not self.footer==None
                
                #key fields
                elementName = LAYOUTELEMENT_KEYFIELDS
                if elementName in layouts:
                    keyFields = layouts[elementName]
                    
                    for keyField in keyFields:
                        layoutKeyField = LayoutKeyField()
                        
                        for elementName in LAYOUTLIST_KEYFIELDS:
                            if elementName in keyField:
                                setattr(layoutKeyField, elementName, keyField[elementName])
                            else:
                                if elementName not in [LAYOUTELEMENT_SCALE, LAYOUTELEMENT_KEYTYPE]:
                                    self.errorDescription = "Key fileds: '" + elementName + "' element is not found: " + str(keyField)
                                    return not self.isError

                        self.keyFields.append(layoutKeyField)

                    #figure out how many key type fields added
                    layoutTypeCounter = 0
                    layoutSizeCounter = 0
                    elementName = LAYOUTELEMENT_KEYTYPE
                    for layoutKeyField in self.keyFields:
                        if layoutKeyField.keytype!=None:
                            if layoutKeyField.keytype==LAYOUTVALUE_KEYTYPE_LAYOUTTYPE:
                                layoutTypeCounter += 1
                                self.keyLayoutType = layoutKeyField
                            if layoutKeyField.keytype==LAYOUTVALUE_KEYTYPE_LAYOUTSIZE:
                                layoutSizeCounter += 1
                                self.keyLayoutSize = layoutKeyField
                                
                    #validate if no more than one key type field
                    if layoutTypeCounter>1:
                        self.errorDescription = "Key fileds: 'keytype' element is repeated with '" + LAYOUTVALUE_KEYTYPE_LAYOUTTYPE + "' value " + str(layoutTypeCounter) + " times"
                        return not self.isError
                    
                    if layoutSizeCounter>1:
                        self.errorDescription = "Key fileds: 'keytype' element is repeated with '" + LAYOUTVALUE_KEYTYPE_LAYOUTSIZE + "' value " + str(layoutTypeCounter) + " times"
                        return not self.isError
                    
                    #store key type flags for quick and easy access
                    self.isKeyLayoutType = layoutTypeCounter==1
                    self.isKeyLayoutSize = layoutSizeCounter==1
                
                layoutIndex = -1
                for layout in layouts[LAYOUTELEMENT_LAYOUTS]:
                    
                    layoutIndex += 1
                    
                    #check if both layout types are present
                    if LAYOUTELEMENT_LAYOUTTYPE in layout and LAYOUTELEMENT_LAYOUTTYPECONDITIONAL in layout:
                        self.errorDescription = "Both '" + LAYOUTELEMENT_LAYOUTTYPE + "' and '" + LAYOUTELEMENT_LAYOUTTYPECONDITIONAL + "' are found in layout with index #" + str(layoutIndex)
                        return not self.isError
                    
                    #check if no layout type at all
                    if LAYOUTELEMENT_LAYOUTTYPE not in layout and LAYOUTELEMENT_LAYOUTTYPECONDITIONAL not in layout:
                        self.errorDescription = "'" + LAYOUTELEMENT_LAYOUTTYPE + "' or '" + LAYOUTELEMENT_LAYOUTTYPECONDITIONAL + "' is not found in layout with index #" + str(layoutIndex)
                        return not self.isError
                    
                    layoutFields = LayoutFields()
                    
                    # TODO (1) validation and (2) parse condition
                    layoutFields.isLayoutConditional = LAYOUTELEMENT_LAYOUTTYPECONDITIONAL in layout
                    if LAYOUTELEMENT_LAYOUTTYPE in layout:
                        layoutFields.layoutType = layout[LAYOUTELEMENT_LAYOUTTYPE]
                    else:
                        layoutFields.layoutType = layout[LAYOUTELEMENT_LAYOUTTYPECONDITIONAL]
                    
                    #Terminator
                    elementName = LAYOUTELEMENT_TERMINATOR
                    if elementName in layout:
                        terminator = layout[elementName]
                        elementName = LAYOUTELEMENT_SIZE
                        if elementName in terminator:
                            layoutFields.terminatorSize = terminator[elementName]
                        else:
                            self.errorDescription = "Layout: '" + elementName + "' element is not found in '" + LAYOUTELEMENT_TERMINATOR + "' of '" + layoutFields.layoutType + "': " + str(field)
                            return not self.isError
                    
                    #add start position calc total size for static fields
                    startPosition = 0
                    layoutFields.layoutSize = 0
                    for field in layout[LAYOUTELEMENT_LAYOUT]:
                        layoutField = LayoutField()
                        
                        for elementName in LAYOUTLIST_FIELDS:
                            if elementName in field:
                                setattr(layoutField, elementName, field[elementName])
                            else:
                                if elementName not in [LAYOUTELEMENT_SCALE]:
                                    self.errorDescription = "Fileds: '" + elementName + "' element is not found: " + str(field)
                                    return not self.isError
                           
                        layoutField.start = startPosition
                        startPosition = startPosition + field[LAYOUTELEMENT_SIZE]
                        layoutFields.layoutSize += field[LAYOUTELEMENT_SIZE]
                        
                        layoutFields.fields.append(layoutField)
                    
                    #add start position and calc total size for variable fields
                    startPosition = 0
                    layoutFields.variableLayoutSize = 0
                    if(LAYOUTELEMENT_VARIABLELAYOUT in layout):
                        for field in layout[LAYOUTELEMENT_VARIABLELAYOUT]:
                            variableLayoutField = LayoutField()
                            
                            for elementName in LAYOUTLIST_FIELDS:
                                if elementName in field:
                                    setattr(variableLayoutField, elementName, field[elementName])
                                else:
                                    if elementName not in [LAYOUTELEMENT_SCALE]:
                                        self.errorDescription = "Variable fileds: '" + elementName + "' element is not found: " + str(field)
                                        return not self.isError
                                
                            variableLayoutField.start = startPosition
                            startPosition = startPosition + field[LAYOUTELEMENT_SIZE]
                            layoutFields.variableLayoutSize += field[LAYOUTELEMENT_SIZE]
                            
                            layoutFields.variableFields.append(variableLayoutField)
                    
                    layoutFields.isVariableFields = len(layoutFields.variableFields)>0
                    self.layouts.append(layoutFields)
                
                self.isMultipleLayouts = len(self.layouts)>1
                
                self.isVariableFields = False
                for layoutFields in self.layouts:
                    if len(layoutFields.variableFields)>0:
                        self.isVariableFields = True
                        break
                
                #valdate key fields - type 1 and key layout type is provided
                if not self.isMultipleLayouts and self.isKeyLayoutType:
                    self.errorDescription = "Key type is provided but there is only 1 layout type"
                    return not self.isError
                
                #validate key fields - there are more than 1 layout but key layout type is not provided
                if self.isMultipleLayouts and not self.isKeyLayoutType:
                    self.errorDescription = "There are more than 1 layout but key layout type is not provided"
                    return not self.isError
                
                #calculate file format
                if not self.isMultipleLayouts:
                    self.fileFormat = FileFormat.SingleSchema.value
                elif self.isMultipleLayouts and not self.isVariableFields:
                    self.fileFormat = FileFormat.MultiSchemaFixed.value
                else:
                    self.fileFormat = FileFormat.MultiSchemaVariable.value
                
                self.isError = False
                    
        except (ValueError) as errValue:
            self.errorDescription = str(errValue)
        except Exception as err:
            error_class = err.__class__.__name__
            detail = err.args[0]
            cl, exc, tb = sys.exc_info()
            line_number = traceback.extract_tb(tb)[-1][1]
            errText = '{} at line {}: {}'.format(error_class, line_number, detail)
            if not str(sys.exc_info()[1])==str(detail):
                errText = errText + "\n" + str(sys.exc_info()[1])
            self.errorDescription = errText
         
        return not self.isError

def packedDecimal(fieldBytes, fieldDefinition, pythonEncoding, encodingName):
    n = [ '' ]
    for b in fieldBytes[:-1]:
        hi, lo = divmod( b, 16 )
        n.append(str(hi))
        n.append(str(lo))
    digit, sign = divmod(fieldBytes[-1], 16)
    n.append(str(digit))
    if sign in (0x0b, 0x0d ):
        n[0]= '-'

    buf = int(''.join(str(x) for x in n))
    
    if(not fieldDefinition.scale==None):
        decodedValue = buf / (10 ** fieldDefinition.scale)
    else:
        decodedValue = buf
    
    return decodedValue

def integer(fieldBytes, fieldDefinition, pythonEncoding, encodingName):
    decodedValue=int.from_bytes(fieldBytes, byteorder='big', signed=True)
    
    return decodedValue

def short(fieldBytes, fieldDefinition, pythonEncoding, encodingName):
    decodedValue =  integer(fieldBytes, fieldDefinition, pythonEncoding, encodingName)
    return decodedValue

def string(fieldBytes, fieldDefinition, pythonEncoding, encodingName):
    buf2 = ""
    if(pythonEncoding):
        ibmDecoder = codecs.getdecoder(encodingName)
        buf = ibmDecoder(fieldBytes)
        buf2 = buf[0]
    else:
        if JAVABRIDGEINCLUDED:
            array = numpy.array(list(fieldBytes) ,numpy.uint8)
            buf = JavaEncoder(array, encodingName)
            buf2 = buf.toString()
        else:
            raise KnownIssue("Java encoding is not available as JAVABRIDGEINCLUDED=False")
    decodedValue = controlCharRegex.sub("", buf2).strip()
    
    return decodedValue

def decimal(fieldBytes, fieldDefinition, pythonEncoding, encodingName):
    buf = string(fieldBytes, fieldDefinition, pythonEncoding, encodingName)
    if(buf==""):
        buf = "0"
    decodedValue = int(buf)
    if(not fieldDefinition.scale==None):
        decodedValue = decodedValue / (10 ** fieldDefinition.scale)
    
    return decodedValue;

def zonedDecimal(fieldBytes, fieldDefinition, pythonEncoding, encodingName):
    buf = string(fieldBytes, fieldDefinition, pythonEncoding, encodingName)
    if(buf==""):
        buf = "0"
    decodedValue = int(buf)
    if(not fieldDefinition.scale==None):
        decodedValue = decodedValue / (10 ** fieldDefinition.scale)
    
    return decodedValue;

def convertFunction(fieldType):
    convert=None
        
    if(fieldType=="packedDecimal"):
        convert=packedDecimal
    elif(fieldType=="integer"):
        convert=integer
    elif(fieldType=="short"):
        convert=short
    elif(fieldType=="string"):
        convert=string
    elif(fieldType=="decimal"):
        convert=decimal
    elif(fieldType=="zonedDecimal"):
        convert=zonedDecimal

    return convert

def convert_error_message(recordLayoutType, field, fieldBytes, catchedError):
    return "Conversation error\t" + "Layout type: " + recordLayoutType + "\t" + str(field.name) + "\t" + str(fieldBytes) + "\t" + str(catchedError)

returnCode = 1

# store references to open output files. Keys as file names
outputOpenFiles = {}

# it will allow don't show print statemnet in finally of catch
wrongArgumentsFlag = True

#store how many bytes read in case of error
NumberOfRecordsRead = 0
    
try:
    appDescription = "Convert EBCDIC data into delimited text format. Version " + __version__
    appDescription += "\n"
    appDescription += "\nSupported file formats:"
    formatIndex = 1
    for formartDesc in FILEFORMATS.values():
        appDescription += "\n" + "(" + str(formatIndex) + ") " + formartDesc
        formatIndex += 1
        
    appDescription += "\n\nFeatures"
    appDescription += "\n1. Python doesn't include enough code pages, so it's added Java code pages as well."
    appDescription += "\n   Java is implemented in javabridge module. If javabridge module is not installed, "
    appDescription += "\n   Java functionlaity can be disabled changing JAVABRIDGEINCLUDED = True to False in Python code"

    parser = argparse.ArgumentParser(description=appDescription, 
                                     epilog="Exit codes: 0 - successful completion, 1 - completion with any error",
                                     formatter_class=RawTextHelpFormatter)
    parser.add_argument("--inputfile", nargs=1, required=True, help="Input EBCDIC file path", metavar='"input file path"')
    parser.add_argument("--outputfolder", nargs=1, required=True, help="Output folder to store delimited files", metavar='"output folder"')
    parser.add_argument("--layoutfile", nargs=1, required=True, help="Layout file path", metavar='"layout file"')
    parser.add_argument("--outputdelimiter", nargs="?", default=DELIMITER, help="output text file delimiter", metavar='delimiter')
    parser.add_argument("--outputfileextension", nargs="?", default=OUTPUTFILEEXTENSION, help="output text file extension", metavar='extension')
    parser.add_argument("--ignoreconversionerrors", nargs="?", default="yes" if IGNORECONVERSIONERRORS else "no", choices=["yes","no"], help="ignore any conversion error", metavar='yes/no')
    parser.add_argument("--logfolder", nargs="?", default="", help="Output folder to store log file", metavar='log folder')
    parser.add_argument("--pythonencoding", nargs="?", default="yes" if PYTHONENCODING else "no", choices=["yes","no"], help="use Python encoding rather than Java", metavar='yes/no')
    parser.add_argument("--encodingname", nargs="?", default=INPUTENCODING, help="Code page name to encode characters (Python or Java)", metavar='encoding name')
    parser.add_argument("--grouprecords", nargs="?", default="yes" if GROUPRECORDS else "no", choices=["yes","no"], help="create relationships between records", metavar='yes/no')
    parser.add_argument("--grouprecordslevel2", nargs="?", default="yes" if GROUPRECORDSLEVEL2 else "no", choices=["yes","no"], help="create relationships between records for level 2", metavar='yes/no')
    parser.add_argument("--verbose", nargs="?", default="yes" if VERBOSE else "no", choices=["yes","no"], help="show information on screen", metavar='yes/no')

    args = parser.parse_args()
    wrongArgumentsFlag = False
    
    # mandatory arguments########################################################
    filePath = args.inputfile[0]
    outputFolder = args.outputfolder[0]
    layoutPath = args.layoutfile[0]
    ##############################################################################
    
    # optional arguments##########################################################
    DELIMITER = args.outputdelimiter
    OUTPUTFILEEXTENSION = args.outputfileextension
    IGNORECONVERSIONERRORS = True if args.ignoreconversionerrors=="yes" else False
    LOGFILEPATH = path.join(path.dirname(__file__) if args.logfolder=="" else args.logfolder, LOGFILENAME)
    PYTHONENCODING = True if args.pythonencoding=="yes" else False
    INPUTENCODING = args.encodingname
    GROUPRECORDS = True if args.grouprecords=="yes" else False
    GROUPRECORDSLEVEL2 = True if args.grouprecordslevel2=="yes" else False
    VERBOSE = True if args.verbose=="yes" else False
    ##############################################################################
    
    sys.stdout = Logger(LOGFILEPATH, VERBOSE)
    
    fileFolder = path.dirname(filePath)
    fileName, fileExtension  = path.splitext(path.basename(filePath))
    
    print("")
    print("Processing file...", fileName + fileExtension)
    print("Application version:", __version__)
    print("Stated:", datetime.datetime.now())
    print("Parameters:")
    print("-----------------------------------")
    print("Data folder: " + fileFolder)
    print("Output folder:", outputFolder)
    print("Layout file: " + layoutPath)
    print("Output delimiter:", r"\t" if DELIMITER=="\t" else DELIMITER)
    print("Output file extension: " + OUTPUTFILEEXTENSION)
    print("Ignore conversion errors:", args.ignoreconversionerrors)
    print("Log file: " + LOGFILEPATH)
    print("Python encoding: ", "yes" if PYTHONENCODING else "no")
    print("Encoding name: " + INPUTENCODING)
    print("Group records: ", "yes" if GROUPRECORDS else "no")
    print("Group records level 2: ", "yes" if GROUPRECORDSLEVEL2 else "no")
    print("Show information on screen:", args.verbose)
    
    # layout definitions and other suppelemental info
    layoutDefinition = LayoutDefinition()

    if not layoutDefinition.loadLayouts(layoutPath):
        raise KnownIssue(layoutDefinition.errorDescription)
    
    print("Format:", "(" + str(layoutDefinition.fileFormat) + ")", FILEFORMATS[layoutDefinition.fileFormat])
    
    print("-----------------------------------")
    
    if(not PYTHONENCODING):
        if JAVABRIDGEINCLUDED:
            javabridge.start_vm(run_headless=True)
        else:
            raise KnownIssue("Java encoding is not available as JAVABRIDGEINCLUDED=False")
    
    # store record count for each output file
    recordCount = {}
    
    # create a list of control (unreadable/invisible) characters
    allChars = (chr(i) for i in range(sys.maxunicode))
    controlChars = ''.join(c for c in allChars if unicodedata.category(c) == 'Cc')
    controlCharRegex = re.compile('[%s]' % re.escape(controlChars))
        
    with codecs.open(filePath, "rb") as f_in:
        
        #skip header data. Only "skip" data type works now
        if(layoutDefinition.isHeader):
            headerFieldByteSize = layoutDefinition.header.size
            f_in.read(headerFieldByteSize)
            NumberOfRecordsRead += headerFieldByteSize
        
        #stop processing if left bytes
        footerFieldByteSize = 0
        if(layoutDefinition.isFooter):
            footerFieldByteSize = layoutDefinition.footer.size
        
        fileSize = os.stat(filePath).st_size
        
        # read the first portion of data. In some cases, we read entire record, other cases, we read record size and/or record type
        if(layoutDefinition.isMultipleLayouts):
            for keyField in layoutDefinition.keyFields:
                recordSizeBytes = f_in.read(keyField.size)
                NumberOfRecordsRead += keyField.size
                if(recordSizeBytes):
                    keyField.rawValue = recordSizeBytes
                else:
                    break
        else:
            layoutFields = layoutDefinition.layouts[0];
            layoutSize = layoutFields.layoutSize
            recordSizeBytes = f_in.read(layoutSize)
            NumberOfRecordsRead += layoutSize
        
        # it shows related groups for each layout. A value is reset when the first layout is met. The first layout in a layout file defines relaetd group start
        relatedGroup = None
        #if GROUPRECORDS:
        #    relatedGroup = uuid.uuid4().hex
        
        # it shows related groups for each level 2 layout. A value is reset when the second layout is met. The second layout in a layout file defines relaetd group start
        relatedGroupLevel2 = None
        #if GROUPRECORDSLEVEL2:
        #    relatedGroupLevel2 = uuid.uuid4().hex
            
        #link between static and variable records. It's used to link parent static record with variable one
        unqKey = None
        if layoutDefinition.isVariableFields:
            unqKey = uuid.uuid4().hex
        
        while recordSizeBytes:
            
            #convert key values
            for keyField in layoutDefinition.keyFields:
                if keyField.type!=SKIPFIELDTYPENAME:
                    convert = convertFunction(keyField.type)
                    if(convert == None):
                        raise KnownIssue("Error in record layout type: " + "Layout type - unknown" + " Field name: " + keyField.name + "Field type - " + str(keyField.type))
                    else:
                        keyField.convertedValue = convert(keyField.rawValue, keyField, PYTHONENCODING, INPUTENCODING)           

            # check if record type in layouts and extract layout
            isFoundLayout = False
            if layoutDefinition.isMultipleLayouts:
                layoutTypeFields = []
                for layout in layoutDefinition.layouts:
                    if layout.isLayoutConditional:
                        condition = layout.layoutType[0:2]
                        conditionParameter = layout.layoutType[3:]
                        if condition=="ne":
                            if str(conditionParameter).strip()!=str(layoutDefinition.keyLayoutType.convertedValue).strip():
                                foundLayout = layout
                                isFoundLayout = True
                        else:
                            raise KnownIssue("Error in layout type condition: condition is not supported: " + layout.layoutType)
                    else:
                        if str(layout.layoutType).strip()==str(layoutDefinition.keyLayoutType.convertedValue).strip():
                            foundLayout = layout
                            isFoundLayout = True
                            break
            else:
                #there is only 1 layout
                isFoundLayout = True
                foundLayout = layoutDefinition.layouts[0]
            
            if(not isFoundLayout):
                # we need to store message in a variable because KnowIssue doesn't catch any errors if "str(recordLayoutType)" fails
                errorMessage = "Error in finding layout type in layout file: " + "Layout type in input file -" , str(layoutDefinition.keyLayoutType.convertedValue)
                raise KnownIssue(errorMessage)
            
            #layoutTypeName = "Skipped type "
            #if(len(layoutTypeFields)>0):
            layoutTypeName = "Processed type "
                    
            keyRecordCount = layoutTypeName + str(foundLayout.layoutType)
            if(keyRecordCount in recordCount):
                recordCount[keyRecordCount] += 1
            else:
                recordCount[keyRecordCount] = 1

            outputParentFileName = fileName + "_type_" + str(foundLayout.layoutType).replace(" ", "_")
            outputParentPath = path.join(outputFolder, outputParentFileName + OUTPUTFILEEXTENSION)
            
            #if((len(layoutTypeFields)>0):
            if(not outputParentFileName in outputOpenFiles):
                outputOpenFiles[outputParentFileName] = codecs.open(outputParentPath,"w", encoding=OUTPUTENCODING)

            f_out = outputOpenFiles[outputParentFileName]
            
            delimiter=""
            
            # read record data
            record = None
            if(layoutDefinition.isMultipleLayouts):
                if(foundLayout.isVariableFields and layoutDefinition.isKeyLayoutSize):
                    #this is Walmart EDI extraction logic
                    recordSize = layoutDefinition.keyLayoutSize.convertedValue - layoutDefinition.keyLayoutSize.size - layoutDefinition.keyLayoutType.size + foundLayout.terminatorSize
                    record = f_in.read(recordSize)
                    NumberOfRecordsRead += recordSize
                else:
                    recordSize = foundLayout.layoutSize
                    record = f_in.read(recordSize)
                    NumberOfRecordsRead += recordSize
            else:
                record = recordSizeBytes
            
            lastField = None
            currentRecordPosition = 0
            
            if layoutDefinition.isMultipleLayouts and GROUPRECORDS:
                if(str(layoutDefinition.layouts[0].layoutType).strip()==str(foundLayout.layoutType)):
                    relatedGroup = uuid.uuid4().hex
                    
            if layoutDefinition.isMultipleLayouts and GROUPRECORDSLEVEL2:
                if(str(layoutDefinition.layouts[1].layoutType).strip()==str(foundLayout.layoutType)):
                    relatedGroupLevel2 = uuid.uuid4().hex
            
            recordBuf = ""
            
            for field in foundLayout.fields:
                fieldName = field.name
                fieldType = field.type
                fieldSize = field.size
                fieldStart = field.start
                fieldEnd= fieldStart + fieldSize
                
                # don't extract field value if field type is "skip"
                if(fieldType!=SKIPFIELDTYPENAME):
                    convert = convertFunction(fieldType)
                    if(convert == None):
                        raise KnownIssue("Error in field\t" + "Layout type - " + foundLayout.layoutType + "\t" + str(field))
                    else:
                        try: 
                            recordBuf = recordBuf + delimiter + str(convert(record[fieldStart:fieldEnd], field, PYTHONENCODING, INPUTENCODING))
                        except UnicodeEncodeError as ex:
                            if(IGNORECONVERSIONERRORS):
                                print(convert_error_message(foundLayout.layoutType, field, record[fieldStart:fieldEnd], ex))
                                recordBuf = recordBuf + delimiter
                            else:
                                raise KnownIssue(convert_error_message(foundLayout.layoutType, field, record[fieldStart:fieldEnd], ex))
                        except:
                            if(IGNORECONVERSIONERRORS):
                                print(convert_error_message(foundLayout.layoutType, field, record[fieldStart:fieldEnd], str(sys.exc_info()[0]) + " " + str(sys.exc_info()[1])))
                                recordBuf = recordBuf + delimiter
                            else:
                                raise KnownIssue(convert_error_message(foundLayout.layoutType, field, record[fieldStart:fieldEnd], str(sys.exc_info()[0]) + " " + str(sys.exc_info()[1])))
                            
                currentRecordPosition += fieldSize
                        
                lastField = field
                delimiter = DELIMITER
            
            # adding header fields
            for headerField in layoutDefinition.keyFields:
                if headerField.type!=SKIPFIELDTYPENAME:
                    if headerField.keytype==None:
                        recordBuf = recordBuf + delimiter + str(headerField.convertedValue)
            
            # adding FileTag
            recordBuf = recordBuf + delimiter + fileName
            
            # adding special keys to link records together
            if layoutDefinition.isMultipleLayouts and GROUPRECORDS:
                recordBuf = recordBuf + delimiter + str(relatedGroup)
            
            # adding special keys to link level 2 records together    
            if layoutDefinition.isMultipleLayouts and GROUPRECORDSLEVEL2:
                if(str(layoutDefinition.layouts[0].layoutType).strip()!=str(foundLayout.layoutType).strip()):
                    recordBuf = recordBuf + delimiter + str(relatedGroupLevel2)
                    
            if(foundLayout.isVariableFields):
                # link static and variable records    
                unqKey = uuid.uuid4().hex
                recordBuf = recordBuf + delimiter + str(unqKey)
            
            recordBuf = recordBuf + NEWLINE
            f_out.write(recordBuf)

            if(foundLayout.isVariableFields):
                outputVariableFileName = fileName + "_type_" + str(foundLayout.layoutType) + "_variable"
                outputVariablePath = path.join(outputFolder, outputVariableFileName + OUTPUTFILEEXTENSION)

                
                if(not outputVariableFileName in outputOpenFiles):
                    outputOpenFiles[outputVariableFileName] = codecs.open(outputVariablePath,"w", encoding=OUTPUTENCODING)
                
                f_outVariable = outputOpenFiles[outputVariableFileName]
                
                #find number of variable records which sitting in the last field of the current record
                fieldName = field.name
                fieldType = field.type
                fieldSize = field.size
                fieldStart = field.start
                fieldEnd= fieldStart + fieldSize
                convert = convertFunction(fieldType)        
                numberDymanicRecords = convert(record[fieldStart:fieldEnd], field, PYTHONENCODING, INPUTENCODING)

                for i in range(0, int(numberDymanicRecords)):
                            
                    delimiter=""
                    recordVariableBuf = ""
                    
                    for field in foundLayout.variableFields:
                        fieldName = field.name
                        fieldType = field.type
                        fieldSize = field.size
                        fieldStart = currentRecordPosition + field.start
                        fieldEnd= fieldStart + fieldSize
                        
                        # don't extract field value if field type is "skip"
                        if(fieldType==SKIPFIELDTYPENAME):
                            recordVariableBuf = recordVariableBuf + delimiter
                        else:
                            convert = convertFunction(fieldType)
                            if(convert == None):
                                raise KnownIssue("Error in field\t" + "Layout type: " + foundLayout.layoutType + "\t" + "Field name: " + fieldName + "\t" + "Field type - " + str(fieldType))
                            else:
                                try: 
                                    recordVariableBuf = recordVariableBuf + delimiter + str(convert(record[fieldStart:fieldEnd], field, PYTHONENCODING, INPUTENCODING))
                                except UnicodeEncodeError as ex:
                                    if(IGNORECONVERSIONERRORS):
                                        print(convert_error_message(foundLayout.layoutType, field, record[fieldStart:fieldEnd], ex))
                                        recordVariableBuf = recordVariableBuf + delimiter
                                    else:
                                        raise KnownIssue(convert_error_message(foundLayout.layoutType, field, record[fieldStart:fieldEnd], ex))
                                except:
                                    if(IGNORECONVERSIONERRORS):
                                        print(convert_error_message(foundLayout.layoutType, field, record[fieldStart:fieldEnd], sys.exc_info()[0]))
                                        recordVariableBuf = recordVariableBuf + delimiter
                                    else:
                                        raise KnownIssue(convert_error_message(foundLayout.layoutType, field, record[fieldStart:fieldEnd], sys.exc_info()[0]))
                                    
                        delimiter = DELIMITER

                    currentRecordPosition += foundLayout.variableLayoutSize
                    
                    # adding FileTag
                    recordVariableBuf = recordVariableBuf + delimiter + fileName
            
                    # adding special keys to link records together
                    if GROUPRECORDS:
                        recordVariableBuf = recordVariableBuf + delimiter + str(relatedGroup)
                    
                    # adding special keys to link level2 records together    
                    if GROUPRECORDSLEVEL2:
                        if(str(layoutDefinition.layouts[0].layoutType).strip()!=str(foundLayout.layoutTyp).strip()):
                            recordVariableBuf = recordVariableBuf + delimiter + str(relatedGroupLevel2)
                    
                    # adding parent-child relationship between static and dynamic records
                    recordVariableBuf = recordVariableBuf + delimiter + str(unqKey)
                    
                    recordVariableBuf = recordVariableBuf + NEWLINE
                    f_outVariable.write(recordVariableBuf)

                    layoutTypeName = "Processed variable type "
                                
                    keyRecordCount = layoutTypeName + str(foundLayout.layoutType)
                    if(keyRecordCount in recordCount):
                        recordCount[keyRecordCount] += 1
                    else:
                        recordCount[keyRecordCount] = 1
            
            #stop processing if bottom part of file skipped
            if fileSize - (layoutDefinition.footer.size if layoutDefinition.isFooter else 0) <= NumberOfRecordsRead:
                break
            
            if(layoutDefinition.isMultipleLayouts):
                for keyField in layoutDefinition.keyFields:
                    recordSizeBytes = f_in.read(keyField.size)
                    NumberOfRecordsRead += keyField.size
                    if(recordSizeBytes):
                        keyField.rawValue = recordSizeBytes
                    else:
                        break
            else:
                layoutFields = layoutDefinition.layouts[0];
                layoutSize = layoutFields.layoutSize
                recordSizeBytes = f_in.read(layoutSize)
                NumberOfRecordsRead += layoutSize
            
    print("Processed data:", fileName + fileExtension)            
    keys = sorted(recordCount.keys())
    for key in keys:
        print(str(key) + ": " +  str(recordCount[key]))
    
    # successful exit is 0
    returnCode = 0
     
except UnicodeError as errMessage:
    print(errMessage)
    print("Number of bytes read:", NumberOfRecordsRead)
    
except KnownIssue as descr:
    print(descr)
    print("Number of bytes read:", NumberOfRecordsRead)
        
except SystemExit:
    #just used to catch system exit exception initiated by sys.exit()
    pass
    
except Exception as err:
    error_class = err.__class__.__name__
    detail = err.args[0]
    cl, exc, tb = sys.exc_info()
    line_number = traceback.extract_tb(tb)[-1][1]
    print("Unexpected error: %s at line %d: %s"% (error_class, line_number, detail))
    if not str(sys.exc_info()[1])==str(detail):
        print(sys.exc_info()[1])
    print("Number of bytes read:", NumberOfRecordsRead)
    
finally:
    for key in outputOpenFiles.keys():
        outputOpenFiles[key].close()
    
    if(not wrongArgumentsFlag):
        print("Completed:", datetime.datetime.now())
        
    if(not PYTHONENCODING):
        if JAVABRIDGEINCLUDED:
            javabridge.kill_vm()
            
    sys.exit(returnCode)
