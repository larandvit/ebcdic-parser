JAVABRIDGEINCLUDED = True

import codecs
import io

import ebcdic_parser as parser

if JAVABRIDGEINCLUDED:
    import javabridge
    import numpy
    
INPUTENCODING = "utf_8"
OUTPUTENCODING = "cp037"

DELIMITER_INPUT = "\t"

class DataConverter():
    def __init__(self, pythonEncoding, encodingName):
        self.pythonEncoding = pythonEncoding
        self.encodingName = encodingName
        
        if(not self.pythonEncoding):
            if JAVABRIDGEINCLUDED:
                javabridge.start_vm(run_headless=True)
    
    def packedDecimal(self, fieldBytes, fieldDefinition):
        pass
    
    def integer(self, fieldBytes, fieldDefinition):
        pass
    
    def uinteger(self, fieldBytes, fieldDefinition):
        pass
    
    def string(self, text, fieldDefinition):
        buf2 = ""

        ibmDecoder = codecs.getencoder(self.encodingName)
        buf = ibmDecoder(text)
        buf2 = buf[0]
        
        decodedValue = bytearray(buf2)
        
        emptyFiller = 64
        
        replicateFiller = fieldDefinition.size - len(decodedValue)
        
        for i in range(replicateFiller):
            decodedValue.append(emptyFiller)
        
        return decodedValue
    
    def decimal(self, fieldBytes, fieldDefinition):
        pass
    
    def convert(self, fieldBytes, fieldDefinition):
        
        fieldType = fieldDefinition.type
        convert=None
            
        if(fieldType=="packedDecimal"):
            convert=self.packedDecimal
        elif(fieldType=="integer"):
            convert=self.integer
        elif(fieldType=="short"):
            convert=self.integer
        elif(fieldType=="string"):
            convert=self.string
        elif(fieldType=="decimal"):
            convert=self.decimal
        elif(fieldType=="zonedDecimal"):
            convert=self.decimal

        return convert(fieldBytes, fieldDefinition)
        
    def release(self):
        if(not self.pythonEncoding):
            if JAVABRIDGEINCLUDED:
                javabridge.kill_vm()
                
if __name__=="__main__":
    
    layoutPath =r"C:\Temp\opensource\ebcdic-parser\layout_repository\311_calls_for_service_requests_all_strings.json"
    
    layoutDefinition = parser.LayoutDefinition()
    if not layoutDefinition.loadLayouts(layoutPath):
        raise parser.KnownIssue(layoutDefinition.errorDescription)
        
    layoutFields = layoutDefinition.layouts[0]
    
    inputFilePath = r"C:\Temp\opensource\Samples\311_calls_for_service_requests\311_calls_for_service_requests.txt"
    outputFilePath = r"C:\Temp\opensource\Samples\311_calls_for_service_requests\311_calls_for_service_requests.dat"
    
    PYTHONENCODING = True
    
    dataConverter = DataConverter(PYTHONENCODING, OUTPUTENCODING)
    
    with codecs.open(inputFilePath, "r", encoding=INPUTENCODING) as fIn:
        with open(outputFilePath, "wb") as fOut:
            lineNbr = 1
            line = fIn.readline()
            for line in fIn:
                values = line.split(DELIMITER_INPUT)
                fieldIdx = 0
                for field in layoutFields.fields:
                    convertedBytes = dataConverter.string(values[fieldIdx].rstrip("\r\n").rstrip("\r").rstrip("\n"), field)
                    fOut.write(convertedBytes)
                    fieldIdx += 1 
                lineNbr += 1
                    
    dataConverter.release()  
            
    