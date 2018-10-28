import os.path as path
import sys

import re
import json

from datetime import datetime
from enum import IntEnum

class AppArguments():
    
    APP_NAME = r'ebcdic_parser.py'
    APPARGUMENT_INPUTFILE = r'--inputfile'
    APPARGUMENT_OUTPUTFOLDER =r'--outputfolder'
    APPARGUMENT_LAYPOUTFILE =r'--layoutfile'
    APPARGUMENT_LOGFOLDER =r'--logfolder'
    APPARGUMENT_PYTHONCODING =r'--pythonencoding'
    APPARGUMENT_ENCODINGNAME =r'--encodingname'
    APPARGUMENT_GROUPRECORDS = r'--grouprecords'
    APPARGUMENT_GROUPRECORDSLEVEL2 = r'--grouprecordslevel2'

    VALIDATION_SAMPLE_FOLDER = r'validationsamplefolder'
    PERFORMANCE_LOGFILE = r'performancelog'
    
    def __init__(self, rootFolder=None):
        self.args = {}
        self.rootFolder = rootFolder
    
    def getAppArgument(self, appFolder=None):
        buf = appFolder
        
        if appFolder==None:
            buf = self.rootFolder
        
        return path.join(buf, self.APP_NAME)
    
    def addArgumentValue(self, argumentKey, argumentValue):
        self.args[argumentKey] = argumentValue
    
    def getArgumentValue(self, argumentKey):
        buf=self.args[argumentKey]
        
        if argumentKey in [self.APPARGUMENT_INPUTFILE, 
                           self.APPARGUMENT_OUTPUTFOLDER, 
                           self.APPARGUMENT_LAYPOUTFILE, 
                           self.APPARGUMENT_LOGFOLDER,
                           self.VALIDATION_SAMPLE_FOLDER,
                           self.PERFORMANCE_LOGFILE]:
            if self.rootFolder!=None:
                buf = path.join(self.rootFolder, buf)
        
        return buf
    
    def getArgument(self, argumentKey):     
        buf = argumentKey + "=" + self.getArgumentValue(argumentKey);
        return buf
    
    def getInputFileName(self):
        return path.basename(self.getArgumentValue(self.APPARGUMENT_INPUTFILE))
    
    def getOutputFileName(self, segmentName):
        return path.splitext(path.basename(self.getArgumentValue(self.APPARGUMENT_INPUTFILE)))[0] + "_type_" + segmentName + ".txt"
    
    def getOutputFilePath(self, segmentName):
        return path.join(self.getArgumentValue(self.APPARGUMENT_OUTPUTFOLDER), self.getOutputFileName(segmentName))
    
    def getValidationSampleFilePath(self, segmentName):
        return path.join(self.getArgumentValue(self.VALIDATION_SAMPLE_FOLDER), self.getOutputFileName(segmentName))

class RunStats():
    
    def storeCompletedProcess(self, completedProcess):
        self.runStats = completedProcess
        
    def getReturnCode(self):
        return self.runStats.returncode
    
    def getOutput(self):
        return self.runStats.stdout.decode("utf-8").replace("\r","")
    
    def getNumberRecords(self, segmentName):
        numberRecoreds = 0
        
        found = re.search('^Processed type ' + segmentName + ':\s*(\d+)$', self.getOutput(), re.RegexFlag.MULTILINE)
        if found :
            numberRecoreds = int(found.group(1))
        
        return numberRecoreds
    
    def getTestDuration(self):
        testDuration = None
        startTime = None
        endTime = None
        
        found = re.search('^Stated:\s*(\d{4}-\d{2}-\d{2} \d{2}:\d{2}:\d{2})\.\d{6}', self.getOutput(), re.RegexFlag.MULTILINE)
        if found :
            startTime = datetime.strptime(found.group(1), '%Y-%m-%d %H:%M:%S')
        
        found = re.search('^Completed:\s*(\d{4}-\d{2}-\d{2} \d{2}:\d{2}:\d{2})\.\d{6}', self.getOutput(), re.RegexFlag.MULTILINE)
        if found :
            endTime = datetime.strptime(found.group(1), '%Y-%m-%d %H:%M:%S')
        
        if startTime!=None and endTime!=None:
            elapsedTime = endTime - startTime
            minutes, seconds = divmod(elapsedTime.total_seconds(), 60)
            hours, minutes = divmod(minutes, 60)
            testDuration = (int(hours), int(minutes), int(seconds))
        
        return testDuration
    
    def savePerformanceLog(self, appArguments, testEnvironment):
        if self.getTestDuration()!=(0,0,0):
            with open(appArguments.getArgumentValue(appArguments.PERFORMANCE_LOGFILE), 'a', encoding="utf-8") as performanceLog:
                performanceLog.write(datetime.now().strftime('%Y-%m-%d %H:%M:%S'))
                performanceLog.write("\t" + testEnvironment)
                performanceLog.write("\t" + appArguments.getInputFileName())
                performanceLog.write("\t" + str(path.getsize(appArguments.getArgumentValue(appArguments.APPARGUMENT_INPUTFILE))))
                performanceLog.write("\t" + str(self.getTestDuration()[0]) + ":" + str(self.getTestDuration()[1]) + ":" + str(self.getTestDuration()[2]))
                performanceLog.write("\n")

class TestType(IntEnum):
    AllTestsSample = 1
    Performance = 2
      
class Setup:
    #check setup.json file. There are two options 
    #"testtype":"full" or "performance"
    #"environmentdesctiption":"" 
    @staticmethod
    def getSetup(setupFileFolder):
        
        setupParameters = {"testtype":1, "environmentdesctiption":"Unknown"}
        errMessage=None
        
        try:
            setupFilePath = path.join(setupFileFolder, "setup.json")
            
            with open(setupFilePath,'r') as testSetup:
                setupParameters=json.load(testSetup)
        except (ValueError) as errValue:
            errMessage = str(errValue)
        except:
            errMessage=str(sys.exc_info()[0])
         
        return (setupParameters, errMessage)
    
    @staticmethod
    def getSetupTestType(setupFileFolder):
        return  Setup.getSetup(setupFileFolder)[0]["testtype"]
    
    @staticmethod
    def getSetupEnvironmentDesctiption(setupFileFolder):
        return  Setup.getSetup(setupFileFolder)[0]["environmentdesctiption"]
