import os
import os.path as path
import subprocess 

import unittest

import test_system_common as common

APPARGUMENT_INPUTFILE_SAMPLE_VALUE = r'test_data\311_calls_for_service_requests_all_strings\311_calls_for_service_requests_sample.dat'
APPARGUMENT_INPUTFILE_FULL_VALUE = r'test_data\311_calls_for_service_requests_all_strings\311_calls_for_service_requests.dat'
APPARGUMENT_OUTPUTFOLDER_VALUE =r'test_data\311_calls_for_service_requests_all_strings\output'
APPARGUMENT_LAYPOUTFILE_VALUE =r'layout_repository\311_calls_for_service_requests_all_strings.json'
APPARGUMENT_LOGFOLDER_VALUE =r'test_data\311_calls_for_service_requests_all_strings\log'

VALIDATION_SAMPLE_FOLDER_VALUE = r'test_data\311_calls_for_service_requests_all_strings\validate'
PERFORMANCE_LOGFILE_VALUE =r'test_data\311_calls_for_service_requests_all_strings\performance_log\ebcdic_converter_performance.log'

DATA_SEGMENTS = ["main"]

class TestEbcdicConverter(unittest.TestCase):

    @classmethod
    def setUpClass(self):
        print("\n\nPreparing data... 311 calls for service requests all strings")
        
        codePath = path.dirname(__file__)
        
        self.testEnvironment = common.Setup.getSetupEnvironmentDesctiption(codePath)
        self.testType = common.Setup.getSetupTestType(codePath)
         
        inputFile = APPARGUMENT_INPUTFILE_SAMPLE_VALUE
        
        if self.testType==common.TestType.Performance:
            inputFile = APPARGUMENT_INPUTFILE_FULL_VALUE
        
        self.appArguments = common.AppArguments(codePath)
        self.appArguments.addArgumentValue(self.appArguments.APPARGUMENT_INPUTFILE, inputFile)
        self.appArguments.addArgumentValue(self.appArguments.APPARGUMENT_OUTPUTFOLDER, APPARGUMENT_OUTPUTFOLDER_VALUE)
        self.appArguments.addArgumentValue(self.appArguments.APPARGUMENT_LAYPOUTFILE, APPARGUMENT_LAYPOUTFILE_VALUE)
        self.appArguments.addArgumentValue(self.appArguments.APPARGUMENT_LOGFOLDER, APPARGUMENT_LOGFOLDER_VALUE)
        
        self.appArguments.addArgumentValue(self.appArguments.VALIDATION_SAMPLE_FOLDER, VALIDATION_SAMPLE_FOLDER_VALUE)
        self.appArguments.addArgumentValue(self.appArguments.PERFORMANCE_LOGFILE, PERFORMANCE_LOGFILE_VALUE)
        
        #create folder structure
        folders = [self.appArguments.APPARGUMENT_OUTPUTFOLDER, self.appArguments.APPARGUMENT_LOGFOLDER, self.appArguments.VALIDATION_SAMPLE_FOLDER, self.appArguments.PERFORMANCE_LOGFILE]
        for folderKey in folders:
            folderPath = self.appArguments.getArgumentValue(folderKey)
            if folderKey==self.appArguments.PERFORMANCE_LOGFILE:
                folderPath = path.dirname(folderPath)
            if not path.exists(folderPath):
                os.mkdir(folderPath)
         
        #delete any already existing output converted files
        for segmentName in DATA_SEGMENTS:
            filePath = self.appArguments.getOutputFilePath(segmentName)
            if path.exists(filePath):
                os.remove(filePath)
        
        self.runStats = common.RunStats()
        self.runStats.storeCompletedProcess(subprocess.run(args=["python.exe", self.appArguments.getAppArgument(), 
                                                                         self.appArguments.getArgument(self.appArguments.APPARGUMENT_INPUTFILE),
                                                                         self.appArguments.getArgument(self.appArguments.APPARGUMENT_OUTPUTFOLDER),
                                                                         self.appArguments.getArgument(self.appArguments.APPARGUMENT_LAYPOUTFILE),
                                                                         self.appArguments.getArgument(self.appArguments.APPARGUMENT_LOGFOLDER)
                                                                         ],
                                                                   stdout=subprocess.PIPE,
                                                                   stderr=subprocess.STDOUT))
        
        print(self.runStats.getOutput())
        if self.testType==common.TestType.Performance:
            print()
            print("-----------No tests completed. Just reporting performance-----------")

    def tearDown(self):
        # release resources
        pass
    
    @classmethod
    def tearDownClass(self):
        #store performance info
        #write in performance log if no run-time error
        self.runStats.savePerformanceLog(self.appArguments, self.testEnvironment)
    
    @unittest.skipIf(common.Setup.getSetupTestType(path.dirname(__file__))==common.TestType.Performance, "Don't run to get performance")
    def test_01_ReturnCode(self):
        self.assertEqual(self.runStats.getReturnCode(), 0, "Converter returns other than 0 return code")
    
    @unittest.skipIf(common.Setup.getSetupTestType(path.dirname(__file__))==common.TestType.Performance, "Don't run to get performance")
    def test_02_NumberRecords(self):
        segmentRecordCount = {"main":1000}
        for segmentName in DATA_SEGMENTS:
            self.assertEqual(self.runStats.getNumberRecords(segmentName), segmentRecordCount[segmentName], "Wrong number of converted records")
    
    @unittest.skipIf(common.Setup.getSetupTestType(path.dirname(__file__))==common.TestType.Performance, "Don't run to get performance")  
    def test_03_CompareContent(self):
        for segmentName in DATA_SEGMENTS:
            lineNumber = 0
            with open(self.appArguments.getOutputFilePath(segmentName), 'r', encoding="utf-8") as convertedFile:
                with open(self.appArguments.getValidationSampleFilePath(segmentName), 'r', encoding="utf-8") as validationSampleFile:
                    for convertedLine in convertedFile:
                        validationLine = validationSampleFile.readline()
                        lineNumber += 1
                        self.assertEqual(convertedLine, validationLine, "Content doesn't match in line: " + str(lineNumber))
    
if __name__=="__main__":
    unittest.main()
