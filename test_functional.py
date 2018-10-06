import unittest

import ebcdic_parser as parser

class TestFunctional(unittest.TestCase):

    @classmethod
    def setUpClass(self):
        pass
    
    @classmethod
    def tearDownClass(self):
        pass
    
    def test_01_integerSigned(self):
        layoutField = parser.LayoutField()
        pythonEncoding = True
        encodingName = parser.INPUTENCODING
        controlCharRegex = parser.buildControlCharRegex()
        
        # 1 byte
        # max value
        testBytes = bytes(b"\x7f")
        testResult = 127
        self.assertEqual(parser.integer(testBytes, layoutField, pythonEncoding, encodingName, controlCharRegex), testResult, "Not passed: {}".format(testResult))
        # min value
        testBytes = bytes(b"\x80")
        testResult = -128
        self.assertEqual(parser.integer(testBytes, layoutField, pythonEncoding, encodingName, controlCharRegex), testResult, "Not passed: {}".format(testResult))
        # 59 value
        testBytes = bytes(b"\x3b")
        testResult = 59
        self.assertEqual(parser.integer(testBytes, layoutField, pythonEncoding, encodingName, controlCharRegex), testResult, "Not passed: {}".format(testResult))
        # -59 value
        testBytes = bytes(b"\xc5")
        testResult = -59
        self.assertEqual(parser.integer(testBytes, layoutField, pythonEncoding, encodingName, controlCharRegex), testResult, "Not passed: {}".format(testResult))
        
        # 2 bytes
        # max value
        testBytes = bytes(b"\x7f\xff")
        testResult = 32767
        self.assertEqual(parser.integer(testBytes, layoutField, pythonEncoding, encodingName, controlCharRegex), testResult, "Not passed: {}".format(testResult))
        # min value
        testBytes = bytes(b"\x80\x00")
        testResult = -32768
        self.assertEqual(parser.integer(testBytes, layoutField, pythonEncoding, encodingName, controlCharRegex), testResult, "Not passed: {}".format(testResult))
        # 27899 value
        testBytes = bytes(b"\x6c\xfb")
        testResult = 27899
        self.assertEqual(parser.integer(testBytes, layoutField, pythonEncoding, encodingName, controlCharRegex), testResult, "Not passed: {}".format(testResult))
        # -27899 value
        testBytes = bytes(b"\x93\x05")
        testResult = -27899
        self.assertEqual(parser.integer(testBytes, layoutField, pythonEncoding, encodingName, controlCharRegex), testResult, "Not passed: {}".format(testResult))
        
        # 4 bytes
        # max value
        testBytes = bytes(b"\x7f\xff\xff\xff")
        testResult = 2147483647
        self.assertEqual(parser.integer(testBytes, layoutField, pythonEncoding, encodingName, controlCharRegex), testResult, "Not passed: {}".format(testResult))
        # min value
        testBytes = bytes(b"\x80\x00\x00\x00")
        testResult = -2147483648
        self.assertEqual(parser.integer(testBytes, layoutField, pythonEncoding, encodingName, controlCharRegex), testResult, "Not passed: {}".format(testResult))
        # 35791 value
        testBytes = bytes(b"\x00\x00\x8b\xcf")
        testResult = 35791
        self.assertEqual(parser.integer(testBytes, layoutField, pythonEncoding, encodingName, controlCharRegex), testResult, "Not passed: {}".format(testResult))
        # -35791 value
        testBytes = bytes(b"\xff\xff\x74\x31")
        testResult = -35791
        self.assertEqual(parser.integer(testBytes, layoutField, pythonEncoding, encodingName, controlCharRegex), testResult, "Not passed: {}".format(testResult))
        
        # 8 bytes
        # max value
        testBytes = bytes(b"\x7f\xff\xff\xff\xff\xff\xff\xff")
        testResult = 9223372036854775807
        self.assertEqual(parser.integer(testBytes, layoutField, pythonEncoding, encodingName, controlCharRegex), testResult, "Not passed: {}".format(testResult))
        # min value
        testBytes = bytes(b"\x80\x00\x00\x00\x00\x00\x00\x00")
        testResult = -9223372036854775808
        self.assertEqual(parser.integer(testBytes, layoutField, pythonEncoding, encodingName, controlCharRegex), testResult, "Not passed: {}".format(testResult))
        # 5482450658163456 value
        testBytes = bytes(b"\x00\x13\x7a\x42\x83\xfa\x7b\x00")
        testResult = 5482450658163456
        self.assertEqual(parser.integer(testBytes, layoutField, pythonEncoding, encodingName, controlCharRegex), testResult, "Not passed: {}".format(testResult))
        # -5482450658163456 value
        testBytes = bytes(b"\xff\xec\x85\xbd\x7c\x05\x85\x00")
        testResult = -5482450658163456
        self.assertEqual(parser.integer(testBytes, layoutField, pythonEncoding, encodingName, controlCharRegex), testResult, "Not passed: {}".format(testResult))
    
    def test_02_integerUnsigned(self):
        layoutField = parser.LayoutField()
        pythonEncoding = True
        encodingName = parser.INPUTENCODING
        controlCharRegex = parser.buildControlCharRegex()
        
        # 1 byte
        # max value
        testBytes = bytes(b"\xff")
        testResult = 255
        self.assertEqual(parser.uinteger(testBytes, layoutField, pythonEncoding, encodingName, controlCharRegex), testResult, "Not passed: {}".format(testResult))
        # min value
        testBytes = bytes(b"\x00")
        testResult = 0
        self.assertEqual(parser.uinteger(testBytes, layoutField, pythonEncoding, encodingName, controlCharRegex), testResult, "Not passed: {}".format(testResult))
        # 59 value
        testBytes = bytes(b"\x3b")
        testResult = 59
        self.assertEqual(parser.uinteger(testBytes, layoutField, pythonEncoding, encodingName, controlCharRegex), testResult, "Not passed: {}".format(testResult))
        
        # 2 bytes
        # max value
        testBytes = bytes(b"\xff\xff")
        testResult = 65535
        self.assertEqual(parser.uinteger(testBytes, layoutField, pythonEncoding, encodingName, controlCharRegex), testResult, "Not passed: {}".format(testResult))
        # min value
        testBytes = bytes(b"\x00\x00")
        testResult = 0
        self.assertEqual(parser.uinteger(testBytes, layoutField, pythonEncoding, encodingName, controlCharRegex), testResult, "Not passed: {}".format(testResult))
        # 27899 value
        testBytes = bytes(b"\x6c\xfb")
        testResult = 27899
        self.assertEqual(parser.uinteger(testBytes, layoutField, pythonEncoding, encodingName, controlCharRegex), testResult, "Not passed: {}".format(testResult))
        
        # 4 bytes
        # max value
        testBytes = bytes(b"\xff\xff\xff\xff")
        testResult = 4294967295
        self.assertEqual(parser.uinteger(testBytes, layoutField, pythonEncoding, encodingName, controlCharRegex), testResult, "Not passed: {}".format(testResult))
        # min value
        testBytes = bytes(b"\x00\x00\x00\x00")
        testResult = 0
        self.assertEqual(parser.uinteger(testBytes, layoutField, pythonEncoding, encodingName, controlCharRegex), testResult, "Not passed: {}".format(testResult))
        # 35791 value
        testBytes = bytes(b"\x00\x00\x8b\xcf")
        testResult = 35791
        self.assertEqual(parser.uinteger(testBytes, layoutField, pythonEncoding, encodingName, controlCharRegex), testResult, "Not passed: {}".format(testResult))
        
        # 8 bytes
        # max value
        testBytes = bytes(b"\xff\xff\xff\xff\xff\xff\xff\xff")
        testResult = 18446744073709551615
        self.assertEqual(parser.uinteger(testBytes, layoutField, pythonEncoding, encodingName, controlCharRegex), testResult, "Not passed: {}".format(testResult))
        # min value
        testBytes = bytes(b"\x00\x00\x00\x00\x00\x00\x00\x00")
        testResult = 0
        self.assertEqual(parser.uinteger(testBytes, layoutField, pythonEncoding, encodingName, controlCharRegex), testResult, "Not passed: {}".format(testResult))
        # 5482450658163456 value
        testBytes = bytes(b"\x00\x13\x7a\x42\x83\xfa\x7b\x00")
        testResult = 5482450658163456
        self.assertEqual(parser.uinteger(testBytes, layoutField, pythonEncoding, encodingName, controlCharRegex), testResult, "Not passed: {}".format(testResult))
    
    def test_03_character_cp037(self):
        layoutField = parser.LayoutField()
        pythonEncoding = True
        controlCharRegex = parser.buildControlCharRegex()
        
        encodingName = "cp037"
        
        # text
        testBytes = bytes(b"\xc8\x85\x93\x93\x96\x40\x85\xa5\x85\x99\xa8\x82\x96\x84\xa8\x5a")
        testResult = "Hello everybody!"
        self.assertEqual(parser.string(testBytes, layoutField, pythonEncoding, encodingName, controlCharRegex), testResult, "Not passed: {}".format(testResult))
        
        # text with control characters
        testBytes = bytes(b"\x00\xf5\xf9\x81\x16\x15\x98")
        testResult = "59aq"
        self.assertEqual(parser.string(testBytes, layoutField, pythonEncoding, encodingName, controlCharRegex), testResult, "Not passed: {}".format(testResult))
        
        # signs
        testBytes = bytes(b"\x7b\x5b\x50\x5d\x4e\x61")
        testResult = r"#$&)+/"
        self.assertEqual(parser.string(testBytes, layoutField, pythonEncoding, encodingName, controlCharRegex), testResult, "Not passed: {}".format(testResult))
        
        # LATIN
        testBytes = bytes(b"\x64\x73\x48\xdf\xcc\x9c")
        testResult = "ÀËçÿöæ"
        self.assertEqual(parser.string(testBytes, layoutField, pythonEncoding, encodingName, controlCharRegex), testResult, "Not passed: {}".format(testResult))
            
if __name__=="__main__":
    unittest.main()