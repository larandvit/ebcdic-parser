import unittest

import ebcdic_parser as parser

JAVABRIDGEINCLUDED = True

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
        
        dataConverter = parser.DataConverter(pythonEncoding, encodingName)
                
        # 1 byte
        # max value
        testBytes = bytes(b"\x7f")
        testResult = 127
        self.assertEqual(dataConverter.integer(testBytes, layoutField), testResult, "Not passed: {}".format(testResult))
        # min value
        testBytes = bytes(b"\x80")
        testResult = -128
        self.assertEqual(dataConverter.integer(testBytes, layoutField), testResult, "Not passed: {}".format(testResult))
        # 59 value
        testBytes = bytes(b"\x3b")
        testResult = 59
        self.assertEqual(dataConverter.integer(testBytes, layoutField), testResult, "Not passed: {}".format(testResult))
        # -59 value
        testBytes = bytes(b"\xc5")
        testResult = -59
        self.assertEqual(dataConverter.integer(testBytes, layoutField), testResult, "Not passed: {}".format(testResult))
        
        # 2 bytes
        # max value
        testBytes = bytes(b"\x7f\xff")
        testResult = 32767
        self.assertEqual(dataConverter.integer(testBytes, layoutField), testResult, "Not passed: {}".format(testResult))
        # min value
        testBytes = bytes(b"\x80\x00")
        testResult = -32768
        self.assertEqual(dataConverter.integer(testBytes, layoutField), testResult, "Not passed: {}".format(testResult))
        # 27899 value
        testBytes = bytes(b"\x6c\xfb")
        testResult = 27899
        self.assertEqual(dataConverter.integer(testBytes, layoutField), testResult, "Not passed: {}".format(testResult))
        # -27899 value
        testBytes = bytes(b"\x93\x05")
        testResult = -27899
        self.assertEqual(dataConverter.integer(testBytes, layoutField), testResult, "Not passed: {}".format(testResult))
        
        # 4 bytes
        # max value
        testBytes = bytes(b"\x7f\xff\xff\xff")
        testResult = 2147483647
        self.assertEqual(dataConverter.integer(testBytes, layoutField), testResult, "Not passed: {}".format(testResult))
        # min value
        testBytes = bytes(b"\x80\x00\x00\x00")
        testResult = -2147483648
        self.assertEqual(dataConverter.integer(testBytes, layoutField), testResult, "Not passed: {}".format(testResult))
        # 35791 value
        testBytes = bytes(b"\x00\x00\x8b\xcf")
        testResult = 35791
        self.assertEqual(dataConverter.integer(testBytes, layoutField), testResult, "Not passed: {}".format(testResult))
        # -35791 value
        testBytes = bytes(b"\xff\xff\x74\x31")
        testResult = -35791
        self.assertEqual(dataConverter.integer(testBytes, layoutField), testResult, "Not passed: {}".format(testResult))
        
        # 8 bytes
        # max value
        testBytes = bytes(b"\x7f\xff\xff\xff\xff\xff\xff\xff")
        testResult = 9223372036854775807
        self.assertEqual(dataConverter.integer(testBytes, layoutField), testResult, "Not passed: {}".format(testResult))
        # min value
        testBytes = bytes(b"\x80\x00\x00\x00\x00\x00\x00\x00")
        testResult = -9223372036854775808
        self.assertEqual(dataConverter.integer(testBytes, layoutField), testResult, "Not passed: {}".format(testResult))
        # 5482450658163456 value
        testBytes = bytes(b"\x00\x13\x7a\x42\x83\xfa\x7b\x00")
        testResult = 5482450658163456
        self.assertEqual(dataConverter.integer(testBytes, layoutField), testResult, "Not passed: {}".format(testResult))
        # -5482450658163456 value
        testBytes = bytes(b"\xff\xec\x85\xbd\x7c\x05\x85\x00")
        testResult = -5482450658163456
        self.assertEqual(dataConverter.integer(testBytes, layoutField), testResult, "Not passed: {}".format(testResult))
        
        dataConverter.release()
    
    def test_02_integerUnsigned(self):
        layoutField = parser.LayoutField()
        pythonEncoding = True
        encodingName = parser.INPUTENCODING
        
        dataConverter = parser.DataConverter(pythonEncoding, encodingName)
        
        # 1 byte
        # max value
        testBytes = bytes(b"\xff")
        testResult = 255
        self.assertEqual(dataConverter.uinteger(testBytes, layoutField), testResult, "Not passed: {}".format(testResult))
        # min value
        testBytes = bytes(b"\x00")
        testResult = 0
        self.assertEqual(dataConverter.uinteger(testBytes, layoutField), testResult, "Not passed: {}".format(testResult))
        # 59 value
        testBytes = bytes(b"\x3b")
        testResult = 59
        self.assertEqual(dataConverter.uinteger(testBytes, layoutField), testResult, "Not passed: {}".format(testResult))
        
        # 2 bytes
        # max value
        testBytes = bytes(b"\xff\xff")
        testResult = 65535
        self.assertEqual(dataConverter.uinteger(testBytes, layoutField), testResult, "Not passed: {}".format(testResult))
        # min value
        testBytes = bytes(b"\x00\x00")
        testResult = 0
        self.assertEqual(dataConverter.uinteger(testBytes, layoutField), testResult, "Not passed: {}".format(testResult))
        # 27899 value
        testBytes = bytes(b"\x6c\xfb")
        testResult = 27899
        self.assertEqual(dataConverter.uinteger(testBytes, layoutField), testResult, "Not passed: {}".format(testResult))
        
        # 4 bytes
        # max value
        testBytes = bytes(b"\xff\xff\xff\xff")
        testResult = 4294967295
        self.assertEqual(dataConverter.uinteger(testBytes, layoutField), testResult, "Not passed: {}".format(testResult))
        # min value
        testBytes = bytes(b"\x00\x00\x00\x00")
        testResult = 0
        self.assertEqual(dataConverter.uinteger(testBytes, layoutField), testResult, "Not passed: {}".format(testResult))
        # 35791 value
        testBytes = bytes(b"\x00\x00\x8b\xcf")
        testResult = 35791
        self.assertEqual(dataConverter.uinteger(testBytes, layoutField), testResult, "Not passed: {}".format(testResult))
        
        # 8 bytes
        # max value
        testBytes = bytes(b"\xff\xff\xff\xff\xff\xff\xff\xff")
        testResult = 18446744073709551615
        self.assertEqual(dataConverter.uinteger(testBytes, layoutField), testResult, "Not passed: {}".format(testResult))
        # min value
        testBytes = bytes(b"\x00\x00\x00\x00\x00\x00\x00\x00")
        testResult = 0
        self.assertEqual(dataConverter.uinteger(testBytes, layoutField), testResult, "Not passed: {}".format(testResult))
        # 5482450658163456 value
        testBytes = bytes(b"\x00\x13\x7a\x42\x83\xfa\x7b\x00")
        testResult = 5482450658163456
        self.assertEqual(dataConverter.uinteger(testBytes, layoutField), testResult, "Not passed: {}".format(testResult))
        
        dataConverter.release()
    
    def test_03_character_cp037(self):
        layoutField = parser.LayoutField()
        pythonEncoding = True
        encodingName = "cp037"
        
        dataConverter = parser.DataConverter(pythonEncoding, encodingName)
        
        # English text
        testBytes = bytes(b"\xc8\x85\x93\x93\x96\x40\x85\xa5\x85\x99\xa8\x82\x96\x84\xa8\x5a")
        testResult = "Hello everybody!"
        self.assertEqual(dataConverter.string(testBytes, layoutField), testResult, "Not passed: {}".format(testResult))
        
        # text with control characters
        testBytes = bytes(b"\x00\xf5\xf9\x81\x16\x15\x98")
        testResult = "59aq"
        self.assertEqual(dataConverter.string(testBytes, layoutField), testResult, "Not passed: {}".format(testResult))
        
        # signs
        testBytes = bytes(b"\x7b\x5b\x50\x5d\x4e\x61")
        testResult = r"#$&)+/"
        self.assertEqual(dataConverter.string(testBytes, layoutField), testResult, "Not passed: {}".format(testResult))
        
        # LATIN text
        testBytes = bytes(b"\x64\x73\x48\xdf\xcc\x9c")
        testResult = "ÀËçÿöæ"
        self.assertEqual(dataConverter.string(testBytes, layoutField), testResult, "Not passed: {}".format(testResult))
        
        dataConverter.release()
            
    def test_04_character_ibm835(self):
        layoutField = parser.LayoutField()
        pythonEncoding = False
        encodingName = "x-IBM935"
        
        dataConverter = parser.DataConverter(pythonEncoding, encodingName)
        
        # English Hello everybody! text
        testBytes = bytes(b"\xc8\x85\x93\x93\x96\x40\x85\xa5\x85\x99\xa8\x82\x96\x84\xa8\x5a")
        testResult = "Hello everybody!"
        self.assertEqual(dataConverter.string(testBytes, layoutField), testResult, "Not passed: {}".format(testResult))
        
        # Chinese random text
        # testBytes = bytes(b"\x0e\x5b\xed\x0f\x0e\x58\xb2\x0f\x0e\x6a\xf7\x0f\x0e\x5d\x7f\x0f")
        testBytes =   bytes(b"\x0e\x5b\xed\x58\xb2\x6a\xf7\x5d\x7f\x0f")
        testResult = "诸谐豇龠"
        self.assertEqual(dataConverter.string(testBytes, layoutField), testResult, "Not passed: {}".format(testResult))
        
        # Chinese "Hello everybody!" text
        testBytes = bytes(b"\x0e\x4a\xf2\x4e\xd1\x4d\xc2\x0f\x5a")
        # testBytes = bytes(b"\x0e\x4a\xf2\x0f\x0e\x4e\xd1\x0f\x0e\x4d\xc2\x0f\x5a")
        testResult = "大家好!"
        self.assertEqual(dataConverter.string(testBytes, layoutField), testResult, "Not passed: {}".format(testResult))
        
        dataConverter.release()
        
    def test_05_packedDecimal(self):
        layoutField = parser.LayoutField()
        layoutField.size = 1
        layoutField.scale = 0
        pythonEncoding = True
        encodingName = parser.INPUTENCODING
        
        dataConverter = parser.DataConverter(pythonEncoding, encodingName)
                
        # 1 byte
        # max value
        testBytes = bytes(b"\xff")
        testResult = 15
        self.assertEqual(dataConverter.packedDecimal(testBytes, layoutField), testResult, "Not passed: {}".format(testResult))
        # min value
        testBytes = bytes(b"\xfd")
        testResult = -15
        self.assertEqual(dataConverter.packedDecimal(testBytes, layoutField), testResult, "Not passed: {}".format(testResult))
        # 59 value
        testBytes = bytes(b"\x6f")
        testResult = 6
        self.assertEqual(dataConverter.packedDecimal(testBytes, layoutField), testResult, "Not passed: {}".format(testResult))
        # -59 value
        testBytes = bytes(b"\x6d")
        testResult = -6
        self.assertEqual(dataConverter.packedDecimal(testBytes, layoutField), testResult, "Not passed: {}".format(testResult))
        # max value divided by 10
        layoutField.scale = 1
        testBytes = bytes(b"\xff")
        testResult = 1.5
        self.assertEqual(dataConverter.packedDecimal(testBytes, layoutField), testResult, "Not passed: {}".format(testResult))
        # min value divided by 10
        testBytes = bytes(b"\xfd")
        testResult = -1.5
        self.assertEqual(dataConverter.packedDecimal(testBytes, layoutField), testResult, "Not passed: {}".format(testResult))
        
        # 2 bytes
        layoutField.scale = 0
        # max value
        testBytes = bytes(b"\xff\xff")
        testResult = 151515
        self.assertEqual(dataConverter.packedDecimal(testBytes, layoutField), testResult, "Not passed: {}".format(testResult))
        # min value
        testBytes = bytes(b"\xff\xfd")
        testResult = -151515
        self.assertEqual(dataConverter.packedDecimal(testBytes, layoutField), testResult, "Not passed: {}".format(testResult))
        # 899 value
        testBytes = bytes(b"\x89\x9f")
        testResult = 899
        self.assertEqual(dataConverter.packedDecimal(testBytes, layoutField), testResult, "Not passed: {}".format(testResult))
        # 899 value divided by 1000
        layoutField.scale = 3
        testBytes = bytes(b"\x89\x9f")
        testResult = 0.899
        self.assertEqual(dataConverter.packedDecimal(testBytes, layoutField), testResult, "Not passed: {}".format(testResult))
        
        # 4 bytes
        layoutField.scale = 0
        # max value
        testBytes = bytes(b"\xff\xff\xff\xff")
        testResult = 15151515151515
        self.assertEqual(dataConverter.packedDecimal(testBytes, layoutField), testResult, "Not passed: {}".format(testResult))
        # min value
        testBytes = bytes(b"\x00\x00\x00\x00")
        testResult = 0
        self.assertEqual(dataConverter.packedDecimal(testBytes, layoutField), testResult, "Not passed: {}".format(testResult))
        # 35791 value
        testBytes = bytes(b"\x00\x35\x79\x1f")
        testResult = 35791
        self.assertEqual(dataConverter.packedDecimal(testBytes, layoutField), testResult, "Not passed: {}".format(testResult))
        # 35791 value divided by 10
        layoutField.scale = 2
        testBytes = bytes(b"\x00\x35\x79\x1f")
        testResult = 357.91
        self.assertEqual(dataConverter.packedDecimal(testBytes, layoutField), testResult, "Not passed: {}".format(testResult))
        
        dataConverter.release()
        
    def test_06_decimal(self):
        layoutField = parser.LayoutField()
        layoutField.size = 1
        layoutField.scale = 0
        pythonEncoding = True
        encodingName = parser.INPUTENCODING
        
        dataConverter = parser.DataConverter(pythonEncoding, encodingName)
                
        # 1 byte
        # max value
        testBytes = bytes(b"\xf9")
        testResult = 9
        self.assertEqual(dataConverter.decimal(testBytes, layoutField), testResult, "Not passed: {}".format(testResult))
        # min value
        testBytes = bytes(b"\xd9")
        testResult = -9
        self.assertEqual(dataConverter.decimal(testBytes, layoutField), testResult, "Not passed: {}".format(testResult))
        # 6 value
        testBytes = bytes(b"\xf6")
        testResult = 6
        self.assertEqual(dataConverter.decimal(testBytes, layoutField), testResult, "Not passed: {}".format(testResult))
        # -6 value
        testBytes = bytes(b"\xd6")
        testResult = -6
        self.assertEqual(dataConverter.decimal(testBytes, layoutField), testResult, "Not passed: {}".format(testResult))
        # 0 value
        testBytes = bytes(b"\xf0")
        testResult = 0
        self.assertEqual(dataConverter.decimal(testBytes, layoutField), testResult, "Not passed: {}".format(testResult))
        # max value divided by 10
        layoutField.scale = 1
        testBytes = bytes(b"\xf9")
        testResult = 0.9
        self.assertEqual(dataConverter.decimal(testBytes, layoutField), testResult, "Not passed: {}".format(testResult))
        # min value divided by 10
        testBytes = bytes(b"\xd9")
        testResult = -0.9
        self.assertEqual(dataConverter.decimal(testBytes, layoutField), testResult, "Not passed: {}".format(testResult))
        
        # 2 bytes
        layoutField.scale = 0
        # max value
        testBytes = bytes(b"\xf9\xf9")
        testResult = 99
        self.assertEqual(dataConverter.decimal(testBytes, layoutField), testResult, "Not passed: {}".format(testResult))
        # min value
        testBytes = bytes(b"\xf9\xd9")
        testResult = -99
        self.assertEqual(dataConverter.decimal(testBytes, layoutField), testResult, "Not passed: {}".format(testResult))
        # 0 value
        testBytes = bytes(b"\xf0\xf0")
        testResult = 0
        self.assertEqual(dataConverter.decimal(testBytes, layoutField), testResult, "Not passed: {}".format(testResult))
        # 81 value divided by 1000
        layoutField.scale = 3
        testBytes = bytes(b"\xf8\xf1")
        testResult = 0.081
        self.assertEqual(dataConverter.decimal(testBytes, layoutField), testResult, "Not passed: {}".format(testResult))
         
        # 4 bytes
        layoutField.scale = 0
        # max value
        testBytes = bytes(b"\xf9\xf9\xf9\xf9")
        testResult = 9999
        self.assertEqual(dataConverter.decimal(testBytes, layoutField), testResult, "Not passed: {}".format(testResult))
        # min value
        testBytes = bytes(b"\xf9\xf9\xf9\xd9")
        testResult = -9999
        self.assertEqual(dataConverter.decimal(testBytes, layoutField), testResult, "Not passed: {}".format(testResult))
        # 0 value
        testBytes = bytes(b"\x00\x00\x00\x00")
        testResult = 0
        self.assertEqual(dataConverter.decimal(testBytes, layoutField), testResult, "Not passed: {}".format(testResult))
        # 35791 value
        testBytes = bytes(b"\xf6\xf7\xf8\xf9")
        testResult = 6789 
        self.assertEqual(dataConverter.decimal(testBytes, layoutField), testResult, "Not passed: {}".format(testResult))
        # 35791 value divided by 10
        layoutField.scale = 2
        testBytes = bytes(b"\xf5\xf7\xf9\xf1")
        testResult = 57.91
        self.assertEqual(dataConverter.decimal(testBytes, layoutField), testResult, "Not passed: {}".format(testResult))
        
        dataConverter.release()
        
if __name__=="__main__":
    unittest.main()