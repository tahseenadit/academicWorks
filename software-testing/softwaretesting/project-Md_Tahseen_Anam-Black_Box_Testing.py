import math
import unittest
import random
import sys
from ctypes import *

class Test( unittest.TestCase ):

    # Setup general variables to be used in functions test
    def setUp(self):
        self.NormalNumber1 = 0
        self.NormalNumber2 = 1e309
        self.NormalNumber3 = 10000000000000001
        self.NormalNumber4 = 1.0e-2
        self.NormalNumber5 = 1e308
        self.NegativeNumber1 = -0
        self.NegativeNumber2 = -40
        self.NegativeNumber3 = -100
        self.NegativeNumber4 = -1
        self.MaxNumber = sys.maxsize
        self.MinNumber = -sys.maxsize
        self.PositiveFloat1 = 0.0
        self.PositiveFloat2 = 40.04
        self.PositiveFloat3 = 100.001
        self.NegativeFloat1 = -0.0
        self.NegativeFloat2 = -40.04
        self.NegativeFloat3 = -100.001
        self.RandomNumber1 = random.random()
        self.MathInfo1 = math.inf
        self.MathInfo2 = math.nan
        self.StringTest1 = 'This is String'
        self.StringTest2 = " "
        self.CharTest1 = 'h'
        self.CharTest2 = '2'
        self.BooleanTest1 = True
        self.BooleanTest2 = False

    def test_isqrt(self):
        self.assertRaises(TypeError, math.isqrt, None)
        self.assertRaises(TypeError, math.isqrt, "a")
        self.assertRaises(ValueError, math.isqrt, -1)
        self.assertEqual(math.isqrt(0), 0)
        self.assertEqual(math.isqrt(18446744073709551616), 4294967296)
        self.assertEqual(math.isqrt(sys.maxsize ** 2), 9223372036854775807)
        self.assertEqual(math.isqrt(9223372036854775807), 3037000499)
        self.assertEqual(math.isqrt(0xFFFFFFFF), 65535)
        self.assertEqual(math.isqrt(0b1111111111111111111111111111111), 46340)
        self.assertEqual(math.isqrt(0b111111111111111111111111111111), 32767)

    # Testing 'remainder' function which returns the IEEE 754-style remainder of x with respect to y
    # The function is tested using different values, including Zero, positive numbers,
    # negative numbers including -0, maximum allowed positive numbers, minimum allowed
    # negative numbers, positive and negative float numbers, character, string and random numbers

    def test_remainder(self):

        # Testing character and string
        self.assertRaises(TypeError, math.remainder, 2, self.CharTest1)
        self.assertRaises(TypeError, math.remainder, self.CharTest2, 4)
        self.assertRaises(TypeError, math.remainder, self.CharTest2, self.CharTest1)
        self.assertRaises(TypeError, math.remainder, self.StringTest1, 4)
        self.assertRaises(TypeError, math.remainder, 2, self.StringTest1)
        self.assertRaises(TypeError, math.remainder, 2, self.StringTest2)

        # Testing normal numbers
        self.assertRaises(ValueError, math.remainder, 2, self.NormalNumber1)
        self.assertRaises(ValueError, math.remainder, self.NormalNumber2, 2)
        self.assertEqual(math.remainder(self.NormalNumber3, 2), 0.0)

        # Testing nan value
        print(math.remainder(self.MathInfo2, 0))

        # Testing negative numbers
        self.assertRaises(ValueError, math.remainder, 2, self.NegativeNumber1)
        result = math.remainder(self.NegativeNumber1, 2)
        self.assertEqual(result, 0.0)
        result = math.remainder(self.NegativeNumber2, 2)
        self.assertEqual(result, -0.0)
        result = math.remainder(self.NegativeNumber3, 2)
        self.assertEqual(result, -0.0)
        result = math.remainder(self.NegativeNumber3, self.NegativeNumber2)
        self.assertEqual(result, -20.0)

        # Testing positive and negative floats
        self.assertRaises(ValueError, math.remainder, 2, self.PositiveFloat1)
        self.assertRaises(ValueError, math.remainder, 2, self.NegativeFloat1)
        result = math.remainder(self.PositiveFloat1, 2)
        self.assertEqual(result, 0.0)
        result = math.remainder(self.NegativeFloat1, 2)
        self.assertEqual(result, -0.0)
        result = math.remainder(self.PositiveFloat3, -40)
        self.assertEqual(result, -19.998999999999995)
        result = math.remainder(self.NegativeFloat2, self.NegativeFloat3)
        self.assertEqual(result, -40.04)

        # Testnig min and max
        result = math.remainder(self.MaxNumber, 3)
        self.assertEqual(result, -1.0)
        result = math.remainder(self.MinNumber, 3)
        self.assertEqual(result, 1.0)

        # Testing random numbers
        print(math.remainder(self.RandomNumber1, 3))


    # Testing 'log10' function which returns the base-10 logarithm of x
    # The function is tested using different values, including Zero, positive numbers,
    # negative numbers including -0, maximum allowed positive numbers, minimum allowed
    # negative numbers, positive and negative float numbers, character, string and random numbers

    def test_log10(self):

        # Testing character and string
        self.assertRaises(TypeError, math.log10, self.CharTest1)
        self.assertRaises(TypeError, math.log10, self.CharTest2)
        self.assertRaises(TypeError, math.log10, self.StringTest1)
        self.assertRaises(TypeError, math.log10, self.StringTest2)

        # Testing normal numbers
        self.assertRaises(ValueError, math.log10, self.NormalNumber1)
        self.assertEqual(math.log10(self.NormalNumber2), math.inf)
        self.assertEqual(math.log10(self.NormalNumber4), -2)

        # Testing nan value
        print(math.log10(self.MathInfo2))

        # Testing negative numbers
        self.assertRaises(ValueError, math.log10, self.NegativeNumber1)
        self.assertRaises(ValueError, math.log10, self.NegativeNumber2)
        self.assertRaises(ValueError, math.log10, self.NegativeNumber3)
        self.assertRaises(ValueError, math.log10, self.NegativeNumber4)

        # Testing positive and negative floats
        self.assertRaises(ValueError, math.log10, self.PositiveFloat1)
        result = math.log10(self.PositiveFloat2)
        self.assertEqual(result, 1.602494068807281)
        result = math.log10(self.PositiveFloat3)
        self.assertEqual(result, 2.0000043429231047)
        self.assertRaises(ValueError, math.log10, self.NegativeFloat1)
        self.assertRaises(ValueError, math.log10, self.NegativeFloat2)
        self.assertRaises(ValueError, math.log10, self.NegativeFloat3)

        # Testing max and min value
        print(math.log10(self.MaxNumber))
        self.assertRaises(ValueError, math.log10, self.MinNumber)

        # Testing random number
        print(math.log10(self.RandomNumber1))

    # Testing 'dist' which returns the Euclidean distance between two points p and q, each given as a sequence (or iterable) of coordinates.
    # The function is tested using different values, including Zero, positive numbers,
    # negative numbers including -0, maximum allowed positive numbers, minimum allowed
    # negative numbers, positive and negative float numbers, character, string and random numbers

    def test_dist(self):

        # Testing character and string
        self.assertRaises(TypeError, math.dist, [self.StringTest2], [1])
        self.assertRaises(TypeError, math.dist, [self.StringTest1], [1])
        self.assertRaises(TypeError, math.dist, [self.CharTest1], [1])
        self.assertRaises(TypeError, math.dist, [self.CharTest2], [1])
        self.assertRaises(TypeError, math.dist, [self.CharTest2], [self.StringTest1])

        # Testing normal numbers
        self.assertRaises(TypeError, math.dist, [self.NormalNumber1], 1)
        result = math.dist([self.NormalNumber2], [self.NormalNumber5])
        self.assertEqual(result, self.MathInfo1)
        result = math.dist([self.NormalNumber3], [self.NormalNumber5])
        self.assertEqual(result, self.NormalNumber5)
        result = math.dist([self.NormalNumber3], [self.NormalNumber1])
        self.assertEqual(result, 1e16)
        result = math.dist([], [])
        self.assertEqual(result, 0.0)

        # Testing nan and inf value
        self.assertRaises(ValueError, math.dist, [self.MathInfo2], [])
        print(math.dist([self.MathInfo2], [1]))
        result = math.dist([self.MathInfo1], [1])
        self.assertEqual(result, self.MathInfo1)
        self.assertEqual(math.dist([self.NormalNumber2], [self.NormalNumber5]), self.MathInfo1)

        # Testing negative numbers
        result = math.dist([self.NegativeNumber1], [self.NegativeNumber4])
        self.assertEqual(result, 1.0)
        result = math.dist([self.NegativeNumber2], [self.NegativeNumber3])
        self.assertEqual(result, 60.0)

        # Testing positive and negative floats
        result = math.dist([self.NegativeFloat1], [self.NegativeFloat2])
        self.assertEqual(result, 40.04)
        result = math.dist([self.NegativeFloat2], [self.NegativeFloat3])
        self.assertEqual(result, 59.961000000000006)
        result = math.dist([self.NegativeFloat1], [self.PositiveFloat1])
        self.assertEqual(result, 0.0)

        # Testing max and min value
        result = math.dist([self.MaxNumber], [self.MinNumber])
        self.assertEqual(result, 1.8446744073709552e19)

        # Testing random numbers
        result = math.dist([self.RandomNumber1], [self.RandomNumber1])
        self.assertEqual(result, 0.0)

    # Testing 'frexp' which returns the mantissa and exponent of x as the pair (m, e).
    # The function is tested using different values, including Zero, positive numbers,
    # negative numbers including -0, maximum allowed positive numbers, minimum allowed
    # negative numbers, positive and negative float numbers, character, string and random numbers

    def test_frexp(self):

        # Testing character and string
        self.assertRaises(TypeError, math.frexp, self.StringTest1)
        self.assertRaises(TypeError, math.frexp, self.StringTest2)
        self.assertRaises(TypeError, math.frexp, self.CharTest1)
        self.assertRaises(TypeError, math.frexp, self.CharTest2)

        # Testing normal numbers
        self.assertEqual(math.frexp(self.NormalNumber1), (0.0, 0))
        self.assertEqual(math.frexp(self.NormalNumber2), (self.MathInfo1, 0))
        self.assertEqual(math.frexp(self.NormalNumber3), (0.5551115123125783, 54))
        self.assertEqual(math.frexp(self.NormalNumber4), (0.64, -6))
        self.assertEqual(math.frexp(self.NormalNumber5), (0.5562684646268004, 1024))

        # Testing nan and inf value
        self.assertEqual(math.frexp(-self.MathInfo1), (-self.MathInfo1, 0))
        print(math.frexp(self.MathInfo2))

        # Testing negative numbers
        self.assertEqual(math.frexp(self.NegativeNumber1), (0.0, 0))
        self.assertEqual(math.frexp(self.NegativeNumber2), (-0.625, 6))
        self.assertEqual(math.frexp(self.NegativeNumber3), (-0.78125, 7))
        self.assertEqual(math.frexp(self.NegativeNumber4), (-0.5, 1))

        # Testing positive and negative floats

        self.assertEqual(math.frexp(self.PositiveFloat1), (0.0, 0))
        self.assertEqual(math.frexp(self.PositiveFloat2), (0.625625, 6))
        self.assertEqual(math.frexp(self.PositiveFloat3), (0.7812578125, 7))
        self.assertEqual(math.frexp(self.NegativeFloat1), (-0.0, 0))
        self.assertEqual(math.frexp(self.NegativeFloat2), (-0.625625, 6))
        self.assertEqual(math.frexp(self.NegativeFloat3), (-0.7812578125, 7))

        # Testing min and max value
        self.assertEqual(math.frexp(self.MinNumber), (-0.5, 64))
        self.assertEqual(math.frexp(self.MaxNumber), (0.5, 64))

        # Testing random value
        print(math.frexp(self.RandomNumber1))

        # Testing boolean value
        self.assertEqual(math.frexp(self.BooleanTest1**self.BooleanTest2), (0.5, 1))
        self.assertEqual(math.frexp(self.BooleanTest2**self.BooleanTest1), (0.0, 0))

    # Testing 'ldexp' which returns x * (2**i). This is essentially the inverse of function frexp().
    # The function is tested using different values, including Zero, positive numbers,
    # negative numbers including -0, maximum allowed positive numbers, minimum allowed
    # negative numbers, positive and negative float numbers, character, string and random numbers

    def test_ldexp(self):

        # Testing character and string
        self.assertRaises(TypeError, math.ldexp, self.StringTest1, self.StringTest1)
        self.assertRaises(TypeError, math.ldexp, self.StringTest1, self.StringTest2)
        self.assertRaises(TypeError, math.ldexp, self.CharTest1, self.CharTest2)

        # Testing normal numbers
        self.assertRaises(OverflowError, math.ldexp, int(self.NormalNumber5), 1)
        self.assertEqual(math.ldexp(self.NormalNumber1,1), 0.0)
        self.assertEqual(math.ldexp(self.NormalNumber2,1), self.MathInfo1)
        self.assertEqual(math.ldexp(self.NormalNumber3,1), 2e16)
        self.assertEqual(math.ldexp(self.NormalNumber4,1), 0.02)
        self.assertRaises(OverflowError, math.ldexp, int(self.NormalNumber3), self.NormalNumber3)
        self.assertRaises(OverflowError, math.ldexp, 1, 10000)

        # Testing nan value
        print(math.ldexp(math.nan, 1))

        # Testing negative numbers
        self.assertEqual(math.ldexp(self.NegativeNumber1, 1), 0.0)
        self.assertEqual(math.ldexp(self.NegativeNumber1, self.NegativeNumber1), 0.0)
        self.assertEqual(math.ldexp(self.NegativeNumber1, self.NegativeNumber2), 0.0)
        self.assertEqual(math.ldexp(self.NegativeNumber2, self.NegativeNumber2), -3.637978807091713e-11)
        self.assertEqual(math.ldexp(self.NegativeNumber3, self.NegativeNumber2), -9.094947017729282e-11)
        self.assertEqual(math.ldexp(self.NegativeNumber4, self.NegativeNumber2), -9.094947017729282e-13)
        self.assertEqual(math.ldexp(self.NegativeNumber4, self.NegativeNumber1), -1.0)
        self.assertEqual(math.ldexp(self.NegativeNumber3, self.NegativeNumber1), -100.0)

        # Testing positive and negative floats

        self.assertEqual(math.ldexp(self.PositiveFloat1, 1), 0.0)
        self.assertRaises(TypeError, math.ldexp, self.PositiveFloat1, self.PositiveFloat1)
        self.assertRaises(TypeError, math.ldexp, self.PositiveFloat1, self.PositiveFloat2)
        self.assertRaises(TypeError, math.ldexp, self.PositiveFloat1, self.PositiveFloat3)
        self.assertRaises(TypeError, math.ldexp, self.PositiveFloat2, self.PositiveFloat3)
        self.assertRaises(TypeError, math.ldexp, self.PositiveFloat3, self.PositiveFloat3)
        self.assertRaises(TypeError, math.ldexp, self.NegativeFloat1, self.PositiveFloat3)
        self.assertRaises(TypeError, math.ldexp, self.NegativeFloat2, self.PositiveFloat3)
        self.assertRaises(TypeError, math.ldexp, self.NegativeFloat3, self.PositiveFloat3)
        self.assertRaises(TypeError, math.ldexp, self.NegativeFloat3, self.NegativeFloat2)
        self.assertRaises(TypeError, math.ldexp, self.NegativeFloat3, self.NegativeFloat1)
        self.assertEqual(math.ldexp(self.NegativeFloat1, 1), 0.0)

        # Testing min and max value
        self.assertEqual(math.ldexp(self.MaxNumber, 1), 1.8446744073709552e+19)
        self.assertEqual(math.ldexp(self.MinNumber, 1), -1.8446744073709552e+19)
        self.assertEqual(math.ldexp(self.MaxNumber, self.MinNumber), 0.0)
        self.assertEqual(math.ldexp(self.MinNumber, self.MinNumber), 0.0)
        self.assertRaises(OverflowError, math.ldexp, self.MinNumber, self.MaxNumber)
        self.assertRaises(OverflowError, math.ldexp, self.MaxNumber, self.MaxNumber)

        # Testing random value
        print(math.ldexp(self.RandomNumber1, 1))

        # Testing boolean value
        result = math.ldexp(self.BooleanTest1, self.BooleanTest2)
        self.assertEqual(result, 1.0)
        result = math.ldexp(self.BooleanTest2, self.BooleanTest1)
        self.assertEqual(result, 0.0)

if __name__ == '__main__' :
    unittest.main()



