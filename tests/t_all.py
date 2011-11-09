import unittest
import sys

from t_program import TestProgram

def add_test(test):
	return unittest.TestLoader().loadTestsFromTestCase(test)

if __name__ == '__main__':
        if sys.platform != 'linux2':
                print "This test suite was only tested on the 'linux2' platform"
                print "Please feel free to port and prove it's correctness"
                print "on other platforms."
                sys.exit(0)
	suite = unittest.TestSuite()
	suite.addTests( add_test( TestProgram ) )
	unittest.TextTestRunner(verbosity=2).run(suite)
