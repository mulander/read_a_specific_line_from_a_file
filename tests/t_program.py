import unittest
import os
import subprocess

class TestProgram(unittest.TestCase):
	def setUp(self):
                self.bin = "../rosetta_read"
                self.target = "./input.txt"
                self.cmd = "%s %s" % (self.bin, self.target)
                self.no_file = "File %s does not exist or is not a regular file\n"
                self.fewer_than_7_lines = "%s has fewer than 7 lines\n"
                self.handled_exception = 'Error while trying to read file: %s\n'
                self.not_a_file = 'File %s does not exist or is not a regular file\n'
                self.line_7_empty = 'Line 7 in %s is empty\n'
                self.line_too_long = 'Line 7 of %s too long to store in memory available to this progra\n'
                if os.path.exists(self.target):
                        if os.path.isdir(self.target):
                                os.rmdir(self.target)
                        else:
                                os.remove(self.target)

	def tearDown(self):
                if os.path.exists(self.target):
                        if os.path.isdir(self.target):
                                os.rmdir(self.target)
                        else:
                                os.remove(self.target)

	def test_file_does_not_exist(self):
                p = subprocess.Popen([self.bin, self.target],stderr=subprocess.PIPE)

                output = ''.join(p.stderr.readlines())
                self.assertEqual(output, self.no_file % self.target)

        def test_file_empty(self):
                subprocess.call(["touch",self.target])
                p = subprocess.Popen([self.bin, self.target],stderr=subprocess.PIPE)

                output = ''.join(p.stderr.readlines())
                self.assertEqual(output, self.fewer_than_7_lines % self.target)

        def test_no_read_access_to_file(self):
                subprocess.call(["touch",self.target])
                subprocess.call(["chmod","a-r",self.target])
                p = subprocess.Popen([self.bin, self.target],stderr=subprocess.PIPE)

                output = ''.join(p.stderr.readlines())
                self.assertEqual(output, self.handled_exception % self.target)

        def test_file_fewer_lines_than_7(self):
                subprocess.call(["touch",self.target])
                f = open(self.target,'w')
                f.write("one line\n")
                f.close()
                p = subprocess.Popen([self.bin, self.target],stderr=subprocess.PIPE)

                output = ''.join(p.stderr.readlines())
                self.assertEqual(output, self.fewer_than_7_lines % self.target)

        def test_target_is_a_directory(self):
                subprocess.call(["mkdir",self.target])
                p = subprocess.Popen([self.bin, self.target],stderr=subprocess.PIPE)

                output = ''.join(p.stderr.readlines())
                self.assertEqual(output, self.not_a_file % self.target)
                subprocess.call(["rmdir",self.target])

        def test_target_is_a_special_file(self):
                subprocess.call(["ln","-s",self.target])
                p = subprocess.Popen([self.bin, self.target],stderr=subprocess.PIPE)

                output = ''.join(p.stderr.readlines())
                self.assertEqual(output, self.not_a_file % self.target)
                subprocess.call(["rm",self.target])

        def test_line_7_empty(self):
                data = "\n".join(["1","2","3","4","5","6","","8","9"])
                f = open(self.target,'w')
                f.write(data)
                f.close()
                p = subprocess.Popen([self.bin, self.target],stdout=subprocess.PIPE, stderr=subprocess.PIPE)
                output = ''.join(p.stdout.readlines())
                self.assertEqual(output, self.line_7_empty % self.target)

        def test_line_7_correct(self):
                data = "\n".join(["1","2","3","4","5","6","7 correct","8","9"])
                f = open(self.target,'w')
                f.write(data)
                f.close()
                p = subprocess.Popen([self.bin, self.target],stdout=subprocess.PIPE, stderr=subprocess.PIPE)
                output = ''.join(p.stdout.readlines())
                self.assertEqual(output, "7 correct\n")

        def test_line_to_large_to_store_in_memory(self):
                "Creates a 11M file, most of the data located at line 7. See man ld --stack for details"
                large_line = 'A' * ((262144 * 2) * 20)
                data = "\n".join(["1","2","3","4","5","6",large_line,"8","9"])
                f = open(self.target,'w')
                f.write(data)
                f.close()

                p = subprocess.Popen([self.bin, self.target],stdout=subprocess.PIPE, stderr=subprocess.PIPE)
                output = ''.join(p.stderr.readlines())
                self.assertEqual(output, self.line_too_long % self.target)

if __name__ == '__main__':
	unittest.main()
