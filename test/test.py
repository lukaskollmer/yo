#!/usr/bin/env python3

"""
yo test runner
"""

import os
import re
import subprocess

TESTS_ROOT_DIR = os.path.join(os.getcwd(), 'test')


OPTION_PATTERN = re.compile(r'// ([\w-]+): ([\w-]+)')


DEFAULT_OPTIONS = {
    'exit-code': 0
}


class TestOptions(object):
    def __init__(self, options):
        self.options = options
    
    def __getitem__(self, name):
        try:
            return self.options[name]
        except KeyError:
            return DEFAULT_OPTIONS[name]



def collect_tests():
    all_tests = []
    for (dirpath, _, filenames) in os.walk(os.path.join(TESTS_ROOT_DIR, 'tests')):
        all_tests.extend(map(lambda x: os.path.join(dirpath, x), filter(lambda x: not x.startswith('_'), filenames)))
    return all_tests



def parse_test_options(path):
    lines = []
    with open(path, 'r') as file:
        while True:
            line: str = file.readline().rstrip() 
            if not line.startswith('//'):
                break
            lines.append(line)
    
    options = {}
    for line in lines:
        name, value = OPTION_PATTERN.search(line).group(1, 2)
        options[name] = value
    
    return TestOptions(options)


class TestResult(object):
    def __init__(self, path):
        self.path = path
        self.failures = []
    
    def add_failure(self, option_name, actual, expected):
        self.failures.append((option_name, actual, expected))


class TestRunner(object):
    def __init__(self, test_file_paths):
        self.test_file_paths = test_file_paths
        self.test_results = {}
    

    def run_all(self):
        for path in self.test_file_paths:
            self.run_test(path)
    

    def all_succeeded(self):
        for result in self.test_results.values():
            if len(result.failures) > 0:
                return False
        return True

    def check(self, path, options, option_name, actual):
        expected = str(options[option_name])
        actual = str(actual)
        if expected != actual:
            self.test_results[path].add_failure(option_name, expected, actual)
    

    def run_test(self, path):
        self.test_results[path] = TestResult(path)
        options = parse_test_options(path)
        process = subprocess.run(['./build/Debug/yo', path], stdout=subprocess.PIPE)

        # check
        # TODO make sure we actually check all possible options
        #all_options = DEFAULT_OPTIONS.keys()

        self.check(path, options, 'exit-code', process.returncode)


    def print_report(self):
        print("Tests:")
        for result in self.test_results.values():
            print(f"- .{result.path.replace(TESTS_ROOT_DIR, '')}: {'Success' if not result.failures else 'Failed'}")
            for (option_name, expected, actual) in result.failures:
                print(f"  - [{option_name}]: Expected {expected}, got {actual}")


if __name__ == '__main__':
    test_runner = TestRunner(collect_tests())
    test_runner.run_all()
    test_runner.print_report()

    exit(0 if test_runner.all_succeeded() else 1)