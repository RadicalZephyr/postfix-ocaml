#!/usr/bin/env python

import re
from subprocess32 import check_output, CalledProcessError, STDOUT

postfix_prog = "./postfix.byte"

result_re = re.compile(r"Result: '(?P<result>-?\d+)'")

def run_test(program, args, result):
    to_run = [postfix_prog, "(postfix {})".format(program)]
    to_run.extend(args.split())

    try:
        output = check_output(to_run, stderr=STDOUT)
        output = output[:-1]
        match = result_re.match(output)
        if match and result == match.group("result"):
            print u'  \u2713'
        else:
            print u"  \u2718: Expected '{}', got '{}'\n".format(result,
                                                       output)

    except CalledProcessError as e:
        print u"  \u2718: Errored\nRunning '{}'\n\n{}\n\n".format(to_run, e.output)

testre = re.compile(r"^(?P<program>.*?) \[(?P<args>.*?)\] (?P<result>-?\d+)")

with open("test/postfix-tests") as tests:
    for testline in tests:
        test = testre.match(testline[:-1])
        if test:
            run_test(test.group("program"),
                     test.group("args"),
                     test.group("result"))
