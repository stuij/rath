#!/usr/bin/env python

import argparse
import re
import sys

LAST_WORD = "beany_pal"


class Stmt:
    def __init__(self, name, words):
        self.name = name
        self.words = words

class Const(Stmt):
    def __init__(self, name, words, val):
        super().__init__(name, words)
        self.val = val

def tokenize(file):
    with open(file) as fp:
        return fp.read().split()

def peek(tokens):
    return tokens[0]

# todo at some point
# def chunk_tokenize(file_object, chunk_size=1024):
#     """Lazy function (generator) to read a file line per line
#     returning """
#     line_nr = 0
#     while True:
#         line_nr += 1
#         line = file_object.readline()
#         if not line:
#             break
#         yield line, line_nr

def parse_fn(tokens, stmts, words):
    sys.exit("function parsing not yet implemented")

def parse_comment(tokens):
    while tokens.pop(0) != ")":
        pass

def parse_var(tokens, stmts, words):
    sys.exit("variable parsing not yet implemented")

def parse_const(tokens, stmts, words):
    val = tokens.pop(0)
    const = tokens.pop(0)
    name = tokens.pop(0)
    stmt = Const(name, [val, const, name], val)
    stmts.append(stmt)
    words[name] = stmt

def parse(tokens):
    """Currently parse just supports fn, variable and constant definitions."""
    stmts = []
    words = {}
    while tokens:
        next = peek(tokens)
        # we're unsophistically assuming everything is in hex
        if next == 'hex':
            tokens.pop(0)
        elif next == ':':
            parse_fn(tokens, stmts, words)
        elif next == "(":
            parse_comment(tokens)
        # no compile time operations allowed currently
        elif tokens[1] == 'variable':
            parse_var(tokens, stmts, words)
        elif tokens[1] == 'constant':
            parse_const(tokens, stmts, words)
        else:
            sys.exit("word sequence not recognized as Forth at: {0}".format(next))

    return stmts, words

def name_to_ass(name):
    return re.sub('-', '_', name)

def print_constant(file, const, prev_word):
    ass_name = name_to_ass(const.name)
    out = '  head {0},{1},"{2}",docon,{3}\n    .word 0x{4}\n\n'.format(
        ass_name, len(const.name), const.name, prev_word, const.val)
    file.write(out)
    return ass_name

def to_assembly(out, stmts):
    global LAST_WORD
    prev_word = LAST_WORD
    with open(out, 'w') as f:
        f.write("# compiled from Forth file\n\n")
        for stmt in stmts:
            prev_word = print_constant(f, stmt, prev_word)
        f.write(".set lastword, link_{0} /* last word */\n".format(prev_word))

def main():
    parser = argparse.ArgumentParser(description='high-level Forth word compiler')
    parser.add_argument('file', help='The file that needs to be compiled')
    parser.add_argument('-o', default="",
                        help='the output file')
    args = parser.parse_args()
    out = args.file if args.o == "" else args.o

    tokens = tokenize(args.file)
    stmts, _ = parse(tokens)
    to_assembly(out, stmts)

if __name__ == "__main__":
    main()
