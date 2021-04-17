#!/usr/bin/env python

import argparse
import os
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

class Var(Stmt):
    def __init__(self, name, words):
        super().__init__(name, words)

class Token():
    def __init__(self, tok, line):
        self.tok = tok
        self.line = line

    def __repr__(self):
        return self.print()

    def __str__(self):
        return self.print()

    def print(self):
        return "tok: {0} _ {1}".format(self.tok, self.line)

def tokenize(file, tokens):
    with open(file) as fp:
        line = fp.readline()
        count = 1
        while line:
            things = line.split()
            [tokens.append(Token(x, count)) for x in things]
            line = fp.readline()

def peek(tokens):
    return tokens[0]

def parse_fn(tokens, stmts, words):
    sys.exit("function parsing not yet implemented")

def parse_comment(tokens):
    while tokens.pop(0).tok != ")":
        pass

def parse_var(tokens, stmts, words):
    const = tokens.pop(0)
    name = tokens.pop(0)
    stmt = Var(name.tok, [const, name])
    stmts.append(stmt)
    words[name] = stmt

def parse_const(tokens, stmts, words, stack):
    assert len(stack) > 0, "expected at least one token on the stack!!"
    val = stack.pop()
    const = tokens.pop(0)
    name = tokens.pop(0)
    stmt = Const(name.tok, [val, const, name], val.tok)
    stmts.append(stmt)
    words[name] = stmt

def parse(tokens):
    """Currently parse just supports fn, variable and constant definitions."""
    stmts = []
    words = {}
    stack = []
    while tokens:
        next = peek(tokens)
        token = next.tok
        # we're unsophistically assuming everything is in hex
        if token == 'hex':
            tokens.pop(0)
        elif token == ':':
            parse_fn(tokens, stmts, words)
        elif token == "(":
            parse_comment(tokens)
        # no compile time operations allowed currently
        elif token == 'variable':
            parse_var(tokens, stmts, words)
        elif token == 'constant':
            parse_const(tokens, stmts, words, stack)
        else:
            stack.append(next)
            tokens.pop(0)

    if stack:
        print("after parsing, there are still words on the stack!!:\n{0}".format(stack))

    return stmts, words

def name_to_ass(name):
    return re.sub('-', '_', name)

def const_to_ass(file, const, prev_word):
    ass_name = name_to_ass(const.name)
    out = '  head {0},{1},"{2}",docon,{3}\n    .word 0x{4}\n\n'.format(
        ass_name, len(const.name), const.name, prev_word, const.val)
    file.write(out)
    return ass_name

def var_to_ass(file, var, prev_word):
    ass_name = name_to_ass(var.name)
    out = '  head {0},{1},"{2}",dovar,{3}\n    .word 0\n\n'.format(
        ass_name, len(var.name), var.name, prev_word)
    file.write(out)
    return ass_name

def to_assembly(out, stmts):
    global LAST_WORD
    prev_word = LAST_WORD
    with open(out, 'w') as f:
        f.write("# compiled from Forth file\n\n")
        for stmt in stmts:
            if isinstance(stmt, Const):
                prev_word = const_to_ass(f, stmt, prev_word)
            elif isinstance(stmt, Var):
                prev_word = var_to_ass(f, stmt, prev_word)
            else:
                sys.exit("statement type not supported!!: {0}".format(stmt))
              
        f.write(".set lastword, link_{0} /* last word */\n".format(prev_word))

def main():
    parser = argparse.ArgumentParser(description='high-level Forth word compiler')
    parser.add_argument('file', help='The file that needs to be compiled')
    parser.add_argument('-o', default="",
                        help='the output file')
    args = parser.parse_args()
    out = os.path.splitext(os.path.basename(args.file))[0] + ".s" if args.o == "" else args.o

    tokens = []
    tokenize(args.file, tokens)
    stmts, _ = parse(tokens)
    to_assembly(out, stmts)

if __name__ == "__main__":
    main()
