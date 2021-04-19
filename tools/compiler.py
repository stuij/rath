#!/usr/bin/env python

import argparse
import os
import re

LAST_WORD = "beany_pal"

# classes
class Stmt:
    def __init__(self, name, tokens):
        self.name = name
        self.tokens = tokens

    def to_ass(self, file, prev_word):
        raise CompileError("to_ass not implemented for this stmt: {0}".format(self.print()))

    def __repr__(self):
        return self.print()

    def __str__(self):
        return self.print()

    def print(self):
        return "stmt: {0} _ {1}".format(self.name, self.tokens[0].line)


class Const(Stmt):
    def __init__(self, name, tokens, val):
        super().__init__(name, tokens)
        self.val = val

    def to_ass(self, file, prev_word):
        ass_name = name_to_ass(self.name)
        out = '  head {0},{1},"{2}",docon,{3}\n    .word 0x{4}\n\n'.format(
            ass_name, len(self.name), self.name, prev_word, hex(self.val))
        file.write(out)
        return ass_name

    def print(self):
        return "const: {0} . {1} _ {2}".format(self.name, self.val, self.tokens[0].line)


class Var(Stmt):
    def __init__(self, name, tokens):
        super().__init__(name, tokens)

    def to_ass(self, file, prev_word):
        ass_name = name_to_ass(self.name)
        out = '  head {0},{1},"{2}",dovar,{3}\n    .word 0\n\n'.format(
            ass_name, len(self.name), self.name, prev_word)
        file.write(out)
        return ass_name

    def print(self):
        return "var: {0} _ {1}".format(self.name, self.tokens[0].line)


class Nr(Stmt):
    def __init__(self, name, token, val):
        super().__init__(name, [token])
        self.val = val

    def print(self):
        return "const: {0} . {1} _ {2}".format(self.name, self.val, self.tokens[0].line)


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


class Context():
    def __init__(self):
        self.stmts = []
        self.words = {}
        self.base = 10
        self.stack = []


class CompileError(Exception):
    """Base class for exceptions in this module."""
    pass


# tokenize
def tokenize(file):
    tokens = []
    with open(file) as fp:
        line = fp.readline()
        count = 1
        while line:
            things = line.split()
            [tokens.append(Token(x, count)) for x in things]
            line = fp.readline()
    return tokens

def peek(tokens):
    return tokens[0]


# parse
def parse_fn(tokens, context):
    raise CompileError("function parsing not yet implemented")

def parse_comment(tokens):
    while tokens.pop(0).tok != ")":
        pass

def parse_var(tokens, context):
    const = tokens.pop(0)
    name = tokens.pop(0)
    stmt = Var(name.tok, [const, name])
    context.stmts.append(stmt)
    context.words[name] = stmt

def parse_const(tokens, context):
    assert len(context.stack) > 0, "expected at least one token on the stack!!"
    number = context.stack.pop()
    assert isinstance(number, Nr), "expected number!!: {0}".format(number)
    const = tokens.pop(0)
    name = tokens.pop(0)
    stmt = Const(name.tok, [number, const, name], number.val)
    context.stmts.append(stmt)
    context.words[name] = stmt

def parse_number(tokens, context):
    nr = int(peek(tokens).tok, context.base)
    if nr >= -0x80000000 and nr <= 0xffffffff:
        token = tokens.pop(0)
        context.stack.append(Nr(token.tok, token, nr))
    else:
        raise CompileError("number isn't within range!!: {0}".format(peek(tokens)))

def parse_arith(tokens, context):
    raise CompileError("parse_arith not yet implemented")

def parse_token(tokens, context):
    next = peek(tokens)
    token = next.tok

    try:
        parse_number(tokens, context)
        return
    except ValueError:
        pass

    if token == 'hex':
        tokens.pop(0)
        context.base = 16
    elif token == ':':
        parse_fn(tokens, context)
    elif token == "(":
        parse_comment(tokens)
    # no compile time operations allowed currently
    elif token == 'variable':
        parse_var(tokens, context)
    elif token == 'constant':
        parse_const(tokens, context)
    elif token in ['+', '-', '*']:
        parse_arith(tokens, context)
    else:
        context.stack.append(tokens.pop(0))


def parse(tokens):
    """Currently parse just supports fn, variable and constant definitions."""
    context = Context()

    while tokens:
        parse_token(tokens, context)

    if context.stack:
        raise CompileError("after parsing, there are still words on the stack!!:\n{0}".format(
            context.stack))

    return context

# output
def name_to_ass(name):
    return re.sub('-', '_', name)

def to_assembly(out, context):
    global LAST_WORD
    prev_word = LAST_WORD
    with open(out, 'w') as f:
        f.write("# compiled from Forth file\n\n")
        for stmt in context.stmts:
            prev_word = stmt.to_ass(f, prev_word)
              
        f.write(".set lastword, link_{0} /* last word */\n".format(prev_word))

# start
def main():
    parser = argparse.ArgumentParser(description='high-level Forth word compiler')
    parser.add_argument('file', help='The file that needs to be compiled')
    parser.add_argument('-o', default="",
                        help='the output file')
    args = parser.parse_args()
    out = os.path.splitext(os.path.basename(args.file))[0] + ".s" if args.o == "" else args.o

    tokens = tokenize(args.file)
    context = parse(tokens)
    to_assembly(out, context)

if __name__ == "__main__":
    main()
