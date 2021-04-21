#!/usr/bin/env python

# Very very naive forth cross-compiler, just about functional enough to make this
# workable for a simple entry for a GBA game competition.

# When I have a bit more time (HA!), I'll write a full-blown one that will
# inherently basically be a whole Forth implementation all by itself.

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
        out = '  head {0},{1},"{2}",docon,{3}\n    .word {4}\n\n'.format(
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
        out = '  head {0},{1},"{2}",dovar,{3}\n    .word 0x0\n\n'.format(
            ass_name, len(self.name), self.name, prev_word)
        file.write(out)
        return ass_name

    def print(self):
        return "var: {0} _ {1}".format(self.name, self.tokens[0].line)


class Nr(Stmt):
    def __init__(self, name, token, val):
        super().__init__(name, [token])
        self.val = val

    def to_ass(self, file, prev_word):
        file.write("lit,{0}".format(hex(self.val)))

    def print(self):
        return "const: {0} . {1} _ {2}".format(self.name, self.val, self.tokens[0].line)


class Word(Stmt):
    def __init__(self, name, tokens, words):
        super().__init__(name, tokens)
        self.words = words

    def to_ass(self, file, prev_word):
        ass_name = name_to_ass(self.name)
        file.write('  head {0},{1},"{2}",docolon,{3}\n    .word '.format(
            ass_name, len(self.name), self.name, prev_word))

        for word in self.words:
            word.to_ass(file, prev_word)
            file.write(',')

        file.write('exit\n\n')

        return ass_name

    def print(self):
        return "const: {0} . {1} _ {2}".format(self.name, self.words, self.tokens[0].line)


class Token:
    def __init__(self, tok, line):
        self.tok = tok
        self.line = line

    def __repr__(self):
        return self.print()

    def __str__(self):
        return self.print()

    def to_ass(self, file, prev_word):
        file.write("{0}".format(name_to_ass(self.tok)))

    def print(self):
        return "tok: {0} _ {1}".format(self.tok, self.line)


class Context:
    def __init__(self):
        self.stmts = []
        self.words = {}
        self.base = 10
        self.stack = []
        self.tokens = []

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
def colon_compile(context):
    words = []
    tokens = context.tokens
    definition = [tokens.pop(0), tokens.pop(0)]
    name = definition[1].tok

    next = peek(tokens).tok
    while next != ";":
        try:
            words.append(parse_number(context))
            continue
        except ValueError:
            pass

        if next == "(":
            parse_comment(context)
        else:
            words.append(tokens.pop(0))

        next = peek(tokens).tok

    definition.extend(words)
    definition.append(tokens.pop(0))

    stmt = Word(name, definition, words)
    context.stmts.append(stmt)
    context.words[name] = stmt

def parse_comment(context):
    while context.tokens.pop(0).tok != ")":
        pass

def parse_var(context):
    tokens = context.tokens
    const = tokens.pop(0)
    name = tokens.pop(0)
    stmt = Var(name.tok, [const, name])
    context.stmts.append(stmt)
    context.words[name] = stmt

def parse_const(context):
    tokens = context.tokens
    assert len(context.stack) > 0, "expected at least one token on the stack!!"
    number = context.stack.pop()
    assert isinstance(number, Nr), "expected number!!: {0}".format(number)
    const = tokens.pop(0)
    name = tokens.pop(0)
    stmt = Const(name.tok, [number, const, name], number.val)
    context.stmts.append(stmt)
    context.words[name] = stmt

def parse_number(context):
    tokens = context.tokens
    nr = int(peek(tokens).tok, context.base)
    if nr >= -0x80000000 and nr <= 0xffffffff:
        token = tokens.pop(0)
        return Nr(token.tok, token, nr)
    else:
        raise CompileError("number isn't within range!!: {0}".format(peek(tokens)))

def parse_arith(context):
    raise CompileError("parse_arith not yet implemented")

def parse_token(context):
    tokens = context.tokens
    next = peek(tokens)
    token = next.tok

    try:
        context.stack.append(parse_number(context))
        return
    except ValueError:
        pass

    if token == 'hex':
        tokens.pop(0)
        context.base = 16
    elif token == ':':
        colon_compile(context)
    elif token == "(":
        parse_comment(context)
    # no compile time operations allowed currently
    elif token == 'variable':
        parse_var(context)
    elif token == 'constant':
        parse_const(context)
    elif token in ['+', '-', '*']:
        parse_arith(context)
    else:
        context.stack.append(tokens.pop(0))


def parse(tokens):
    """Currently parse just supports fn, variable and constant definitions."""
    context = Context()
    context.tokens = tokens

    while tokens:
        parse_token(context)

    if context.stack:
        raise CompileError("after parsing, there are still words on the stack!!:\n{0}".format(
            context.stack))

    return context

# output
name_ass_table = {
    "-": "minus"

}

def try_ass_sub(name):
    sub_list = [
        ('-', '_'),
        ('!', 'store'),
        ('@', 'fetch'),
        ('\+', 'plus'),
        ('\*', 'star'),
        ('0', 'zero'),
        ('2', 'two'),
        ('=', 'equal'),
        ('>', 'greater'),
        ('<', 'less'),
    ]

    for thing,sub in sub_list:
        name = re.sub(thing, sub, name)

    return name


def name_to_ass(name):
    try:
        return name_ass_table[name]
    except KeyError:
        return try_ass_sub(name)


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
