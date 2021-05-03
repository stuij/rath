#!/usr/bin/env python

# Very very naive forth cross-compiler, just about functional enough to make this
# workable for a simple entry for a GBA game competition.

# When I have a bit more time (HA!), I'll write a full-blown one that will
# inherently basically be a whole Forth implementation all by itself.

import argparse
import os
import re

LAST_WORD = "beany_pal"
GENSYM = 0

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
        file.write('  head {0},{1},"{2}",docolon,{3}\n'.format(
            ass_name, len(self.name), self.name, prev_word))

        if not isinstance(self.words[0], Label):
            file.write("    .word ")

        word_size = len(self.words)
        for i, word in enumerate(self.words):
            word.to_ass(file, prev_word)
            if (i < word_size - 1 and
                not isinstance(word, Label) and
                not isinstance(self.words[i+1], Label)):
                file.write(',')
            if i >= word_size - 1 and not isinstance(word, Label):
                file.write(',')
            if (i < word_size - 1 and
                isinstance(word, Label) and
                not isinstance(self.words[i+1], Label)):
                file.write("    .word ")
            if i >= word_size - 1 and isinstance(word, Label):
                file.write("    .word ")

        file.write('exit\n\n')

        return ass_name

    def print(self):
        return "const: {0} . {1} _ {2}".format(self.name, self.words, self.tokens[0].line)


class Label(Stmt):
    def __init__(self, name, token):
        global GENSYM
        label_name = name + "_" + str(GENSYM)
        GENSYM += 1
        super().__init__(label_name, [token])

    def to_ass(self, file, prev_word):
        ass_name = name_to_ass(self.name)
        file.write('\n{0}:'.format(ass_name))

    def print(self):
        return "label: {0} _ {1}".format(self.name, self.tokens[0].line)


class BranchUncond(Stmt):
    def __init__(self, name, token):
        super().__init__(name, [token])

    def to_ass(self, file, prev_word):
        ass_name = name_to_ass(self.name)
        file.write('branch,{0}'.format(ass_name))

    def print(self):
        return "branch: to {0} _ {1}".format(self.name, self.tokens[0].line)


class BranchZero(Stmt):
    def __init__(self, name, token):
        super().__init__(name, [token])

    def to_ass(self, file, prev_word):
        ass_name = name_to_ass(self.name)
        file.write('qbranch,{0}'.format(ass_name))

    def print(self):
        return "qbranch: to {0} _ {1}".format(self.name, self.tokens[0].line)

class Branch(Stmt):
    def __init__(self, name, token, branch_type):
        super().__init__(name, [token])
        self.branch_type = branch_type

    def to_ass(self, file, prev_word):
        ass_type = name_to_ass(self.branch_type)
        ass_name = name_to_ass(self.name)
        file.write('{0},{1}'.format(ass_type,ass_name))

    def print(self):
        return "{0}: to {1} _ {2}".format(self.branch_type, self.name, self.tokens[0].line)

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
            count += 1
    return tokens

def peek(tokens):
    return tokens[0]


# parse
def colon_compile(context):
    words = []
    tokens = context.tokens
    definition = [tokens.pop(0), tokens.pop(0)]
    name = definition[1].tok
    branch_stack = []
    if_stack = []

    next = peek(tokens).tok
    while next != ";":
        try:
            words.append(parse_number(context))
            next = peek(tokens).tok
            continue
        except ValueError:
            pass

        if next == "(":
            parse_comment(context)
        elif next == "begin":
            label = Label(name, tokens.pop(0))
            words.append(label)
            branch_stack.append(label)
        elif next == "until":
            assert branch_stack, "branch stack is empty!"
            label = branch_stack.pop()
            words.append(BranchZero(label.name, tokens.pop(0)))
        elif next == "if":
            not_if_branch = BranchZero(None, tokens.pop(0))
            words.append(not_if_branch)
            if_stack.append(not_if_branch)
        elif next == "else":
            # here we should end up with:
            #   end of if, so unconditional branch to then
            #   landing label of qbranch, so beginning of else
            #   then label
            # but be sure to remove else landing from stack before pushing if
            assert if_stack, "if stack is empty!"
            else_token = tokens.pop(0)
            # remove else landing
            else_branch = if_stack.pop()
            # unconditional branch
            if_branch = BranchUncond(None, else_token)
            words.append(if_branch)
            if_stack.append(if_branch)
            # landing label of else
            label = Label(name, else_token)
            else_branch.name = label.name
            words.append(label)
        elif next == "then":
            assert if_stack, "if stack is empty!"
            label = Label(name, tokens.pop(0))
            not_if_branch = if_stack.pop()
            not_if_branch.name = label.name
            words.append(label)
        elif next == "while":
            branch = BranchZero(None, tokens.pop(0))
            branch_stack.append(branch)
            words.append(branch)
        elif next == "repeat":
            while_branch = branch_stack.pop()
            begin_label = branch_stack.pop()
            repeat_token = tokens.pop(0)
            repeat_branch = BranchUncond(begin_label.name, repeat_token)
            repeat_label = Label(name, repeat_token)
            while_branch.name = repeat_label.name
            words.append(repeat_branch)
            words.append(repeat_label)
        elif next == "do":
            token = tokens.pop(0)
            words.append(token)
            label = Label(name, token)
            words.append(label)
            branch_stack.append(label)
        elif next in ["loop", "+loop"]:
            assert branch_stack, "branch stack is empty!"
            label = branch_stack.pop()
            words.append(Branch(label.name, tokens.pop(0), next))
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
    "-": "minus",
    'i': 'ii',
    'j': 'jj',
    'do': 'xdo',
    'loop': 'xloop',
    '+loop': 'xplusloop',
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
