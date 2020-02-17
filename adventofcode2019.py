#!/usr/bin/env python
# coding: utf-8

import pandas
import math

#Day 1 part 1
data = pandas.read_clipboard(header=None)

data.columns = ['V1']

def fuel(data):   
   fuel = data.V1.apply(lambda x: math.floor(x/3)-2).sum()
   print(fuel)
   return;
fuel(data)

#Day 1 part 2

def fuelforfuel(data):
    y = 0
    x = data
    while x > 0:
        x = max(math.floor(x / 3) - 2, 0)
        if x <= 0:
            break
        y = x + y
    return y

data.V1.apply(fuelforfuel).sum()

#Day 2 part 1
data = pandas.read_clipboard(header=None,sep = ',', squeeze = True).values.tolist()[0]

def int_code_program(program_input,noun,verb):
    program = copy.deepcopy(program_input)
    program[1] = noun
    program[2] = verb
    pc = 0
    while pc < len(program):
        opcode = program[pc]
        opcode1 = program[program[pc + 1]]
        opcode2 = program[program[pc + 2]]
        destination = program[pc+3]
        if opcode == 1:
            program[destination] = opcode1 + opcode2
            pc += 4
        elif opcode == 2:
            program[destination] = opcode1 * opcode2
            pc += 4
        elif opcode == 99:
            break
        else:
            print('unknown opcode')
            break
    return program[0]


int_code_program(data,12,2)

#Day 2 part 2
#find a noun verb combination that outputs 19690720
i = 0
possibilities = [(x, y) for x in range(99) for y in range(99)]
while int_code_program(data,n,v) != 19690720:
        i += 1
        n = possibilities[i][0]
        v = possibilities[i][1]




