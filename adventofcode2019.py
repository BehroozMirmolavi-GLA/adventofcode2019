#!/usr/bin/env python
# coding: utf-8

import pandas
import math
import copy
import shapely
from shapely.geometry import Point, LineString

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


# In[2]:


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

#Day 3 part 1
data = pandas.read_clipboard(header = None,sep = ',').values.tolist()

#Need to make vector locations function
def instruct_to_vector(instructions = list):
    vectors = [(0,0)]
    movements = []
    directions = []
    for i in range(len(instructions)):
        direction = instructions[i][0]
        directions.append(direction)
        movement = int(''.join((x for x in instructions[i] if x.isdigit())))
        movements.append(movement)
        origin = vectors[i]
        if direction == "R":
            destination = (movement,0)
            vectors.append(tuple(x+y for x, y in zip(destination, origin)))
        elif direction == "L":
            destination = (-movement,0)
            vectors.append(tuple(x+y for x, y in zip(destination, origin)))
        elif direction == "U":
            destination = (0,movement)
            vectors.append(tuple(x+y for x, y in zip(destination, origin)))
        elif direction == "D":
            destination = (0,-movement)
            vectors.append(tuple(x+y for x, y in zip(destination, origin)))
    return vectors, directions, movements


a,adir,amov = instruct_to_vector(data[0])
b,bdir,bmov = instruct_to_vector(data[1])
#possibilities - run through each line of a against each line of b and find the intersection with the least distance that isn't 0
distance = math.inf
for i in range(1,len(a)-1,1):
        for j in range(len(b)-1):
            line1 = LineString([a[i], a[i+1]])
            line2 = LineString([b[j], b[j+1]])
            
            if line1.intersects(line2): 
                int_pt = line1.intersection(line2)
                dist = abs(0-int_pt.x)+abs(0-int_pt.y)
                if dist < distance:
                    success1 = line1
                    success2 = line2
                    distance = copy.deepcopy(dist)
print(distance)

#part 2
#This time distance is # of steps from the origin
distance = math.inf
for i in range(1,len(a)-1,1):
        for j in range(len(b)-1):
            line1 = LineString([a[i], a[i+1]])
            line2 = LineString([b[j], b[j+1]])
            
            if line1.intersects(line2): 
                int_pt = line1.intersection(line2)
                xy = tuple(int_pt.coords)[0]
                dist = sum(amov[0:i])+                sum(bmov[0:j])+                abs(sum(tuple(p-q for p, q in zip(xy, a[i]))))+                abs(sum(tuple(p-q for p, q in zip(xy, b[j]))))              
                if dist < distance:
                    success1 = line1
                    success2 = line2
                    distance = copy.deepcopy(dist)
print(distance)

#day 4 part 1
#147981-691423
#The password is a six-digit number.
#The value is within the range given in your puzzle input.
#Two adjacent digits are the same (like 22 in 122345).
#Going from left to right, the digits never decrease; they only ever increase or stay the same (like 111123 or 135679).
def digitincrease(i):
    success = False
    if (str(i)[0] <= str(i)[1] and str(i)[1] <= str(i)[2] and str(i)[2] <= str(i)[3] and str(i)[3] <= str(i)[4] and str(i)[4] <= str(i)[5]):
        success =  True
    return success

def digitdouble(i):
    success = False
    if (str(i)[0] == str(i)[1] or str(i)[1] == str(i)[2] or str(i)[2] == str(i)[3] or str(i)[3] == str(i)[4] or str(i)[4] == str(i)[5]):
        success =  True
    return success

len([num for num in range(147981, 691423, 1) if digitincrease(num) and digitdouble(num)])


def digitgroup(num):
    number_str = str(num)
    last_digit = number_str[0]
    group_len = 1
    for digit in number_str[1:]:
        if digit == last_digit:
            group_len += 1
        else:
            if group_len == 2:
                return True
            last_digit = digit
            group_len = 1
    return group_len == 2

len([num for num in range(147981, 691423, 1) if digitincrease(num) and digitgroup(num)])

#day 5
df = pandas.read_clipboard(header = None,sep = ',').values.tolist()

def interpret_instruction(instruction):
    number_str = str(instruction)
    number_str = number_str.zfill(5)
    op_code = number_str[3] + number_str[4]
    param_a = number_str[2]
    param_b = number_str[1]
    param_c = number_str[0]
    return op_code, param_a, param_b,param_c

def eval_mode(parameter, program, position):
    if parameter == "0":
        dest = program[position+1]+1
        return program[dest]
    elif parameter == "1":
        dest = position+1
        return program[dest]

def int_code_program(program_input,input,position): 
  program = copy.deepcopy(program_input)
  output = []
  n_output = 0
  input = 0
  position = 0
  while position < len(program):
        instructionOp_code,instructionParam_a,instructionParam_b,instructionParam_c = interpret_instruction(program[position])
        if instructionOp_code == "01" :
            destination = program[position+3]
            program[destination] = eval_mode(instructionParam_a,program, position) + eval_mode(instructionParam_b,program, position+1)
            position = position + 4
        elif instructionOp_code == "02" :
            destination = program[position+3]
            program[destination] = eval_mode(instructionParam_a,program, position) * eval_mode(instructionParam_b,program, position+1)
            position = position + 4
            
        elif instructionOp_code == "03" : 
            destination = program[position+1]
            program[destination] = input
            position = position + 2
            
        elif instructionOp_code == "04" :
            if instructionParam_a == "0":
                output.append(program[program[pc + 1]])
            else: 
                output.append(program[pc + 1])
            n_output = n_output + 1
            position = position + 2
            
        elif instructionOp_code == "99" :
            print(output)
            
  return program,output

#day 6
df = pandas.read_clipboard(header = None,sep = ')').values
dataset = pandas.DataFrame({'body': df[:, 0], 'orbiter': df[:, 1]})

output = []
for i in range(0,len(dataset),1):
    b = dataset.body[i]
    find = True
    n = 0
    while(find):
        nextorbit = dataset.loc[dataset.orbiter == b, ]
        if nextorbit.empty:
            output.append(n)
            find = False 
        else:
            b = nextorbit.iloc[0,0]
            n += 1
output.sum()           
    
sum(i for i in output) + 1013




