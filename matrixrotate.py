#!/bin/python

from math import pi
from time import sleep
import sys
import os


matsize=8

tmatrix = [
    [' ',' ',' ',' ', 1 ,' ',' ',' '],
    [' ',' ',' ',' ', 1 , 1 ,' ',' '],
    [ 1 , 1 , 1 , 1 , 1 , 1 ,' ',' '],
    [ 1 , 1 , 1 , 1 , 1 , 1 , 1 , 1 ],
    [ 1 , 1 , 1 , 1 , 1 , 1 , 1 , 1 ],
    [ 1 , 1 , 1 , 1 , 1 , 1 ,' ',' '],
    [' ',' ',' ',' ', 1 , 1 ,' ',' '],
    [' ',' ',' ',' ', 1 ,' ',' ',' '],
]


newtmatrix = [
    [0,0,0,0,0,0,0,0],
    [0,0,0,0,0,0,0,0],
    [0,0,0,0,0,0,0,0],
    [0,0,0,0,0,0,0,0],
    [0,0,0,0,0,0,0,0],
    [0,0,0,0,0,0,0,0],
    [0,0,0,0,0,0,0,0],
    [0,0,0,0,0,0,0,0],
]

def printmatrix(mat):
    sys.stdout.write("\n")
    for i in range(0,matsize):
        for j in range(0,matsize):
            sys.stdout.write(str(mat[i][j]))
        sys.stdout.write("\n")


#copy over
def copymatrix(tmatrix, newtmatrix, matsize):
    for i in range(0, matsize):
        for j in range(0, matsize):
            tmatrix[i][j]=newtmatrix[i][j]



#rotate right
def rotateright(tmatrix, newtmatrix, matsize):
    for i in range(0, matsize):
        for j in range(0, matsize):
            newtmatrix[j][matsize-1-i]=tmatrix[i][j]

    copymatrix(tmatrix, newtmatrix, matsize)

#rotate left
def rotateleft(tmatrix, newtmatrix, matsize):
    for i in range(0, matsize):
        for j in range(0, matsize):
            newtmatrix[matsize-1-j][i]=tmatrix[i][j]

    copymatrix(tmatrix, newtmatrix, matsize)

i=0
while True:
    os.system("clear")

    i=i+1
    if i % 8 < 4:
        rotateright(tmatrix, newtmatrix, matsize)
    else:
        rotateleft(tmatrix, newtmatrix, matsize)

    printmatrix(tmatrix)



    sys.stdout.flush()
    sleep(0.5)
