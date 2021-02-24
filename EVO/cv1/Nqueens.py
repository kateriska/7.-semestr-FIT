#!/usr/bin/env python
# encoding: utf-8
import sys
from random import shuffle, randint, random
from math import sqrt, exp

###########################################################
## EVO -- Aplikovane evolucni algoritmy
## pracovni soubor pro cviceni c. 1
##
## autor: Martin Hyrs
## 16. 2. 2017
###########################################################





###########################################################
## zobrazeni sachovnice
def printQueens (qs):
	sys.stdout.write(u"   \u2554") #╔
	for d in range(len(qs)*3):
		sys.stdout.write(u"\u2550") #═
	sys.stdout.write(u"\u2557\n") #╗

	for row in range(len(qs)):
		sys.stdout.write(u"   \u2551") #║
		for col in range(len(qs)):
			if (row&1^col&1):
				if (col==qs[row]):
					sys.stdout.write(u"\u2591\u25CF\u2591") #░●░ #sachova dama je ♕\u2655 nebo ♛\u265B
				else:
					sys.stdout.write(u"\u2591\u2591\u2591") #░░░
			else:
				if (col==qs[row]):
					sys.stdout.write(u" \u25CF ") #●
				else:
					sys.stdout.write(u"   ")
		sys.stdout.write(u"\u2551\n") #║

	sys.stdout.write(u"   \u255A") #╚
	for d in range(len(qs)*3):
		sys.stdout.write(u"\u2550") #═
	sys.stdout.write(u"\u255D\n") #╝
###########################################################





###########################################################
###########################################################
## fitness (pocet konfliktu)
def fitness (qs):
	confs = 0
	## !!! ZDE DOPLNTE !!!
	#  (i - j) = k - l or i + j = k + l. for conflict on diagonals

	for queen1 in range(len(qs)):
		for queen2 in range(len(qs)):
			if (queen1 == queen2):
				continue
			#print row, col
			#print row - col

			if (queen1 - qs[queen1] == queen2 - qs[queen2] or queen1 + qs[queen1] == queen2 + qs[queen2]):
				confs = confs + 1

	#print "Conflicts: " + str(confs)
	return confs
###########################################################
###########################################################





###########################################################
## algoritmus
def metropolis (q, max, param):
	s = len(q)-1

	prevq = q[:]
	prevfit = fitness(q)

	for i in range(max):
		a = randint(0, s)
		b = randint(0, s)
		newq = prevq[:]
		newq[a] = prevq[b]
		newq[b] = prevq[a]

		newfit = fitness(newq)

		is_accepted = exp((prevfit-newfit)/param) > random()

		if is_accepted:
			prevq = newq[:]
			prevfit = newfit
			print >> sys.stderr, i, newfit

			if (newfit == 0):
				break

	print >> sys.stderr
	return prevq[:]
###########################################################





###########################################################
###########################################################
## main

SIZE = 8
MAX_ITER = 10000
PARAM = 0.25

q = list(range(SIZE))
print q

shuffle(q)

q = metropolis(q, MAX_ITER, PARAM)

print "Nalezene reseni, fitness =", fitness(q)
print "Genotyp"
print q
print "Fenotyp"
printQueens(q)
