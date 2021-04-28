import numpy as np

vectors = np.genfromtxt('./csvFiles/allVectorsSixClasses.csv',delimiter=",", dtype=int, skip_header=1)
targets = np.genfromtxt('./csvFiles/allClassesSixClasses.csv',dtype=int, skip_header=1)

class_0_count = 0
class_1_count = 0
class_2_count = 0
class_3_count = 0
class_4_count = 0
class_5_count = 0

ida_count_0 = 0
ida_count_1 = 0
ida_count_2 = 0
ida_count_3 = 0
ida_count_4 = 0
ida_count_5 = 0

inva_count_0 = 0
inva_count_1 = 0
inva_count_2 = 0
inva_count_3 = 0
inva_count_4 = 0
inva_count_5 = 0

and_count_0 = 0
and_count_1 = 0
and_count_2 = 0
and_count_3 = 0
and_count_4 = 0
and_count_5 = 0

or_count_0 = 0
or_count_1 = 0
or_count_2 = 0
or_count_3 = 0
or_count_4 = 0
or_count_5 = 0

xor_count_0 = 0
xor_count_1 = 0
xor_count_2 = 0
xor_count_3 = 0
xor_count_4 = 0
xor_count_5 = 0

nand_count_0 = 0
nand_count_1 = 0
nand_count_2 = 0
nand_count_3 = 0
nand_count_4 = 0
nand_count_5 = 0

nor_count_0 = 0
nor_count_1 = 0
nor_count_2 = 0
nor_count_3 = 0
nor_count_4 = 0
nor_count_5 = 0

xnor_count_0 = 0
xnor_count_1 = 0
xnor_count_2 = 0
xnor_count_3 = 0
xnor_count_4 = 0
xnor_count_5 = 0

for vector, target in zip(vectors, targets):
    if (target == 0):
        class_0_count += 1

        ida_count_0 += vector[0]
        inva_count_0 += vector[1]
        and_count_0 += vector[2]
        or_count_0 += vector[3]
        xor_count_0 += vector[4]
        nand_count_0 += vector[5]
        nor_count_0 += vector[6]
        xnor_count_0 += vector[7]
    elif (target == 1):
        class_1_count += 1

        ida_count_1 += vector[0]
        inva_count_1 += vector[1]
        and_count_1 += vector[2]
        or_count_1 += vector[3]
        xor_count_1 += vector[4]
        nand_count_1 += vector[5]
        nor_count_1 += vector[6]
        xnor_count_1 += vector[7]
    elif (target == 2):
        class_2_count += 1

        ida_count_2 += vector[0]
        inva_count_2 += vector[1]
        and_count_2 += vector[2]
        or_count_2 += vector[3]
        xor_count_2 += vector[4]
        nand_count_2 += vector[5]
        nor_count_2 += vector[6]
        xnor_count_2 += vector[7]
    if (target == 3):
        class_3_count += 1

        ida_count_3 += vector[0]
        inva_count_3 += vector[1]
        and_count_3 += vector[2]
        or_count_3 += vector[3]
        xor_count_3 += vector[4]
        nand_count_3 += vector[5]
        nor_count_3 += vector[6]
        xnor_count_3 += vector[7]
    elif (target == 4):
        class_4_count += 1

        ida_count_4 += vector[0]
        inva_count_4 += vector[1]
        and_count_4 += vector[2]
        or_count_4 += vector[3]
        xor_count_4 += vector[4]
        nand_count_4 += vector[5]
        nor_count_4 += vector[6]
        xnor_count_4 += vector[7]
    elif (target == 5):
        class_5_count += 1

        ida_count_5 += vector[0]
        inva_count_5 += vector[1]
        and_count_5 += vector[2]
        or_count_5 += vector[3]
        xor_count_5 += vector[4]
        nand_count_5 += vector[5]
        nor_count_5 += vector[6]
        xnor_count_5 += vector[7]

print(str(ida_count_0 / class_0_count) + ", " + str(inva_count_0 / class_0_count) + ", " + str(and_count_0 / class_0_count) + ", " + str(or_count_0 / class_0_count) + ", " + str(xor_count_0 / class_0_count) + ", " + str(nand_count_0 / class_0_count) + ", " + str(nor_count_0 / class_0_count) + ", " + str(xnor_count_0 / class_0_count))
print(str(ida_count_1 / class_1_count) + ", " + str(inva_count_1 / class_1_count) + ", " + str(and_count_1 / class_1_count) + ", " + str(or_count_1 / class_1_count) + ", " + str(xor_count_1 / class_1_count) + ", " + str(nand_count_1 / class_1_count) + ", " + str(nor_count_1 / class_1_count) + ", " + str(xnor_count_1 / class_1_count))
print(str(ida_count_2 / class_2_count) + ", " + str(inva_count_2 / class_2_count) + ", " + str(and_count_2 / class_2_count) + ", " + str(or_count_2 / class_2_count) + ", " + str(xor_count_2 / class_2_count) + ", " + str(nand_count_2 / class_2_count) + ", " + str(nor_count_2 / class_2_count) + ", " + str(xnor_count_2 / class_2_count))
print(str(ida_count_3 / class_3_count) + ", " + str(inva_count_3 / class_3_count) + ", " + str(and_count_3 / class_3_count) + ", " + str(or_count_3 / class_3_count) + ", " + str(xor_count_3 / class_3_count) + ", " + str(nand_count_3 / class_3_count) + ", " + str(nor_count_3 / class_3_count) + ", " + str(xnor_count_3 / class_3_count))
print(str(ida_count_4 / class_4_count) + ", " + str(inva_count_4 / class_4_count) + ", " + str(and_count_4 / class_4_count) + ", " + str(or_count_4 / class_4_count) + ", " + str(xor_count_4 / class_4_count) + ", " + str(nand_count_4 / class_4_count) + ", " + str(nor_count_4 / class_4_count) + ", " + str(xnor_count_4 / class_4_count))
print(str(ida_count_5 / class_5_count) + ", " + str(inva_count_5 / class_5_count) + ", " + str(and_count_5 / class_5_count) + ", " + str(or_count_5 / class_5_count) + ", " + str(xor_count_5 / class_5_count) + ", " + str(nand_count_5 / class_5_count) + ", " + str(nor_count_5 / class_5_count) + ", " + str(xnor_count_5 / class_5_count))
