# Project name: Relationship Analysis of Approximated Circuits
# Author: Katerina Fortova
# Login: xforto00
# Year: 2020 / 2021

# Description:
# python filterJson.py rcam|wtmcla|wtmcsa|csamcsa|csamrca|wtmrca|all -n|-p [number] has to be run before running this program (see Readme)!
# Extraction of features to csv file ./csvFiles/chrFeatures.csv:
# Count of Used Gates Types by CGP (IDA, INVA, AND2, OR2, XOR2, NAND2, NOR2, XNOR2)
# Count of All Used Gates by CGP
# Size of Compressed Chr File
# Count of Subparts a XOR b XOR c XOR d
# Count of Subparts (a AND b) XOR (c AND d)
# Variability of Implementation of the Highest Bit of Product ("ORIGIN MULTIPLIER is written on this field for origin multipliers")
# Feature vector - [Sum of All Gates, Sum of Used Gates by CGP]
# In ./csvFiles/allVectors.csv, ./csvFiles/allClasses.csv are written vectors and classes which could be used for classification of seed with MLP
# In the end program shows graphs and correlations based on Pearson Correlation Coefficient

# Analyse of all almost 25 000 multipliers is computationally intensive and lasts a few hours. It is recommended to analyse only some smaller subset of objects to get results in more reasonable time.
# The most computationally intensive parts are computing Variability of Implementation of the Highest Bit of Product, also we search through JSON file, where orjson lib was used instead of json lib for faster processing.

import glob
import csv
import gzip
import os
import orjson
from matplotlib import pyplot as plt
import scipy.stats
import numpy as np

# parse gate and get ids of their two inputs and one output
def parseGateIO(gate):
    # ([16]7,15,2)
    gate_id = int (gate[1:gate.find(']')])
    input1 = int (gate[gate.find(']') + 1:gate.find(',')])
    input2 = int (gate[gate.find(',') + 1:gate.rfind(',')])

    # 16 (output of gate), 7, 15
    return gate_id, input1, input2

# get interesting metrics for multiplier from json file
def jsonMetrics(json_data, file_substr):
    mae_value = json_data[file_substr][0]["mae"]
    wce_value = json_data[file_substr][0]["wce"]
    wce_percent_value = json_data[file_substr][0]["wce%"]
    area_value = json_data[file_substr][0]["pdk45_area"]
    delay_value = json_data[file_substr][0]["pdk45_delay"]
    pwr_value = json_data[file_substr][0]["pdk45_pwr"]
    levels_value = json_data[file_substr][0]["levels"]

    return mae_value, wce_value, wce_percent_value, area_value, delay_value, pwr_value, levels_value

# get size of compressed archive with chromozome of multiplier
def getCompressedSize(chr_path, file_substr):
    # open chr file of json object
    f_in = open(chr_path + file_substr + ".chr", 'rb')

    # create zip of chr file and compute its size
    f_out = gzip.open("./compressedChrFiles/" + file_substr + '.zip', 'wb')
    f_out.writelines(f_in)
    f_out.close()
    f_in.close()

    compressed_file_size = os.path.getsize("./compressedChrFiles/" + file_substr + '.zip')
    return compressed_file_size

# find list of used gates by cgp
def findUsedGates(cgp_gates_codes_list):
    used_cgp_gates_codes_list = []
    for cgp_gates_code in cgp_gates_codes_list:
        gate_id, input1, input2 = parseGateIO(cgp_gates_code)

        for cgp_gates_code2 in cgp_gates_codes_list:
            if (cgp_gates_code == cgp_gates_code2):
                continue
            gate_id2, input12, input22  = parseGateIO(cgp_gates_code2)

            if (input12 == gate_id or input22 == gate_id or gate_id in outputs_list):
                used_cgp_gates_codes_list.append(cgp_gates_code)
                break
    return used_cgp_gates_codes_list

# find types of used gates
def findUsedGatesTypes(used_cgp_gates_codes_list):
    # find count of used types of gates
    '''
    Kódy použitých hradel (mapování v CGP s 2-
    vstupovými uzly)
    0: IDA (drat)
    1: INVA (inverze prvniho stupu)
    2: AND2
    3: OR2
    4: XOR2
    5: NAND2
    6: NOR2
    7: XNOR2
    '''
    ida_count = 0
    inva_count = 0
    and_count = 0
    or_count = 0
    xor_count = 0
    nand_count = 0
    nor_count = 0
    xnor_count = 0

    xor_gates = []
    and_gates = []

    for cgp_gates_code in used_cgp_gates_codes_list:
        used_gate_id_str = cgp_gates_code[cgp_gates_code.rfind(',')+1:len(cgp_gates_code)]
        used_gate_id = int (used_gate_id_str)

        if (used_gate_id == 0):
            ida_count += 1
        elif (used_gate_id == 1):
            inva_count += 1
        elif (used_gate_id == 2):
            and_count += 1
            and_gates.append(cgp_gates_code)
        elif (used_gate_id == 3):
            or_count += 1
        elif (used_gate_id == 4):
            xor_count += 1
            xor_gates.append(cgp_gates_code)
        elif (used_gate_id == 5):
            nand_count += 1
        elif (used_gate_id == 6):
            nor_count += 1
        elif (used_gate_id == 7):
            xnor_count += 1

    return ida_count, inva_count, and_count, or_count, xor_count, nand_count, nor_count, xnor_count, xor_gates, and_gates

# count of subparts a XOR b XOR c XOR d
def getXorXorXorCount(xor_gates):
    xor_xor_xor_count = 0
    for xor_gate in xor_gates:
        gate_id, input1, input2 = parseGateIO(xor_gate)

        for xor_gate2 in xor_gates:
            if (xor_gate == xor_gate2):
                continue
            gate_id2, input12, input22 = parseGateIO(xor_gate2)

            if (gate_id == input12 or gate_id == input22): # a XOR b XOR c
                for xor_gate3 in xor_gates:
                    if (xor_gate2 == xor_gate3):
                        continue
                    gate_id3, input13, input23 = parseGateIO(xor_gate3)

                    if (gate_id2 == input13 or gate_id2 == input23):
                        #print(xor_gate + "->" + xor_gate2 + "->" +xor_gate3)
                        xor_xor_xor_count += 1
    #print("XOR XOR XOR count " + str(xor_xor_xor_count))
    return xor_xor_xor_count

# count of subparts (a AND b) XOR (c AND d)
def getAndXorAndCount(xor_gates, and_gates):
    and_xor_and_count = 0
    for and_gate in and_gates:
        gate_id, input1, input2 = parseGateIO(and_gate)

        for xor_gate2 in xor_gates:
            gate_id2, input12, input22 = parseGateIO(xor_gate2)

            if (gate_id == input12 or gate_id == input22): # a AND b XOR
                for and_gate3 in and_gates:
                    if (and_gate == and_gate3):
                        continue
                    gate_id3, input13, input23 = parseGateIO(and_gate3)

                    if (gate_id3 == input12 or gate_id3 == input22):
                        #print(and_gate + "->" + xor_gate2 + "->" +and_gate3)
                        and_xor_and_count += 1
    #print("AND XOR AND count " + str(and_xor_and_count))
    return and_xor_and_count

# count distance between parent vector of MSB output gate types and approximated multiplier
def computeO15variability(seed, o15_origin_dict, o15_gates):
    o15_variability = []
    for origin_vector, vector in zip(o15_origin_dict[seed], o15_gates):
        o15_variability.append(abs(origin_vector - vector))

    return o15_variability

# predict on some metrics from which origin is the evolved multiplier
def metricsOriginPredict(seed, vectors_origin_dict, delay_value):
    absolute_distance_dict = {}
    for key, value in vectors_origin_dict.items():
        absolute_distance = abs(value - delay_value)
        absolute_distance_dict[key] = absolute_distance

    predicted_origin = min(absolute_distance_dict, key=lambda k: absolute_distance_dict[k])
    return predicted_origin

# recursively compute for gate on which final output is connected (#%o O15,O14,O13,O12,O11,O10,O9,O8,O7,O6,O5,O4,O3,O2,O1,O0)
def computeGatesConnectedOutput(used_cgp_gates_codes_list, outputs_list, gate):
    next_gate_found = False

    gate_id, input1, input2 = parseGateIO(gate)

    for gate2 in used_cgp_gates_codes_list:
        if (gate == gate2):
            continue
        gate_id2, input12, input22 = parseGateIO(gate2)

        if (gate_id == input12 or gate_id == input22):
            next_gate_found = True
            return computeGatesConnectedOutput(used_cgp_gates_codes_list, outputs_list, gate2)
    if (next_gate_found == False):
        if gate_id in outputs_list:
            return gate_id

# write interesting vectors for csv file as train data for classifier of origin of multiplier
def writeVectors(f, g, seed_value, gates_counts):
    if (seed_value == "rcam"):
        class_id = 0
    elif (seed_value == "wtm_cla"):
        class_id = 1
    elif (seed_value == "csam_rca"):
        class_id = 2
    elif (seed_value == "csam_csa"):
        class_id = 3
    elif (seed_value == "wtm_csa"):
        class_id = 4
    elif (seed_value == "wtm_rca"):
        class_id = 5

    #print(gates_counts)
    with open(f, 'a') as csv_file:
        writer = csv.writer(csv_file)
        writer.writerow(gates_counts)
    csv_file.close()

    with open(g, 'a') as csv_file:
        writer = csv.writer(csv_file)
        writer.writerow(str(class_id))
    csv_file.close()



chr_path = './cgp-approx14ep.chr/'
chr_origin_path = './rodicovske8b_nasobicky/'
csv_path = './csvFiles/chrFeatures.csv'
f = "./csvFiles/allVectors.csv"
g = "./csvFiles/allClasses.csv"


# write head of csv files
with open(csv_path, 'w+') as csv_file:
    writer = csv.writer(csv_file)
    writer.writerow(["Name", "IDA Count", "INVA Count", "AND2 Count", "OR2 Count", "XOR2 Count", "NAND2 Count", "NOR2 Count", "XNOR2 Count", "Count of Used Gates", "Compressed Chr Size", "a XOR b XOR c XOR d Count", "(a AND b) XOR (c AND d) Count", "Variability of Implementation of the Highest Bit of Product", "[Sum of All Gates, Sum of Used Gates by CGP]"])

csv_file.close()

with open(f, 'w+') as csv_file:
    writer = csv.writer(csv_file)
    writer.writerow(["IDA Count", "INVA Count", "AND2 Count", "OR2 Count", "XOR2 Count", "NAND2 Count", "NOR2 Count", "XNOR2 Count"])

csv_file.close()

with open(g, 'w+') as csv_file:
    writer = csv.writer(csv_file)
    writer.writerow(["Class of Multiplier Origin"])

csv_file.close()

#################### PARENT MULTIPLIERS ########################################
o15_origin_dict = {} # dictionary for saving count of gates types which are used for computation of highest bit of the product for origin (seed) multipliers

print("PARENT (SEED) MULTIPLIERS:")
# iterate through json file with three origin multipliers which are in original json
with open('origin_data.json', "rb") as json_file:
    json_data = orjson.loads(json_file.read())
    for file_substr in json_data:
        # extract metrics from json file
        mae_value, wce_value, wce_percent_value, area_value, delay_value, pwr_value, levels_value = jsonMetrics(json_data, file_substr)

        compressed_file_size = getCompressedSize(chr_path, file_substr)
        print(file_substr)

        # open chr file for computing features based on chr file
        readed_file = open(chr_path + file_substr + ".chr", 'r')
        lines = readed_file.readlines()

        count = 0
        # Strips the newline character
        for line in lines:
            count += 1
            if (line[0:1] == "#"):
                continue
            cgp_gates_codes = line[line.find('}') + 2:len(line)]
            outputs = (cgp_gates_codes[cgp_gates_codes.rfind('(') + 1:-1]).split(',')
            outputs_list = list (map(int, outputs))
            cgp_gates_codes_list = cgp_gates_codes.split(')(')
            cgp_gates_codes_list = cgp_gates_codes_list[:-1]

        all_gates_size = len(cgp_gates_codes_list)
        # find used gates by cgp
        used_cgp_gates_codes_list = findUsedGates(cgp_gates_codes_list)
        used_gates_size = len(used_cgp_gates_codes_list)

        # get characteristic vector of implementation - [count of all gates, count of used gates by CGP]
        all_used_gates_vector = []
        all_used_gates_vector.append(all_gates_size)
        all_used_gates_vector.append(used_gates_size)

        xor_gates = []
        and_gates = []

        seed_value = json_data[file_substr][0]["seed"] # type of origin of multiplier
        # find count of used types of gates
        ida_count, inva_count, and_count, or_count, xor_count, nand_count, nor_count, xnor_count, xor_gates, and_gates = findUsedGatesTypes(used_cgp_gates_codes_list)

        # compute output (O0 - O15) of all gates
        o15_gates = []
        o15_output = outputs_list[0]

        for gate in used_cgp_gates_codes_list:
            gate_connected_output = computeGatesConnectedOutput(used_cgp_gates_codes_list, outputs_list, gate)
            if (gate_connected_output == o15_output):
                o15_gates.append(gate)

        # find count of used types of gates for O15 output and add this vector to origin dict
        ida_count_o15, inva_count_o15, and_count_o15, or_count_o15, xor_count_o15, nand_count_o15, nor_count_o15, xnor_count_o15, xor_gates_o15, and_gates_o15 = findUsedGatesTypes(o15_gates)
        o15_origin_dict[seed_value] = [ida_count_o15, inva_count_o15, and_count_o15, or_count_o15, xor_count_o15, nand_count_o15, nor_count_o15, xnor_count_o15]

        # count of subparts a XOR b XOR c XOR d
        xor_xor_xor_count = getXorXorXorCount(xor_gates)

        # count of subparts (a AND b) XOR (c AND d)
        and_xor_and_count = getAndXorAndCount(xor_gates, and_gates)

        # write info to csv file
        with open('./csvFiles/chrFeatures.csv', 'a', newline='') as csv_file:
            writer = csv.writer(csv_file)
            writer.writerow([file_substr, ida_count, inva_count, and_count, or_count, xor_count, nand_count, nor_count, xnor_count, used_gates_size, compressed_file_size, xor_xor_xor_count, and_xor_and_count, "ORIGIN MULTIPLIER", all_used_gates_vector])

        csv_file.close()


for file in glob.glob("./rodicovske8b_nasobicky/*"):
    file_substr = file.split('/')[-1] # get name of processed file
    file_substr_without_chr = file_substr[0:file_substr.rfind('.')]
    compressed_file_size = getCompressedSize(chr_origin_path, file_substr_without_chr)
    print(file_substr)

    # open chr file for computing features based on chr file
    readed_file = open(file, 'r')
    lines = readed_file.readlines()

    count = 0
    # Strips the newline character
    for line in lines:
        count += 1
        if (line[0:1] == "#"):
            continue
        cgp_gates_codes = line[line.find('}') + 2:len(line)]
        outputs = (cgp_gates_codes[cgp_gates_codes.rfind('(') + 1:-1]).split(',')
        outputs_list = list (map(int, outputs))
        cgp_gates_codes_list = cgp_gates_codes.split(')(')
        cgp_gates_codes_list = cgp_gates_codes_list[:-1]

    all_gates_size = len(cgp_gates_codes_list)
    # find used gates by cgp
    used_cgp_gates_codes_list = findUsedGates(cgp_gates_codes_list)

    used_gates_size = len(used_cgp_gates_codes_list)
    all_used_gates_vector = []
    all_used_gates_vector.append(all_gates_size)
    all_used_gates_vector.append(used_gates_size)

    xor_gates = []
    and_gates = []

    seed_value = file_substr[2:file_substr.find('-')]
    # find count of used types of gates
    ida_count, inva_count, and_count, or_count, xor_count, nand_count, nor_count, xnor_count, xor_gates, and_gates = findUsedGatesTypes(used_cgp_gates_codes_list)

    o15_gates = []
    o15_output = outputs_list[0]

    for gate in used_cgp_gates_codes_list:
        gate_connected_output = computeGatesConnectedOutput(used_cgp_gates_codes_list, outputs_list, gate)

        if (gate_connected_output == o15_output):
            o15_gates.append(gate)

    # find count of used types of gates for O15 output and add this vector to origin dict
    ida_count_o15, inva_count_o15, and_count_o15, or_count_o15, xor_count_o15, nand_count_o15, nor_count_o15, xnor_count_o15, xor_gates_o15, and_gates_o15 = findUsedGatesTypes(o15_gates)
    o15_origin_dict[seed_value] = [ida_count_o15, inva_count_o15, and_count_o15, or_count_o15, xor_count_o15, nand_count_o15, nor_count_o15, xnor_count_o15]

    # count of subparts a XOR b XOR c XOR d
    xor_xor_xor_count = getXorXorXorCount(xor_gates)

    # count of subparts (a AND b) XOR (c AND d)
    and_xor_and_count = getAndXorAndCount(xor_gates, and_gates)

    # write info to csv file
    with open('./csvFiles/chrFeatures.csv', 'a', newline='') as csv_file:
        writer = csv.writer(csv_file)
        writer.writerow([file_substr, ida_count, inva_count, and_count, or_count, xor_count, nand_count, nor_count, xnor_count, used_gates_size, compressed_file_size, xor_xor_xor_count, and_xor_and_count, "ORIGIN MULTIPLIER", all_used_gates_vector])

    csv_file.close()

print("Gates Types for Computing Highest Bit of Product for Seed Multipliers:")
print(o15_origin_dict)

#################### APROXIMATED EVOLVED MULTIPLIERS ########################################
print("APPROXIMATED (EVOLVED) MULTIPLIERS:")
xor_values = []
mae_values = []
wce_values = []
compressed_chr_sizes = []
wce_percent_values = []
levels_values = []
area_values = []
delay_values = []
pwr_values = []
xor_xor_xor_values = []
and_xor_and_count_values = []

prediction_correct_count = 0
prediction_wrong_count = 0
# iterate through json file with evolved multipliers
with open('filtered_data.json', "rb") as json_file:
    json_data = orjson.loads(json_file.read())
    for file_substr in json_data:
        # extract metrics from json file
        mae_value, wce_value, wce_percent_value, area_value, delay_value, pwr_value, levels_value = jsonMetrics(json_data, file_substr)
        mae_values.append(mae_value)
        wce_values.append(wce_value)
        wce_percent_values.append(wce_percent_value)
        area_values.append(area_value)
        delay_values.append(delay_value)
        pwr_values.append(pwr_value)
        levels_values.append(levels_value)

        compressed_file_size = getCompressedSize(chr_path, file_substr)

        print(file_substr)

        # open chr file for computing features based on chr file
        readed_file = open(chr_path + file_substr + ".chr", 'r')
        lines = readed_file.readlines()

        count = 0
        # Strips the newline character
        for line in lines:
            count += 1
            if (line[0:1] == "#"):
                continue
            cgp_gates_codes = line[line.find('}') + 2:len(line)]
            outputs = (cgp_gates_codes[cgp_gates_codes.rfind('(') + 1:-1]).split(',')
            outputs_list = list (map(int, outputs))
            cgp_gates_codes_list = cgp_gates_codes.split(')(')
            cgp_gates_codes_list = cgp_gates_codes_list[:-1]

        all_gates_size = len(cgp_gates_codes_list)
        # find used gates by cgp
        used_cgp_gates_codes_list = findUsedGates(cgp_gates_codes_list)

        used_gates_size = len(used_cgp_gates_codes_list)
        all_used_gates_vector = []
        all_used_gates_vector.append(all_gates_size)
        all_used_gates_vector.append(used_gates_size)

        xor_gates = []
        and_gates = []

        seed_value = json_data[file_substr][0]["seed"] # type of origin of multiplier

        # find count of used types of gates
        ida_count, inva_count, and_count, or_count, xor_count, nand_count, nor_count, xnor_count, xor_gates, and_gates = findUsedGatesTypes(used_cgp_gates_codes_list)

        o15_gates = []
        o15_output = outputs_list[0]

        for gate in used_cgp_gates_codes_list:
            gate_connected_output = computeGatesConnectedOutput(used_cgp_gates_codes_list, outputs_list, gate)

            if (gate_connected_output == o15_output):
                o15_gates.append(gate)

        # find count of used types of gates for O15 output
        ida_count_o15, inva_count_o15, and_count_o15, or_count_o15, xor_count_o15, nand_count_o15, nor_count_o15, xnor_count_o15, xor_gates_o15, and_gates_o15 = findUsedGatesTypes(o15_gates)
        o15_variability = computeO15variability(seed_value, o15_origin_dict, [ida_count_o15, inva_count_o15, and_count_o15, or_count_o15, xor_count_o15, nand_count_o15, nor_count_o15, xnor_count_o15, xor_gates_o15, and_gates_o15])

        # count of subparts a XOR b XOR c XOR d
        xor_xor_xor_count = getXorXorXorCount(xor_gates)

        # count of subparts (a AND b) XOR (c AND d)
        and_xor_and_count = getAndXorAndCount(xor_gates, and_gates)

        xor_values.append(xor_count)
        compressed_chr_sizes.append(compressed_file_size)
        xor_xor_xor_values.append(xor_xor_xor_count)
        and_xor_and_count_values.append(and_xor_and_count)

        writeVectors(f,g,seed_value,o15_variability) # write vectors and classes for prediction with MLP of seed of approximated multiplier

        # write info to csv file
        with open('./csvFiles/chrFeatures.csv', 'a', newline='') as csv_file:
            writer = csv.writer(csv_file)
            writer.writerow([file_substr, ida_count, inva_count, and_count, or_count, xor_count, nand_count, nor_count, xnor_count, used_gates_size, compressed_file_size, xor_xor_xor_count, and_xor_and_count, o15_variability, all_used_gates_vector])

        csv_file.close()



############### PLOTING OF INTERESTING GRAPHS AND CORRELATIONS BETWEEN DATA ######################################
# show some interesting graphs and correlations of dependecies

############################ FIGURE 1 ################################################################################
figure = plt.figure(figsize=(30, 30))

r1, p1 = scipy.stats.pearsonr(xor_values, mae_values)
#print(r1)
xor_mae_plot = figure.add_subplot(2,3,1)
xor_mae_plot.scatter(xor_values, mae_values)
xor_mae_plot.set_title("Pearsonr: " + str(r1), fontsize=8)
xor_mae_plot.set_xlabel('Count of Used XOR Gates by CGP')
xor_mae_plot.set_ylabel('Mean Absolute Error')

r2, p2 = scipy.stats.pearsonr(xor_values, wce_values)
#print(r2)
xor_wce_plot = figure.add_subplot(2,3,2)
xor_wce_plot.scatter(xor_values, wce_values)
xor_wce_plot.set_title("Pearsonr: " + str(r2), fontsize=8)
xor_wce_plot.set_xlabel('Count of Used XOR Gates by CGP')
xor_wce_plot.set_ylabel('Worst Case Error')

r3, p3 = scipy.stats.pearsonr(compressed_chr_sizes, mae_values)
#print(r3)
compressed_mae_plot = figure.add_subplot(2,3,3)
compressed_mae_plot.scatter(compressed_chr_sizes, mae_values)
compressed_mae_plot.set_title("Pearsonr: " + str(r3), fontsize=8)
compressed_mae_plot.set_xlabel('Compressed Chr File Size')
compressed_mae_plot.set_ylabel('Mean Absolute Error')

r4, p4 = scipy.stats.pearsonr(compressed_chr_sizes, wce_values)
#print(r4)
compressed_wce_plot = figure.add_subplot(2,3,4)
compressed_wce_plot.scatter(compressed_chr_sizes, wce_values)
compressed_wce_plot.set_title("Pearsonr: " + str(r4), fontsize=8)
compressed_wce_plot.set_xlabel('Compressed Chr File Size')
compressed_wce_plot.set_ylabel('Worst Case Error')

r5, p5 = scipy.stats.pearsonr(xor_xor_xor_values, mae_values)
#print(r5)
xor_xor_xor_mae_percent_plot = figure.add_subplot(2,3,5)
xor_xor_xor_mae_percent_plot.scatter(xor_xor_xor_values, mae_values)
xor_xor_xor_mae_percent_plot.set_title("Pearsonr: " + str(r5), fontsize=8)
xor_xor_xor_mae_percent_plot.set_xlabel('Count of XOR XOR XOR Subparts')
xor_xor_xor_mae_percent_plot.set_ylabel('Mean Absolute Error')

r6, p6 = scipy.stats.pearsonr(and_xor_and_count_values, mae_values)
#print(r6)
and_xor_and_mae_plot = figure.add_subplot(2,3,6)
and_xor_and_mae_plot.scatter(and_xor_and_count_values, mae_values)
and_xor_and_mae_plot.set_title("Pearsonr: " + str(r6), fontsize=8)
and_xor_and_mae_plot.set_xlabel('Count of AND XOR AND Subparts')
and_xor_and_mae_plot.set_ylabel('Mean Absolute Error')

############################ FIGURE 2 ################################################################################

figure2 = plt.figure(figsize=(30, 30))

r7, p7 = scipy.stats.pearsonr(xor_values, area_values)
#print(r7)
xor_area_plot = figure2.add_subplot(2,3,1)
xor_area_plot.scatter(xor_values, area_values)
xor_area_plot.set_title("Pearsonr: " + str(r7), fontsize=8)
xor_area_plot.set_xlabel('Count of Used XOR Gates by CGP')
xor_area_plot.set_ylabel('pdk45_area')

r8, p8 = scipy.stats.pearsonr(xor_values, delay_values)
#print(r8)
xor_delay_plot = figure2.add_subplot(2,3,2)
xor_delay_plot.scatter(xor_values, delay_values)
xor_delay_plot.set_title("Pearsonr: " + str(r8), fontsize=8)
xor_delay_plot.set_xlabel('Count of Used XOR Gates by CGP')
xor_delay_plot.set_ylabel('pdk45_delay')

r9, p9 = scipy.stats.pearsonr(xor_values, pwr_values)
#print(r9)
xor_pwr_plot = figure2.add_subplot(2,3,3)
xor_pwr_plot.scatter(xor_values, pwr_values)
xor_pwr_plot.set_title("Pearsonr: " + str(r9), fontsize=8)
xor_pwr_plot.set_xlabel('Count of Used XOR Gates by CGP')
xor_pwr_plot.set_ylabel('pdk45_pwr')

r10, p10 = scipy.stats.pearsonr(xor_xor_xor_values, wce_values)
#print(r10)
xor_xor_xor_wce_plot = figure2.add_subplot(2,3,4)
xor_xor_xor_wce_plot.scatter(xor_xor_xor_values, wce_values)
xor_xor_xor_wce_plot.set_title("Pearsonr: " + str(r10), fontsize=8)
xor_xor_xor_wce_plot.set_xlabel('Count of XOR XOR XOR Subparts')
xor_xor_xor_wce_plot.set_ylabel('Worst Case Error')

r11, p11 = scipy.stats.pearsonr(and_xor_and_count_values, wce_values)
#print(r11)
and_xor_and_wce_plot = figure2.add_subplot(2,3,5)
and_xor_and_wce_plot.scatter(and_xor_and_count_values, wce_values)
and_xor_and_wce_plot.set_title("Pearsonr: " + str(r11), fontsize=8)
and_xor_and_wce_plot.set_xlabel('Count of AND XOR AND Subparts')
and_xor_and_wce_plot.set_ylabel('Worst Case Error')

r12, p12 = scipy.stats.pearsonr(xor_values, levels_values)
#print(r12)
xor_levels_plot = figure2.add_subplot(2,3,6)
xor_levels_plot.scatter(xor_values, levels_values)
xor_levels_plot.set_title("Pearsonr: " + str(r12), fontsize=8)
xor_levels_plot.set_xlabel('Count of Used XOR Gates by CGP')
xor_levels_plot.set_ylabel('Levels')
r12, p12 = scipy.stats.pearsonr(xor_values, levels_values)

plt.show()
