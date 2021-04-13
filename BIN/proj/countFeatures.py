import glob
import csv
import gzip
import os
import orjson
from matplotlib import pyplot as plt
import scipy.stats
import time
import numpy as np

def parseCellIO(cell):
    # ([16]7,15,2)
    gate_id = int (cell[1:cell.find(']')])
    input1 = int (cell[cell.find(']') + 1:cell.find(',')])
    input2 = int (cell[cell.find(',') + 1:cell.rfind(',')])

    # 16 (output of cell), 7, 15
    return gate_id, input1, input2

def jsonMetrics(json_data, file_substr):
    mae_value = json_data[file_substr][0]["mae"]
    wce_value = json_data[file_substr][0]["wce"]
    wce_percent_value = json_data[file_substr][0]["wce%"]
    area_value = json_data[file_substr][0]["pdk45_area"]
    delay_value = json_data[file_substr][0]["pdk45_delay"]
    pwr_value = json_data[file_substr][0]["pdk45_pwr"]
    levels_value = json_data[file_substr][0]["levels"]

    return mae_value, wce_value, wce_percent_value, area_value, delay_value, pwr_value, levels_value

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

def findUsedCells(cgp_gates_codes_list):
    used_cgp_gates_codes_list = []
    for cgp_gates_code in cgp_gates_codes_list:
        gate_id, input1, input2 = parseCellIO(cgp_gates_code)

        for cgp_gates_code2 in cgp_gates_codes_list:
            if (cgp_gates_code == cgp_gates_code2):
                continue
            gate_id2, input12, input22  = parseCellIO(cgp_gates_code2)


            if (input12 == gate_id or input22 == gate_id or gate_id in outputs_list):
                used_cgp_gates_codes_list.append(cgp_gates_code)
                break
    return used_cgp_gates_codes_list

def findUsedCellsTypes(used_cgp_gates_codes_list):
    # find count of used types of cells
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

    xor_cells = []
    and_cells = []

    for cgp_gates_code in used_cgp_gates_codes_list:
        used_gate_id_str = cgp_gates_code[cgp_gates_code.rfind(',')+1:len(cgp_gates_code)]
        used_gate_id = int (used_gate_id_str)

        if (used_gate_id == 0):
            ida_count += 1
        elif (used_gate_id == 1):
            inva_count += 1
        elif (used_gate_id == 2):
            and_count += 1
            and_cells.append(cgp_gates_code)
        elif (used_gate_id == 3):
            or_count += 1
        elif (used_gate_id == 4):
            xor_count += 1
            xor_cells.append(cgp_gates_code)
        elif (used_gate_id == 5):
            nand_count += 1
        elif (used_gate_id == 6):
            nor_count += 1
        elif (used_gate_id == 7):
            xnor_count += 1

    return ida_count, inva_count, and_count, or_count, xor_count, nand_count, nor_count, xnor_count, xor_cells, and_cells

def getXorXorXorCount(xor_cells):
    # count of subparts a XOR b XOR c XOR d
    xor_xor_xor_count = 0
    for xor_cell in xor_cells:
        gate_id, input1, input2 = parseCellIO(xor_cell)

        for xor_cell2 in xor_cells:
            if (xor_cell == xor_cell2):
                continue
            gate_id2, input12, input22 = parseCellIO(xor_cell2)

            if (gate_id == input12 or gate_id == input22): # a XOR b XOR c
                for xor_cell3 in xor_cells:
                    if (xor_cell2 == xor_cell3):
                        continue
                    gate_id3, input13, input23 = parseCellIO(xor_cell3)

                    if (gate_id2 == input13 or gate_id2 == input23):
                        #print(xor_cell + "->" + xor_cell2 + "->" +xor_cell3)
                        xor_xor_xor_count += 1
        #print("XOR XOR XOR count " + str(xor_xor_xor_count))
    return xor_xor_xor_count

def getAndXorAndCount(xor_cells, and_cells):
    # count of subparts (a AND b) XOR (c AND d)
    and_xor_and_count = 0
    for and_cell in and_cells:
        gate_id, input1, input2 = parseCellIO(and_cell)

        for xor_cell2 in xor_cells:
            gate_id2, input12, input22 = parseCellIO(xor_cell2)

            if (gate_id == input12 or gate_id == input22): # a AND b XOR
                for and_cell3 in and_cells:
                    if (and_cell == and_cell3):
                        continue
                    gate_id3, input13, input23 = parseCellIO(and_cell3)

                    if (gate_id3 == input12 or gate_id3 == input22):
                        #print(and_cell + "->" + xor_cell2 + "->" +and_cell3)
                        and_xor_and_count += 1
    #print("AND XOR AND count " + str(and_xor_and_count))
    return and_xor_and_count

def findO15cells(cell, o15_cells):
    gate_id, input1, input2 = parseCellIO(cell)

    inputs = [0,1,2,3,4,5,6,7] # 7 init inputs of multiplier

    if (input1 in inputs or input2 in inputs):
        o15_cells.append(cell)
        return o15_cells
    else:
        for used_cell in used_cgp_gates_codes_list:
            gate_id2, input12, input22 = parseCellIO(used_cell)

            if (gate_id2 == input1 or gate_id2 == input2):
                o15_cells.append(used_cell)
                return findO15cells(used_cell, o15_cells)

def computeO15variability(seed, o15_origin_dict, o15_cells):
    o15_variability = []
    for origin_vector, vector in zip(o15_origin_dict[seed], o15_cells):
        o15_variability.append(abs(origin_vector - vector))

    return o15_variability

# predict on some metrics from which origin is the evolved multiplier
def metricsOriginPredict(seed, vectors_origin_dict, delay_value):
    absolute_distance_dict = {}
    for key, value in vectors_origin_dict.items():
        absolute_distance = abs(value - delay_value)
        absolute_distance_dict[key] = absolute_distance

    #print(absolute_distance_dict)
    predicted_origin = min(absolute_distance_dict, key=lambda k: absolute_distance_dict[k])
    return predicted_origin



start_time = time.time()

chr_path = './cgp-approx14ep.chr/'
csv_path = './csvFiles/chrFeatures.csv'


# write head of csv file
with open(csv_path, 'w+') as csv_file:
    writer = csv.writer(csv_file)
    writer.writerow(["Name", "IDA Count", "INVA Count", "AND2 Count", "OR2 Count", "XOR2 Count", "NAND2 Count", "NOR2 Count", "XNOR2 Count", "Count of Used Cells", "Compressed Chr Size", "a XOR b XOR c XOR d Count", "(a AND b) XOR (c AND d) Count"])

csv_file.close()

o15_origin_dict = {}
vectors_origin_dict = {}
# iterate through json file with three origin multipliers
with open('origin_data.json', "rb") as json_file:
    json_data = orjson.loads(json_file.read())
    for file_substr in json_data:
        # extract metrics from json file
        mae_value, wce_value, wce_percent_value, area_value, delay_value, pwr_value, levels_value = jsonMetrics(json_data, file_substr)

        compressed_file_size = getCompressedSize(chr_path, file_substr)

        #file_substr = file.split('/')[-1] # get name of processed file
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

        # find used cells by cgp
        used_cgp_gates_codes_list = findUsedCells(cgp_gates_codes_list)

        used_cells_size = len(used_cgp_gates_codes_list)
        #print("Size of used cells: " + str(len(used_cgp_gates_codes_list)))

        o_15_output = outputs_list[0]
        for cell in used_cgp_gates_codes_list:
            gate_id, input1, input2 = parseCellIO(cell)

            if (gate_id == o_15_output):
                o15_cells = findO15cells(cell, [])
                break
        #print(o15_cells)

        xor_cells = []
        and_cells = []

        seed_value = json_data[file_substr][0]["seed"] # type of origin of multiplier
        # find count of used types of cells
        ida_count, inva_count, and_count, or_count, xor_count, nand_count, nor_count, xnor_count, xor_cells, and_cells = findUsedCellsTypes(used_cgp_gates_codes_list)

        # find count of used types of cells for O15 output and add this vector to origin dict
        ida_count_o15, inva_count_o15, and_count_o15, or_count_o15, xor_count_o15, nand_count_o15, nor_count_o15, xnor_count_o15, xor_cells_o15, and_cells_o15 = findUsedCellsTypes(o15_cells)
        o15_origin_dict[seed_value] = [ida_count_o15, inva_count_o15, and_count_o15, or_count_o15, xor_count_o15, nand_count_o15, nor_count_o15, xnor_count_o15]
        vectors_origin_dict[seed_value] = delay_value
        # count of subparts a XOR b XOR c XOR d
        xor_xor_xor_count = getXorXorXorCount(xor_cells)
        #print(xor_xor_xor_count)

        # count of subparts (a AND b) XOR (c AND d)
        and_xor_and_count = getAndXorAndCount(xor_cells, and_cells)

        # write info to csv file
        with open('./csvFiles/chrFeatures.csv', 'a', newline='') as csv_file:
            writer = csv.writer(csv_file)
            writer.writerow([file_substr, ida_count, inva_count, and_count, or_count, xor_count, nand_count, nor_count, xnor_count, used_cells_size, compressed_file_size, xor_xor_xor_count, and_xor_and_count, "ORIGIN MULTIPLIER"])

        csv_file.close()

#print(o15_origin_dict)
#print(vectors_origin_dict)


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

        #file_substr = file.split('/')[-1] # get name of processed file
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

        # find used cells by cgp
        used_cgp_gates_codes_list = findUsedCells(cgp_gates_codes_list)

        used_cells_size = len(used_cgp_gates_codes_list)
        #print("Size of used cells: " + str(len(used_cgp_gates_codes_list)))

        xor_cells = []
        and_cells = []

        seed_value = json_data[file_substr][0]["seed"] # type of origin of multiplier

        # find count of used types of cells
        ida_count, inva_count, and_count, or_count, xor_count, nand_count, nor_count, xnor_count, xor_cells, and_cells = findUsedCellsTypes(used_cgp_gates_codes_list)

        # find count of used types of cells for O15 output
        ida_count_o15, inva_count_o15, and_count_o15, or_count_o15, xor_count_o15, nand_count_o15, nor_count_o15, xnor_count_o15, xor_cells_o15, and_cells_o15 = findUsedCellsTypes(o15_cells)
        o15_variability = computeO15variability(seed_value, o15_origin_dict, [ida_count_o15, inva_count_o15, and_count_o15, or_count_o15, xor_count_o15, nand_count_o15, nor_count_o15, xnor_count_o15, xor_cells_o15, and_cells_o15])

        # predict based on some metrics origin or evolved multiplier
        predicted_origin = metricsOriginPredict(seed_value, vectors_origin_dict, delay_value)
        print(predicted_origin)
        if (predicted_origin == seed_value):
            prediction_correct_count = prediction_correct_count + 1
        else:
            prediction_wrong_count = prediction_wrong_count + 1

        # count of subparts a XOR b XOR c XOR d
        xor_xor_xor_count = getXorXorXorCount(xor_cells)
        #print(xor_xor_xor_count)

        # count of subparts (a AND b) XOR (c AND d)
        and_xor_and_count = getAndXorAndCount(xor_cells, and_cells)

        xor_values.append(xor_count)
        compressed_chr_sizes.append(compressed_file_size)
        xor_xor_xor_values.append(xor_xor_xor_count)
        and_xor_and_count_values.append(and_xor_and_count)

        # write info to csv file
        with open('./csvFiles/chrFeatures.csv', 'a', newline='') as csv_file:
            writer = csv.writer(csv_file)
            writer.writerow([file_substr, ida_count, inva_count, and_count, or_count, xor_count, nand_count, nor_count, xnor_count, used_cells_size, compressed_file_size, xor_xor_xor_count, and_xor_and_count, o15_variability])

        csv_file.close()

metrics_prediction_accuracy = ((prediction_correct_count) / (prediction_correct_count + prediction_wrong_count)) * 100
print("Accuracy of predicion based on metrics: " + str(metrics_prediction_accuracy) + " %")
# print duration of program
print("--- %s seconds ---" % (time.time() - start_time))

# show some interesting graphs and correlations of dependecies
figure = plt.figure(figsize=(30, 30))
xor_mae_plot = figure.add_subplot(2,3,1)
xor_mae_plot.scatter(xor_values, mae_values)
xor_mae_plot.set_xlabel('Count of Used XOR Gates by CGP')
xor_mae_plot.set_ylabel('Mean Absolute Error')
r1, p1 = scipy.stats.pearsonr(xor_values, mae_values)
print(r1)

xor_wce_plot = figure.add_subplot(2,3,2)
xor_wce_plot.scatter(xor_values, wce_values)
xor_wce_plot.set_xlabel('Count of Used XOR Gates by CGP')
xor_wce_plot.set_ylabel('Worst Case Error')
r2, p2 = scipy.stats.pearsonr(xor_values, wce_values)
print(r2)

compressed_mae_plot = figure.add_subplot(2,3,3)
compressed_mae_plot.scatter(compressed_chr_sizes, mae_values)
compressed_mae_plot.set_xlabel('Compressed Chr File Size')
compressed_mae_plot.set_ylabel('Mean Absolute Error')
r3, p3 = scipy.stats.pearsonr(compressed_chr_sizes, mae_values)
print(r3)

compressed_wce_plot = figure.add_subplot(2,3,4)
compressed_wce_plot.scatter(compressed_chr_sizes, wce_values)
compressed_wce_plot.set_xlabel('Compressed Chr File Size')
compressed_wce_plot.set_ylabel('Worst Case Error')
r4, p4 = scipy.stats.pearsonr(compressed_chr_sizes, wce_values)
print(r4)

xor_xor_xor_mae_percent_plot = figure.add_subplot(2,3,5)
xor_xor_xor_mae_percent_plot.scatter(xor_xor_xor_values, mae_values)
xor_xor_xor_mae_percent_plot.set_xlabel('Count of XOR XOR XOR Subparts')
xor_xor_xor_mae_percent_plot.set_ylabel('Mean Absolute Error')
r5, p5 = scipy.stats.pearsonr(xor_xor_xor_values, mae_values)
print(r5)

and_xor_and_mae_plot = figure.add_subplot(2,3,6)
and_xor_and_mae_plot.scatter(and_xor_and_count_values, mae_values)
and_xor_and_mae_plot.set_xlabel('Count of AND XOR AND Subparts')
and_xor_and_mae_plot.set_ylabel('Mean Absolute Error')
r6, p6 = scipy.stats.pearsonr(and_xor_and_count_values, mae_values)
print(r6)

figure2 = plt.figure(figsize=(30, 30))

xor_area_plot = figure2.add_subplot(2,3,1)
xor_area_plot.scatter(xor_values, area_values)
xor_area_plot.set_xlabel('Count of Used XOR Gates by CGP')
xor_area_plot.set_ylabel('pdk45_area')
r7, p7 = scipy.stats.pearsonr(xor_values, area_values)
print(r7)

xor_delay_plot = figure2.add_subplot(2,3,2)
xor_delay_plot.scatter(xor_values, delay_values)
xor_delay_plot.set_xlabel('Count of Used XOR Gates by CGP')
xor_delay_plot.set_ylabel('pdk45_delay')
r8, p8 = scipy.stats.pearsonr(xor_values, delay_values)
print(r8)

xor_pwr_plot = figure2.add_subplot(2,3,3)
xor_pwr_plot.scatter(xor_values, pwr_values)
xor_pwr_plot.set_xlabel('Count of Used XOR Gates by CGP')
xor_pwr_plot.set_ylabel('pdk45_pwr')
r9, p9 = scipy.stats.pearsonr(xor_values, pwr_values)
print(r9)

xor_xor_xor_wce_plot = figure2.add_subplot(2,3,4)
xor_xor_xor_wce_plot.scatter(xor_xor_xor_values, wce_values)
xor_xor_xor_wce_plot.set_xlabel('Count of XOR XOR XOR Subparts')
xor_xor_xor_wce_plot.set_ylabel('Worst Case Error')
r10, p10 = scipy.stats.pearsonr(xor_xor_xor_values, wce_values)
print(r10)

and_xor_and_wce_plot = figure2.add_subplot(2,3,5)
and_xor_and_wce_plot.scatter(and_xor_and_count_values, wce_values)
and_xor_and_wce_plot.set_xlabel('Count of AND XOR AND Subparts')
and_xor_and_wce_plot.set_ylabel('Worst Case Error')
r11, p11 = scipy.stats.pearsonr(and_xor_and_count_values, wce_values)
print(r11)

xor_pwr_plot = figure2.add_subplot(2,3,6)
xor_pwr_plot.scatter(xor_values, pwr_values)
xor_pwr_plot.set_xlabel('Count of Used XOR Gates by CGP')
xor_pwr_plot.set_ylabel('pdk45_pwr')
r12, p12 = scipy.stats.pearsonr(xor_values, pwr_values)
print(r12)

plt.show()

'''
-0.5810519990081127
-0.4458477189999104
0.09165341582928331
0.1067373313587608
-0.4458477189999104
0.6370072647108483
0.891686499037995
0.6637750321946084
0.9371490935825837




1.125
-0.5953690466407922
-0.4605243583056319
0.09165341582928331
0.1067373313587608
-0.4605243583056318
0.6949995992005571
0.91653132446021
0.7124773999749886
0.9628147078397827


-0.6105094444846811
-0.5192288570109109
0.5898542809715568
0.45469169242677027
-0.6052006933007329
-0.3548548845358819
0.9419721939938519
0.939126052167467
0.9760709828652058
-0.5277579870127447
-0.25326372824607946
0.9760709828652058


'''
