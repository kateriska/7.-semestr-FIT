import glob
import csv
import gzip
import os
import orjson
from matplotlib import pyplot as plt

chr_path = './cgp-approx14ep.chr/*'
csv_path = './csvFiles/chrFeatures.csv'
'''
for file in glob.glob(chr_path):
    f_in = open(file, 'rb')
    file_substr = file.split('/')[-1] # get name of processed file
    f_out = gzip.open("./compressedChrFiles/" + file_substr + '.zip', 'wb')
    f_out.writelines(f_in)
    f_out.close()
    f_in.close()
'''

xor_values = []
mae_values = []
wce_values = []
compressed_chr_sizes = []

with open(csv_path, 'w+') as csv_file:
    writer = csv.writer(csv_file)
    writer.writerow(["Name", "IDA Count", "INVA Count", "AND2 Count", "OR2 Count", "XOR2 Count", "NAND2 Count", "NOR2 Count", "XNOR2 Count", "Compressed Chr Size"])

csv_file.close()

for file in glob.glob(chr_path):
    f_in = open(file, 'rb')
    file_substr = file.split('/')[-1] # get name of processed file
    file_substr = file_substr[:-4] # cut .char
    f_out = gzip.open("./compressedChrFiles/" + file_substr + '.zip', 'wb')
    f_out.writelines(f_in)
    f_out.close()
    f_in.close()

    compressed_file_size = os.path.getsize("./compressedChrFiles/" + file_substr + '.zip')

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

    #file_substr = file.split('/')[-1] # get name of processed file
    #print(file_substr)

    readed_file = open(file, 'r')
    lines = readed_file.readlines()

    count = 0
    # Strips the newline character
    for line in lines:
        count += 1
        if (line[0:1] == "#"):
            continue
        #print(line.strip())
        cgp_gates_codes = line[line.find('}') + 2:len(line)]
        #print(cgp_gates_codes)

        cgp_gates_codes_list = cgp_gates_codes.split(')(')
        cgp_gates_codes_list = cgp_gates_codes_list[:-1]
        #print (cgp_gates_codes_list)


    for cgp_gates_code in cgp_gates_codes_list:
        used_gate_id_str = cgp_gates_code[cgp_gates_code.rfind(',')+1:len(cgp_gates_code)]
        #print(used_gate_id_str)

        used_gate_id = int (used_gate_id_str)
        #print (used_gate_id)

        if (used_gate_id == 0):
            ida_count += 1
        elif (used_gate_id == 1):
            inva_count += 1
        elif (used_gate_id == 2):
            and_count += 1
        elif (used_gate_id == 3):
            or_count += 1
        elif (used_gate_id == 4):
            xor_count += 1
        elif (used_gate_id == 5):
            nand_count += 1
        elif (used_gate_id == 6):
            nor_count += 1
        elif (used_gate_id == 7):
            xnor_count += 1

    xor_values.append(xor_count)
    compressed_chr_sizes.append(compressed_file_size)

    with open('./csvFiles/chrFeatures.csv', 'a', newline='') as csv_file:
        writer = csv.writer(csv_file)
        writer.writerow([file_substr, ida_count, inva_count, and_count, or_count, xor_count, nand_count, nor_count, xnor_count, compressed_file_size])

    csv_file.close()

    with open('filtered_data.json', "rb") as json_file:
        json_data = orjson.loads(json_file.read())

        #json_name_id = json_data.keys()
        #print (json_name_id)


        mae_value = json_data[file_substr][0]["mae"]
        print(mae_value)
        mae_values.append(mae_value)
        wce_value = json_data[file_substr][0]["wce"]
        wce_values.append(wce_value)

figure = plt.figure(figsize=(30, 30))
xor_mae_plot = figure.add_subplot(2,2,1)
xor_mae_plot.scatter(xor_values, mae_values)
xor_mae_plot.set_xlabel('Count of Used XOR Gates by CGP')
xor_mae_plot.set_ylabel('Mean Absolute Error')

xor_wce_plot = figure.add_subplot(2,2,2)
xor_wce_plot.scatter(xor_values, wce_values)
xor_wce_plot.set_xlabel('Count of Used XOR Gates by CGP')
xor_wce_plot.set_ylabel('Worst Case Error')

compressed_mae_plot = figure.add_subplot(2,2,3)
compressed_mae_plot.scatter(compressed_chr_sizes, mae_values)
compressed_mae_plot.set_xlabel('Compressed Chr File Size')
compressed_mae_plot.set_ylabel('Mean Absolute Error')

compressed_wce_plot = figure.add_subplot(2,2,4)
compressed_wce_plot.scatter(compressed_chr_sizes, wce_values)
compressed_wce_plot.set_xlabel('Compressed Chr File Size')
compressed_wce_plot.set_ylabel('Worst Case Error')


plt.show()
