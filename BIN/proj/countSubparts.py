
file = "./cgp-approx14ep.chr/mult8_cgp14ep_ep13107_wc1_3_rcam.chr"



f_in = open(file, 'rb')
file_substr = file.split('/')[-1] # get name of processed file
file_substr = file_substr[:-4] # cut .char


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
    print(cgp_gates_codes)
    outputs = (cgp_gates_codes[cgp_gates_codes.rfind('(') + 1:-1]).split(',')
    outputs_list = list (map(int, outputs))
    cgp_gates_codes_list = cgp_gates_codes.split(')(')
    cgp_gates_codes_list = cgp_gates_codes_list[:-1]
    print (cgp_gates_codes_list)
    print(outputs_list)

used_cgp_gates_codes_list = []
for cgp_gates_code in cgp_gates_codes_list:
    gate_id = int (cgp_gates_code[1:cgp_gates_code.find(']')])
    print(gate_id)
    input1 = int (cgp_gates_code[cgp_gates_code.find(']') + 1:cgp_gates_code.find(',')])
    print(input1)
    input2 = int (cgp_gates_code[cgp_gates_code.find(',') + 1:cgp_gates_code.rfind(',')])
    print(input2)

    for cgp_gates_code2 in cgp_gates_codes_list:
        if (cgp_gates_code == cgp_gates_code2):
            continue
        gate_id2 = int (cgp_gates_code2[1:cgp_gates_code2.find(']')])
        input12 = int (cgp_gates_code2[cgp_gates_code2.find(']') + 1:cgp_gates_code2.find(',')])
        input22 = int (cgp_gates_code2[cgp_gates_code2.find(',') + 1:cgp_gates_code2.rfind(',')])

        if (input12 == gate_id or input22 == gate_id or gate_id in outputs_list):
            used_cgp_gates_codes_list.append(cgp_gates_code)
            break
print("Size of used cells: " + str(len(used_cgp_gates_codes_list)))
xor_cells = []
and_cells = []
for cgp_gates_code in used_cgp_gates_codes_list:
    used_gate_id_str = cgp_gates_code[cgp_gates_code.rfind(',')+1:len(cgp_gates_code)]
    #print(used_gate_id_str)

    used_gate_id = int (used_gate_id_str)
    #print (used_gate_id)

    if (used_gate_id == 2):
        and_cells.append(cgp_gates_code)
    elif (used_gate_id == 4):
        xor_cells.append(cgp_gates_code)


print(xor_cells)

xor_xor_xor_count = 0
for xor_cell in xor_cells:
    gate_id = int (xor_cell[1:xor_cell.find(']')])
    print(gate_id)
    input1 = int (xor_cell[xor_cell.find(']') + 1:xor_cell.find(',')])
    print(input1)
    input2 = int (xor_cell[xor_cell.find(',') + 1:xor_cell.rfind(',')])
    print(input2)

    for xor_cell2 in xor_cells:
        if (xor_cell == xor_cell2):
            continue
        gate_id2 = int (xor_cell2[1:xor_cell2.find(']')])
        input12 = int (xor_cell2[xor_cell2.find(']') + 1:xor_cell2.find(',')])
        input22 = int (xor_cell2[xor_cell2.find(',') + 1:xor_cell2.rfind(',')])

        if (gate_id == input12 or gate_id == input22): # a XOR b XOR c
            for xor_cell3 in xor_cells:
                if (xor_cell2 == xor_cell3):
                    continue
                gate_id3 = int (xor_cell3[1:xor_cell3.find(']')])
                input13 = int (xor_cell3[xor_cell3.find(']') + 1:xor_cell3.find(',')])
                input23 = int (xor_cell3[xor_cell3.find(',') + 1:xor_cell3.rfind(',')])

                if (gate_id2 == input13 or gate_id2 == input23):
                    print(xor_cell + "->" + xor_cell2 + "->" +xor_cell3)
                    xor_xor_xor_count += 1
print("XOR XOR XOR count " + str(xor_xor_xor_count))


and_xor_and_count = 0
for and_cell in and_cells:
    gate_id = int (and_cell[1:and_cell.find(']')])
    print(gate_id)
    input1 = int (and_cell[and_cell.find(']') + 1:and_cell.find(',')])
    print(input1)
    input2 = int (and_cell[and_cell.find(',') + 1:and_cell.rfind(',')])
    print(input2)

    for xor_cell2 in xor_cells:
        gate_id2 = int (xor_cell2[1:xor_cell2.find(']')])
        input12 = int (xor_cell2[xor_cell2.find(']') + 1:xor_cell2.find(',')])
        input22 = int (xor_cell2[xor_cell2.find(',') + 1:xor_cell2.rfind(',')])

        if (gate_id3 == input12 or gate_id3 == input22): # a AND b XOR
            for and_cell3 in and_cells:
                if (and_cell == and_cell3):
                    continue
                gate_id3 = int (and_cell3[1:and_cell3.find(']')])
                input13 = int (and_cell3[and_cell3.find(']') + 1:and_cell3.find(',')])
                input23 = int (and_cell3[and_cell3.find(',') + 1:and_cell3.rfind(',')])

                if (gate_id2 == input13 or gate_id2 == input23):
                    print(and_cell + "->" + xor_cell2 + "->" +and_cell3)
                    and_xor_and_count += 1
print("AND XOR AND count " + str(and_xor_and_count))
