from Bio import Phylo
import csv

# https://www.geeksforgeeks.org/python-program-to-convert-a-list-to-string/
def listToString(s):

    # initialize an empty string
    str1 = ""

    # traverse in the string
    for ele in s:
        str1 += ele

    # return string
    return str1

tree = Phylo.read("./files/tree.tre", "newick")
print(tree)
Phylo.draw_ascii(tree)

#readed_file = open("./files/ancestrals.csv", 'r')
#lines = readed_file.readlines()
#print(lines)

count = 0
# Strips the newline character
ancestrals_dict = {}
with open("./files/ancestrals.csv", "r") as a_file:
  for line in a_file:
    line = line.strip()

    #print(line)

    if (count == 0):
        count += 1
        continue

    line_list = line.split(",")
    #print(line_list)

    node_key = int (line_list[0])
    ancestrals_dict[node_key] = []

    count += 1

count = 0
with open("./files/ancestrals.csv", "r") as a_file:
  for line in a_file:
    line = line.strip()

    #print(line)

    if (count == 0):
        header_aa_list = line.split(",")
        header_aa_list = header_aa_list[2:]
        count += 1
        continue

    line_list = line.split(",")
    #print(line_list)

    node_key = int (line_list[0])
    index = int (line_list[1]) - 1 # positions are indexed from 1

    #print(line_list)

    aa_probabilities = line_list[2:]
    #print(aa_probabilities)

    max_probability = 0
    max_aa_index = 0
    aa_index = 0

    #print(aa_probabilities)

    for p in aa_probabilities:
        #print(p)
        if (p == '-'):
            aa_index = aa_index + 1
            continue
        p = float (p)

        if (p > max_probability):
            max_probability = p
            max_aa_index = aa_index

        aa_index = aa_index + 1

    '''
    print("Max probability")
    print(max_probability)
    print("Max index")
    print(max_aa_index)
    '''

    ancestrals_dict[node_key].insert(index, header_aa_list[max_aa_index])

    count += 1

#print(ancestrals_dict)
#print(count)

for clade in tree.find_clades():
    tree_node_id = clade.confidence

    if (tree_node_id != None):
        #print(ancestrals_dict[tree_node_id])
        file_name = "node_" + str(tree_node_id) + ".fas"
        f = open("./results/probabilities/" + file_name,"w+")
        sequence = listToString(ancestrals_dict[tree_node_id])
        f.write(sequence + "\n")
        f.close()

        print(tree_node_id.getChildren())
