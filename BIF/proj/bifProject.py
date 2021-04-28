from Bio import Phylo
from Bio import SeqIO
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

#https://www.geeksforgeeks.org/python-split-string-into-list-of-characters/
def split(word):
    return [char for char in word]

# https://biopython.org/wiki/Phylo_cookbook
def all_parents(tree):
    parents = {}
    for clade in tree.find_clades(order="level"):
        for child in clade:
            parents[child] = clade
    return parents

# find recursively all parent nodes ids (confidence) from sequences in fasta file (e.g. from sequence XP_007318498.1) and branch length of specific object
def findAllParentsRecursively(parents, child_key, found_parents, branch_lengths_to_nodes):
    #print(child_key.confidence)
    found_parent = parents.get(child_key)

    if found_parent is None:
        return found_parents, branch_lengths_to_nodes

    #print(found_parent)
    #print(found_parent.confidence)
    found_parents.append(int(found_parent.confidence))

    if (found_parent.branch_length is not None):
        branch_lengths_to_nodes.append(float(found_parent.branch_length))
    #else:
    #    branch_lengths_to_nodes.append(0)
    #print(found_parents)

    return (findAllParentsRecursively(parents, found_parent, found_parents, branch_lengths_to_nodes))

# retuns dictionary in form Node : [sequence of aminoacids with biggest probability on each index]
def getMostProbableAminoacids():
    count = 0
    # init keys to dictionary with nodes_id
    ancestrals_dict = {}
    with open("./files/ancestrals.csv", "r") as a_file:
      for line in a_file:
        line = line.strip()
        #print(line)

        # skip header of file
        if (count == 0):
            count += 1
            continue

        line_list = line.split(",")
        #print(line_list)

        node_key = int (line_list[0])
        ancestrals_dict[node_key] = []

        count += 1

    count = 0
    # fill dictionary values on specified keys
    with open("./files/ancestrals.csv", "r") as a_file:
      for line in a_file:
        line = line.strip()

        # skip header of file
        if (count == 0):
            header_aa_list = line.split(",")
            header_aa_list = header_aa_list[2:]
            count += 1
            continue

        line_list = line.split(",")

        node_key = int (line_list[0])
        index = int (line_list[1]) - 1 # positions are indexed from 1

        #print(line_list)
        # get all probabilities for each aminoacid
        aa_probabilities = line_list[2:]
        #print(aa_probabilities)

        max_probability = 0
        max_aa_index = 0
        aa_index = 0

        #print(aa_probabilities)
        # get maximum probability of aminoacid of each index
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
        # save to dictionary whole sequence of most probabible aminoacids on each indexes on each node
        ancestrals_dict[node_key].insert(index, header_aa_list[max_aa_index])

        count += 1

    return ancestrals_dict

# write sequnces with probabilities or gaps to folder, every node has its own file
def writeSequencesToOutputFolder(tree, ancestrals_dict, output_folder):
    for clade in tree.find_clades():
        tree_node_id = clade.confidence

        if (tree_node_id != None):
            #print(ancestrals_dict[tree_node_id])
            file_name = "node_" + str(tree_node_id) + ".fas"
            f = open(output_folder + file_name,"w+")
            sequence = listToString(ancestrals_dict[tree_node_id])
            f.write(sequence + "\n")
            f.close()


# for every sequence (e.g XP_007318498.1) I got all hierarchic parents and their confidence codes
# dictionary is in form Node : [names of sequences (e.g XP_007318498.1) which are descendants of this node]
def getSequencesOnNode(tree, sequences_id, parents):
    sequences_on_node = {}
    branch_lengths_on_node = {}
    # get keys values for dict
    for sequence_id in sequences_id:
        for clade in tree.find_clades():

            if (str (clade.name) == str (sequence_id)):
                all_parents = []
                branch_lengths_to_nodes = []
                branch_lengths_to_nodes.append(float(clade.branch_length))
                all_parents, branch_lengths_to_nodes = findAllParentsRecursively(parents, clade, [], branch_lengths_to_nodes)
                #print(all_parents)

                for p in all_parents:
                    sequences_on_node[p] = []
                    branch_lengths_on_node[p] = []
                break

    # insert all sequences which are connected in hierarchy to this node
    for sequence_id in sequences_id:
        #print(sequence_id)

        for clade in tree.find_clades():
            #print(clade.name)
            #print(sequence_id)
            if (str (clade.name) == str (sequence_id)):
                #print("FOUND")
                all_parents = []
                branch_lengths_to_nodes = []
                branch_lengths_to_nodes.append(float(clade.branch_length))
                all_parents, branch_lengths_to_nodes = findAllParentsRecursively(parents, clade, [], branch_lengths_to_nodes)
                #print(branch_lengths_to_nodes)

                i = 0
                branch_lengths_to_sum = []
                # we go from leaf child sequence to most distant parent node recursively and lengths of branches are added
                total_branch_lengths_to_nodes = []
                for branch_length in branch_lengths_to_nodes:
                    if (i == 0):
                        branch_lengths_to_sum.append(branch_length)
                    else:
                        branch_lengths_to_sum = branch_lengths_to_nodes[0:i+1]
                    #print(branch_lengths_to_sum)
                    total_branch_lengths_to_nodes.append(sum(branch_lengths_to_sum))
                    i += 1

                #print(total_branch_lengths_to_nodes)
                #print(all_parents)

                j = 0
                for p in all_parents:
                    sequences_on_node[p].append(sequence_id)
                    branch_lengths_on_node[p].append(total_branch_lengths_to_nodes[j])
                    j += 1
                break

    return sequences_on_node, branch_lengths_on_node

# get for every node indexes of positions where to insert gape instead of most probable aminoacid
# dictionary is in form Node : [indexes to insert gape]
# sequence_length = length of one sequence in fasta file
# records = whole loaded fasta file
def getPositionsToInsertGape(sequences_on_node, sequence_length, records):
    positions_to_insert_gape = {}
    for key, value in sequences_on_node.items():
        positions_to_insert_gape[key] = []
    for key, value in sequences_on_node.items():
        # init arrays to store count of gapes for each index and count of aminoacids for each index across sequnces connected to this node
        gapes_count = [0] * sequence_length
        #print(gapes_count)
        aa_count = [0] * sequence_length
        for v in value:
            #print(v)
            for r in records:
                if (v == r.name):
                    sequence_str = str(r.seq)
                    sequence_list = split(sequence_str)
                    #print(sequence_list)
                    i = 0
                    for s in sequence_list:
                        if (s == '-'):
                            old_count = gapes_count[i]
                            new_count = old_count + 1
                            gapes_count[i] = new_count
                        else:
                            old_count = aa_count[i]
                            new_count = old_count + 1
                            aa_count[i] = new_count
                        i += 1
                    #print(gapes_count)
                    #print(aa_count)

        j = 0
        for g, a in zip(gapes_count, aa_count):
            if (g > a):
                positions_to_insert_gape[key].append(j)
            j += 1
    #print(positions_to_insert_gape)
    return positions_to_insert_gape

# calculate positions to insert gape for each ancestral node
# here we also use branch length feature so when we e.g. have a gap in sequence we dont add 1 to occurence but length of branch from this leaf sequence to this ancestral node
def getPositionsToInsertGapeBranchLengthFeature(sequences_on_node, branch_lengths_on_node, sequence_length, records):
    positions_to_insert_gape = {}
    for key, value in sequences_on_node.items():
        positions_to_insert_gape[key] = []

    for key, value in sequences_on_node.items():
        print(key)
        # init arrays to store count of gapes for each index and count of aminoacids for each index across sequnces connected to this node
        branch_lengths_list = branch_lengths_on_node[key]
        #print(branch_lengths_list)
        gapes_count = [0] * sequence_length
        #print(gapes_count)
        aa_count = [0] * sequence_length

        k = 0
        for v in value:
            print(v)
            # get particular length of branch to specific sequence
            for r in records:
                if (v == r.name):
                    branch_length = branch_lengths_list[k]
                    print(branch_length)
                    k += 1
                    sequence_str = str(r.seq)
                    sequence_list = split(sequence_str)
                    #print(sequence_list)
                    i = 0
                    for s in sequence_list:
                        if (s == '-'):
                            old_count = gapes_count[i]
                            new_count = old_count + branch_length
                            gapes_count[i] = new_count
                        else:
                            old_count = aa_count[i]
                            new_count = old_count + branch_length
                            aa_count[i] = new_count
                        i += 1
                    #print(gapes_count)
                    #print(aa_count)

        j = 0
        for g, a in zip(gapes_count, aa_count):
            if (g > a):
                positions_to_insert_gape[key].append(j)
            j += 1
    #print(positions_to_insert_gape)
    return positions_to_insert_gape

# insert gapes to positions in output sequence
def insertGapesToSequence(ancestrals_dict, positions_to_insert_gape):
    for ancestral_key, ancestral_value in ancestrals_dict.items():
        probable_sequence = ancestrals_dict[ancestral_key]
        gape_node_positions = positions_to_insert_gape[ancestral_key]
        #print(gape_node_positions)
        k = 0
        aa_new_codes = []
        for aa_code in probable_sequence:
            #print(positions_to_insert_gape)
            #print(k)
            #print(aa_code)
            if (k in gape_node_positions):
                aa_new_codes.append("-")
            else:
                aa_new_codes.append(aa_code)
            k += 1

        #print(aa_new_codes)

        ancestrals_dict[ancestral_key] = aa_new_codes
    return ancestrals_dict





# read phylogenetic tree
tree = Phylo.read("./files/tree.tre", "newick")
print(tree)
Phylo.draw_ascii(tree)

ancestrals_dict = getMostProbableAminoacids()

# read fasta file and get names of written sequences and length of one written sequence
records = list(SeqIO.parse("./files/msa.fasta", "fasta"))
sequences_id = []
for r in records:
    sequences_id.append(r.name)
    sequence_length = len(str(r.seq))

################################## CALCULATE WITH ONLY MOST PROBABLE AMINOACIDS WITH POSTERIOR PROBABILITES (WITHOUT INSERTING GAPS) ######################################################################
found_leaves = []
writeSequencesToOutputFolder(tree, ancestrals_dict, "./results/probabilities/")

#################################### CALCULATE WITH INSERTING GAPS BASED ON GAPS OCCURENCE FREQUENCE ######################################################################
# get all parents of tree
parents = all_parents(tree)
#print(parents)

sequences_on_node,  branch_lengths_on_node = getSequencesOnNode(tree, sequences_id, parents)
#print(sequences_on_node)

positions_to_insert_gape = getPositionsToInsertGape(sequences_on_node, sequence_length, records)
print(positions_to_insert_gape)


ancestrals_dict = insertGapesToSequence(ancestrals_dict, positions_to_insert_gape)

writeSequencesToOutputFolder(tree, ancestrals_dict, "./results/gapes/")

#################################### CALCULATE WITH INSERTING GAPS BASED ON BRANCH LENGTH FEATURE ######################################################################
parents = all_parents(tree)
print(parents)

sequences_on_node,  branch_lengths_on_node = getSequencesOnNode(tree, sequences_id, parents)
print(sequences_on_node)
print(branch_lengths_on_node)


#for node_key, connected_node_sequences in sequences_on_node.items():
#    connected_node_sequences_list = sequences_on_node[node_key]


#positions_to_insert_gape = getPositionsToInsertGape(sequences_on_node, sequence_length, records)
positions_to_insert_gape = getPositionsToInsertGapeBranchLengthFeature(sequences_on_node, branch_lengths_on_node, sequence_length, records)
print(positions_to_insert_gape)

ancestrals_dict = insertGapesToSequence(ancestrals_dict, positions_to_insert_gape)

writeSequencesToOutputFolder(tree, ancestrals_dict, "./results/gapes_branch_distances/")
