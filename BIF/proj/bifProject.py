from Bio import Phylo
from Bio import SeqIO
import csv

'''
function for converting list into string
Source:
***************************************************************************************
  *    Title: Python program to convert a list to string
  *    Author: author of geeksforgeeks with nickname "Shivam_k" -> https://auth.geeksforgeeks.org/user/Shivam_k/articles
  *    Date: 4.11.2019
  *    Code version: 1
  *    Availability: https://www.geeksforgeeks.org/python-program-to-convert-a-list-to-string/
**************************************************************************************
'''
def listToString(s):
    # initialize an empty string
    str1 = ""

    # traverse in the string
    for ele in s:
        str1 += ele

    # return string
    return str1

'''
function for converting string into list of chars
Source:
***************************************************************************************
  *    Title: Python | Split string into list of characters
  *    Author: author of geeksforgeeks with nickname "Smitha Dinesh Semwal" -> https://auth.geeksforgeeks.org/user/Smitha%20Dinesh%20Semwal/articles
  *    Date: 19.2.2021
  *    Code version: 1
  *    Availability: https://www.geeksforgeeks.org/python-split-string-into-list-of-characters/
**************************************************************************************
'''
def split(word):
    return [char for char in word]

'''
function for finding parents of clades hierarchically
Source:
***************************************************************************************
  *    Title: Bio.Phylo Cookbook.
  *    Authors: users of github with nicknames "etal", "peterjc", "MarkusPiotrowski", "niemasd", "AstrobioMike", "hlapp", "Gasta88"
  *    Date: 16.8.2020
  *    Code version: 1
  *    Availability: https://biopython.org/wiki/Phylo_cookbook, https://github.com/biopython/biopython.github.io/blob/master/wiki/Phylo_cookbook.md
**************************************************************************************
'''
def all_parents(tree):
    parents = {}
    for clade in tree.find_clades(order="level"):
        for child in clade:
            parents[child] = clade
    return parents

# find recursively all parent nodes ids (confidence) from sequences in fasta file (e.g. from sequence XP_007318498.1) and branch length of specific object
def findAllParentsRecursively(parents, child_key, found_parents, branch_lengths_to_nodes):
    found_parent = parents.get(child_key)

    if found_parent is None:
        return found_parents, branch_lengths_to_nodes

    found_parents.append(int(found_parent.confidence))

    if (found_parent.branch_length is not None):
        branch_lengths_to_nodes.append(float(found_parent.branch_length))

    return (findAllParentsRecursively(parents, found_parent, found_parents, branch_lengths_to_nodes))

# retuns dictionary in form Node : [sequence of aminoacids with biggest probability on each index]
def getMostProbableAminoacids():
    count = 0
    # init keys to dictionary with nodes_id
    ancestrals_dict = {}
    with open("ancestrals.csv", "r") as a_file:
      for line in a_file:
        line = line.strip()

        # skip header of file
        if (count == 0):
            count += 1
            continue

        line_list = line.split(",")

        node_key = int (line_list[0])
        ancestrals_dict[node_key] = []

        count += 1

    count = 0
    # fill dictionary values on specified keys
    with open("ancestrals.csv", "r") as a_file:
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

        # get all probabilities for each aminoacid
        aa_probabilities = line_list[2:]

        max_probability = 0
        max_aa_index = 0
        aa_index = 0

        # get maximum probability of aminoacid of each index
        for p in aa_probabilities:
            if (p == '-'):
                aa_index = aa_index + 1
                continue
            p = float (p)

            if (p > max_probability):
                max_probability = p
                max_aa_index = aa_index

            aa_index = aa_index + 1

        # save to dictionary whole sequence of most probabible aminoacids on each indexes on each node
        ancestrals_dict[node_key].insert(index, header_aa_list[max_aa_index])

        count += 1

    return ancestrals_dict

# write sequnces with probabilities or gaps to folder, every node has its own file
def writeSequencesToOutputFolder(tree, ancestrals_dict, output_folder):
    for clade in tree.find_clades():
        tree_node_id = clade.confidence

        if (tree_node_id != None):
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

                # add all keys into dictionary with init value of []
                for p in all_parents:
                    sequences_on_node[p] = []
                    branch_lengths_on_node[p] = []
                break

    # insert all sequences which are connected in hierarchy to this node
    for sequence_id in sequences_id:

        for clade in tree.find_clades():
            if (str (clade.name) == str (sequence_id)):
                all_parents = []
                branch_lengths_to_nodes = []
                branch_lengths_to_nodes.append(float(clade.branch_length))
                all_parents, branch_lengths_to_nodes = findAllParentsRecursively(parents, clade, [], branch_lengths_to_nodes)

                i = 0
                branch_lengths_to_sum = []
                # we go from leaf child sequence to most distant parent node recursively and lengths of branches are added
                total_branch_lengths_to_nodes = []
                for branch_length in branch_lengths_to_nodes:
                    if (i == 0):
                        branch_lengths_to_sum.append(branch_length)
                    else:
                        branch_lengths_to_sum = branch_lengths_to_nodes[0:i+1]
                    total_branch_lengths_to_nodes.append(sum(branch_lengths_to_sum))
                    i += 1

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
        aa_count = [0] * sequence_length
        for v in value:
            for r in records:
                if (v == r.name):
                    sequence_str = str(r.seq)
                    sequence_list = split(sequence_str)

                    i = 0
                    for s in sequence_list:
                        # count occurences of gape or aminoacid on specific index
                        if (s == '-'):
                            old_count = gapes_count[i]
                            new_count = old_count + 1
                            gapes_count[i] = new_count
                        else:
                            old_count = aa_count[i]
                            new_count = old_count + 1
                            aa_count[i] = new_count
                        i += 1

        j = 0
        for g, a in zip(gapes_count, aa_count):
            # if gapes count is higher, append this index as one where gape should be inserted
            if (g > a):
                positions_to_insert_gape[key].append(j)
            j += 1

    return positions_to_insert_gape

# calculate positions to insert gape for each ancestral node
# here we also use branch length feature so when we e.g. have a gap in sequence we dont add 1 to occurence but length of branch from this leaf sequence to this ancestral node
def getPositionsToInsertGapeBranchLengthFeature(sequences_on_node, branch_lengths_on_node, sequence_length, records):
    positions_to_insert_gape = {}
    for key, value in sequences_on_node.items():
        positions_to_insert_gape[key] = []

    for key, value in sequences_on_node.items():
        # init arrays to store count of gapes for each index and count of aminoacids for each index across sequnces connected to this node
        branch_lengths_list = branch_lengths_on_node[key]
        gapes_count = [0] * sequence_length
        aa_count = [0] * sequence_length

        k = 0
        for v in value:
            # get particular length of branch to specific sequence
            for r in records:
                if (v == r.name):
                    branch_length = branch_lengths_list[k]

                    k += 1
                    sequence_str = str(r.seq)
                    sequence_list = split(sequence_str)

                    i = 0
                    for s in sequence_list:
                        # count of occurence of gape or aminoacid but instead of adding 1 we add branch distance
                        if (s == '-'):
                            old_count = gapes_count[i]
                            new_count = old_count + branch_length
                            gapes_count[i] = new_count
                        else:
                            old_count = aa_count[i]
                            new_count = old_count + branch_length
                            aa_count[i] = new_count
                        i += 1

        j = 0
        for g, a in zip(gapes_count, aa_count):
            # if gapes count is higher, append this index as one where gape should be inserted
            if (g > a):
                positions_to_insert_gape[key].append(j)
            j += 1

    return positions_to_insert_gape

# insert gapes to positions in output sequence
def insertGapesToSequence(ancestrals_dict, positions_to_insert_gape):
    for ancestral_key, ancestral_value in ancestrals_dict.items():
        probable_sequence = ancestrals_dict[ancestral_key]
        gape_node_positions = positions_to_insert_gape[ancestral_key]

        k = 0
        aa_new_codes = []
        for aa_code in probable_sequence:
            # insert gape on positions previously computed
            if (k in gape_node_positions):
                aa_new_codes.append("-")
            else:
                aa_new_codes.append(aa_code)
            k += 1

        ancestrals_dict[ancestral_key] = aa_new_codes
    return ancestrals_dict



# read phylogenetic tree
tree = Phylo.read("tree.tre", "newick")
# print input tree in ASCII form
Phylo.draw_ascii(tree)

ancestrals_dict = getMostProbableAminoacids()

# read fasta file and get names of written sequences and length of one written sequence
records = list(SeqIO.parse("msa.fasta", "fasta"))
sequences_id = []
for r in records:
    sequences_id.append(r.name)
    sequence_length = len(str(r.seq))

################################## CALCULATE WITH ONLY MOST PROBABLE AMINOACIDS WITH POSTERIOR PROBABILITES (WITHOUT INSERTING GAPS) ######################################################################
found_leaves = []
writeSequencesToOutputFolder(tree, ancestrals_dict, "./results/probabilities/")
print("ANCESTRAL SEQUENCES WITH USING ONLY MOST PROBABLE AMINOACIDS WRITTEN INTO ./results/probabilities/")

#################################### CALCULATE WITH INSERTING GAPS BASED ON GAPS OCCURENCE FREQUENCE ######################################################################
# get all parents of tree
parents = all_parents(tree)

# get sequence which are connected to each node and branch length from each node (branch length is used as feature in CALCULATE WITH INSERTING GAPS BASED ON BRANCH LENGTH FEATURE)
sequences_on_node,  branch_lengths_on_node = getSequencesOnNode(tree, sequences_id, parents)

# get positions where to insert gape to each ancestral sequence
positions_to_insert_gape = getPositionsToInsertGape(sequences_on_node, sequence_length, records)

# insert gapes to each sequence and get dict in form Node Id : list of chars of sequence e.g. 66 : ['-','G','V',...]
ancestrals_dict = insertGapesToSequence(ancestrals_dict, positions_to_insert_gape)

# write final sequences to separate files based on ancestral node id into specific results folder
writeSequencesToOutputFolder(tree, ancestrals_dict, "./results/gapes/")
print("ANCESTRAL SEQUENCES WITH USING GAPS FREQUENCE (WITHOUT BRANCH LENGTH FEATURE) WRITTEN INTO ./results/gapes/")

#################################### CALCULATE WITH INSERTING GAPS BASED ON BRANCH LENGTH FEATURE ######################################################################
# get all parents of tree
parents = all_parents(tree)

#  get sequence which are connected to each node and branch length from each node (branch lengths are added when necessary, see Assignment of project)
sequences_on_node,  branch_lengths_on_node = getSequencesOnNode(tree, sequences_id, parents)

# get positions where to insert gape to each ancestral sequence
positions_to_insert_gape = getPositionsToInsertGapeBranchLengthFeature(sequences_on_node, branch_lengths_on_node, sequence_length, records)

# insert gapes to each sequnce and get dict in form Node Id : list of chars of sequence e.g. 66 : ['-','G','V',...]
ancestrals_dict = insertGapesToSequence(ancestrals_dict, positions_to_insert_gape)

# write final sequences to separate files based on ancestral node id into specific results folder
writeSequencesToOutputFolder(tree, ancestrals_dict, "./results/gapes_branch_distances/")
print("ANCESTRAL SEQUENCES WITH USING GAPS FREQUENCE AND BRANCH LENGTH FEATURE WRITTEN INTO ./results/gapes_branch_distances/")
