import numpy as np

'''
f = open("./csvFiles/allVectors.csv", "r")
g = open("./csvFiles/allClasses.csv", "r")

reduced_vectors = open("./csvFiles/allVectorsReduced.csv","w+")
reduced_classes = open("./csvFiles/allClassesReduced.csv","w+")


lines_vectors = f.readlines()
lines_classes = g.readlines()
line_id = 0

class_0_count = 0
class_1_count = 0
class_2_count = 0

count_values = []

for line in lines_classes:
    if (line_id == 0): # skip header
        line_id += 1
        continue

    stripped_line = line.strip()
    class_id = int (stripped_line)

    if (class_id == 0):
        class_0_count += 1
    elif (class_id == 1):
        class_1_count += 1
    elif (class_id == 2):
        class_2_count += 1
    line_id += 1

print("Class 0 Count: " + str(class_0_count))
print("Class 1 Count: " + str(class_1_count))
print("Class 2 Count: " + str(class_2_count))

count_values.append(class_0_count)
count_values.append(class_1_count)
count_values.append(class_2_count)

min_value = min(count_values)
min_index = count_values.index(min_value)

line_id = 0
class_0_count = 0
class_1_count = 0
class_2_count = 0

for vector, class in zip(lines_vectors, lines_classes):
    stripped_vector = vector.strip()
    stripped_class = class.strip()

    if (line_id == 0):
        reduced_vectors.write(stripped_vector + "\n")
        reduced_classes.write(stripped_class + "\n")
        continue

    class_id = int (stripped_class)

    if (class_id == 0 and class_0_count == min_value):
        continue
    elif (class_id == 1 and class_1_count == min_value):
        continue
    elif (class_id == 2 and class_2_count == min_value):
        continue


    if (class_id == 0):
        class_0_count += 1
    elif (class_id == 1):
        class_1_count += 1
    elif (class_id == 2):
        class_2_count += 1

    reduced_vectors.write(stripped_vector + "\n")
    reduced_classes.write(stripped_class + "\n")

    line_id += 1






f.close()
g.close()
'''


vectors = np.genfromtxt('./csvFiles/allVectors.csv',delimiter=",", dtype=int, skip_header=1)
targets = np.genfromtxt('./csvFiles/allClasses.csv',dtype=int, skip_header=1)
print(vectors.shape)
print(targets.shape)

unique, counts = np.unique(targets, return_counts=True)
classes_counts_dict = dict(zip(unique, counts))
print(classes_counts_dict)
min_key = min(classes_counts_dict, key=classes_counts_dict.get)
min_value = classes_counts_dict[min_key]
print(min_value)

class_0_count = 0
class_1_count = 0
class_2_count = 0

vectors_reduced = np.array([])
classes_reduced = np.array([])

for vector, target in zip(vectors, targets):
    if (target == 0 and class_0_count == min_value):
        continue
    elif (target == 1 and class_1_count == min_value):
        continue
    elif (target == 2 and class_2_count == min_value):
        continue

    if (target == 0):
        class_0_count += 1
    elif (target == 1):
        class_1_count += 1
    elif (target == 2):
        class_2_count += 1

    #print(vector)
    #print(target)

    vectors_reduced = np.append(vectors_reduced, vector)
    classes_reduced = np.append(classes_reduced, target)

vectors_reduced.shape = (min_value * 3, 8)
print(vectors_reduced)
print(classes_reduced)


print(vectors_reduced.shape)
print(classes_reduced.shape)
