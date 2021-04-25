import torch.nn as nn
import torch
import matplotlib.pyplot as plt
import numpy as np
from sklearn.model_selection import train_test_split
from torch.utils.data import Dataset, DataLoader
from sklearn.preprocessing import Normalizer
import copy

class Data(Dataset):
    def __init__(self):
        self.x = torch.from_numpy(x_train)
        self.y = torch.from_numpy(y_train)
        self.len = self.x.shape[0]
    def __getitem__(self,index):
        return self.x[index], self.y[index]
    def __len__(self):
        return self.len

class Net(nn.Module):
    def __init__(self,D_in,H,D_out):
        super(Net,self).__init__()
        self.linear1 = nn.Linear(D_in,H)
        self.linear2 = nn.Linear(H,D_out)


    def forward(self,x):
        #x=torch.nn.sigmoid(self.linear1(x))
        x = torch.nn.functional.softmax (self.linear1(x))
        x = self.linear2(x)
        return x

def reduceData(vectors, targets):
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

    vectors_reduced = vectors_reduced.astype('int64')
    classes_reduced = classes_reduced.astype('int64')

    return vectors_reduced, classes_reduced

vectors = np.genfromtxt('./csvFiles/allVectors.csv',delimiter=",", dtype=int, skip_header=1)
targets = np.genfromtxt('./csvFiles/allClasses.csv',dtype=int, skip_header=1)
print(vectors.dtype)
print(targets.dtype)

vectors, targets = reduceData(vectors, targets)
print(vectors.dtype)
print(targets.dtype)

transformer = Normalizer().fit(vectors)
vectors = transformer.transform(vectors)



print(vectors)
print(targets)

x, x_val, y, y_val = train_test_split(vectors, targets, test_size=0.33, random_state=42)

print(x.shape)
print(y.shape)
print(x_val.shape)
print(y_val.shape)

x_train = x.reshape(-1, x.shape[1]).astype('float32')
y_train = y

x_val = x_val.reshape(-1, x_val.shape[1]).astype('float32')
y_val = y_val

print(x_train)

x_val = torch.from_numpy(x_val)
y_val = torch.from_numpy(y_val)

data_set = Data()
trainloader = DataLoader(dataset=data_set,batch_size=16)

print(data_set.x[1:10])

input_dim = 8     # how many Variables are in the dataset
hidden_dim = 4 # hidden layers
output_dim = 3   # number of classes

model = Net(input_dim,hidden_dim,output_dim)
print('W:',list(model.parameters())[0].size())
print('b',list(model.parameters())[1].size())

criterion = nn.CrossEntropyLoss()
learning_rate = 0.001
optimizer = torch.optim.Adam(model.parameters(), lr=learning_rate)

n_epochs = 5000
loss_list = []

#n_epochs
accuracy = 0
for epoch in range(n_epochs):
    for x, y in trainloader:
        #clear gradient
        optimizer.zero_grad()
        #make a prediction
        z = model(x)
        # calculate loss, da Cross Entropy benutzt wird muss ich in den loss Klassen vorhersagen,
        # also Wahrscheinlichkeit pro Klasse. Das mach torch.max(y,1)[1])
        loss = criterion(z,y)
        # calculate gradients of parameters
        loss.backward()
        # update parameters
        optimizer.step()

        loss_list.append(loss.data)


        print('epoch {}, loss {}'.format(epoch, loss.item()))

    z = model(x_val)
    values, indices = torch.max(z.data,1)


    np_predictions = indices.detach().numpy()
    np_targets = y_val.detach().numpy()
    #print(np_predictions)
    #print(np_targets)

    correctly_classified = 0
    for target, prediction in zip(np_targets, np_predictions):
        if (target == prediction):
            correctly_classified += 1

    #print(correctly_classified)
    new_accuracy = (correctly_classified / np.shape(np_targets)[0])


    if (new_accuracy > accuracy):
        best_model = copy.deepcopy(model)


z = best_model(x_val)
values, indices = torch.max(z.data,1)


np_predictions = indices.detach().numpy()
np_targets = y_val.detach().numpy()
print(np_predictions)
print(np_targets)

correctly_classified = 0
for target, prediction in zip(np_targets, np_predictions):
    if (target == prediction):
        correctly_classified += 1

print(correctly_classified)
accuracy = (correctly_classified / np.shape(np_targets)[0])
print(accuracy)
