# Project name: Relationship Analysis of Approximated Circuits
# Author: Katerina Fortova
# Login: xforto00
# Year: 2020 / 2021

# Description: MLP prediction of seed of evolved multiplier by CGP, network uses vectors of Variability of Implementation of the Highest Bit of Product (./csvFiles/allVectorsO15.csv, ./csvFiles/allClassesO15.csv) for all evolved multipliers
# It was also experimented with vectors of gates types used by CGP (./csvFiles/allVectorsUsedGatesTypes.csv, ./csvFiles/allClassesUsedGatesTypes.csv) but results werent good (see Project Presentation)
# loss and accuracy graph is saved into ./mlpGraphs/ folder

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

# model of MLP
class Net(nn.Module):
    def __init__(self,input_dim,output_dim):
        super(Net,self).__init__()

        self.layer_1 = nn.Linear(input_dim, 128)
        self.layer_2 = nn.Linear(128, 64)
        self.layer_out = nn.Linear(64, output_dim)

        self.relu = nn.ReLU()
        self.softmax = nn.Softmax(dim=1)
        self.dropout = nn.Dropout(p=0.5)

    def forward(self,x):
        x = self.layer_1(x)
        x = self.relu(x)
        x = self.dropout(x)

        x = self.layer_2(x)
        x = self.softmax(x)
        x = self.dropout(x)

        x = self.layer_out(x)

        return x

# reduce data of each class
# class with minimum of vectors found and each other class reduce vectors to this minimum too
def reduceData(vectors, targets):
    unique, counts = np.unique(targets, return_counts=True)
    classes_counts_dict = dict(zip(unique, counts))
    min_key = min(classes_counts_dict, key=classes_counts_dict.get)
    min_value = classes_counts_dict[min_key]

    class_0_count = 0
    class_1_count = 0
    class_2_count = 0
    class_3_count = 0
    class_4_count = 0
    class_5_count = 0

    vectors_reduced = np.array([])
    classes_reduced = np.array([])

    for vector, target in zip(vectors, targets):
        if (target == 0 and class_0_count == min_value):
            continue
        elif (target == 1 and class_1_count == min_value):
            continue
        elif (target == 2 and class_2_count == min_value):
            continue
        elif (target == 3 and class_3_count == min_value):
            continue
        elif (target == 4 and class_4_count == min_value):
            continue
        elif (target == 5 and class_5_count == min_value):
            continue

        if (target == 0):
            class_0_count += 1
        elif (target == 1):
            class_1_count += 1
        elif (target == 2):
            class_2_count += 1
        if (target == 3):
            class_3_count += 1
        elif (target == 4):
            class_4_count += 1
        elif (target == 5):
            class_5_count += 1

        vectors_reduced = np.append(vectors_reduced, vector)
        classes_reduced = np.append(classes_reduced, target)

    vectors_reduced.shape = (min_value * 3, 8)

    vectors_reduced = vectors_reduced.astype('int64')
    classes_reduced = classes_reduced.astype('int64')

    return vectors_reduced, classes_reduced

# replace classes when we want to have only 3 classes:
# 0 - rcam, 1 - wtm (wtm-cla, wtm-csa, wtm-rca), 2 - csam (csam-rca, csam-csa)
def replaceClasses(targets):
    print(targets)
    targets[targets==3]=2
    targets[targets==4]=1
    targets[targets==5]=1
    print(targets)
    return targets

# load vectors and their classes
vectors = np.genfromtxt('./csvFiles/allVectorsO15.csv',delimiter=",", dtype=int, skip_header=1)
targets = np.genfromtxt('./csvFiles/allClassesO15.csv',dtype=int, skip_header=1)

#targets = replaceClasses(targets)

#vectors, targets = reduceData(vectors, targets)

# normalize vector values
transformer = Normalizer().fit(vectors)
vectors = transformer.transform(vectors)

# split to train and val dataset
x, x_val, y, y_val = train_test_split(vectors, targets, test_size=0.30, random_state=42)
# split val dataset to val dataset and test dataset
x_val, x_test, y_val, y_test = train_test_split(x_val, y_val, test_size=0.50, random_state=42)

x_train = x.reshape(-1, x.shape[1]).astype('float32')
y_train = y

x_val = x_val.reshape(-1, x_val.shape[1]).astype('float32')
y_val = y_val

x_test = x_test.reshape(-1, x_test.shape[1]).astype('float32')
y_test = y_test


x_val = torch.from_numpy(x_val)
y_val = torch.from_numpy(y_val)

x_test = torch.from_numpy(x_test)
y_test = torch.from_numpy(y_test)

data_set = Data()
trainloader = DataLoader(dataset=data_set,batch_size=32)

input_dim = 8     # how many features has vector
output_dim = 6   # number of classes

model = Net(input_dim,output_dim)
# declare criteria of model
criterion = nn.CrossEntropyLoss()
learning_rate = 0.001
optimizer = torch.optim.Adam(model.parameters(), lr=learning_rate)
n_epochs = 200

loss_list = []
epochs_list = []
accuracies = []
losses = []
accuracy = 0
# train model for declared count of epochs
for epoch in range(n_epochs):
    epochs_list.append(epoch)

    minibatches_iterations = 0
    losses_minibatches_sum = 0
    for x, y in trainloader:
        optimizer.zero_grad()
        z = model(x)
        loss = criterion(z,y)
        loss.backward()
        optimizer.step()

        losses_minibatches_sum = losses_minibatches_sum + loss.item() # accumulate losses of minibatches
        minibatches_iterations += 1

    # in each epoch make prediction on validation data and compute accuracy of prediction
    z = model(x_val)
    values, indices = torch.max(z.data,1)

    np_predictions = indices.detach().numpy()
    np_targets = y_val.detach().numpy()

    correctly_classified = 0
    for target, prediction in zip(np_targets, np_predictions):
        if (target == prediction):
            correctly_classified += 1

    new_accuracy = (correctly_classified / np.shape(np_targets)[0])

    accuracies.append(new_accuracy)

    # make deep copy of model with highest accuracy
    if (new_accuracy > accuracy):
        best_model = copy.deepcopy(model)

    loss_average = losses_minibatches_sum / minibatches_iterations

    losses.append(loss_average)

    print('Epoch {}, Loss {}, Accuracy {}'.format(epoch, loss_average, new_accuracy))

# plot graphs of loss and accuracy development
figure = plt.figure(figsize=(10, 10))
performance_plot = figure.add_subplot(2,1,1)
performance_plot.plot(epochs_list, accuracies, color = "orchid", label="accuracy development")
performance_plot.set_xlabel('Count of epochs', fontsize=8, horizontalalignment='right', x=1.0)
performance_plot.legend(prop={'size': 10})
performance_plot.set_title('MLP Performance', fontsize=10)

performance_plot2 = figure.add_subplot(2,1,2)
performance_plot2.plot(epochs_list, losses, color = "indigo", label="loss development")
performance_plot2.set_xlabel('Count of epochs', fontsize=8, horizontalalignment='right', x=1.0)
performance_plot2.legend(prop={'size': 10})
plt.savefig('./mlpGraphs/mlpGraphs.png')

z = best_model(x_test)
values, indices = torch.max(z.data,1)

# finally predict model with best accuracy on test data
np_predictions = indices.detach().numpy()
np_targets = y_test.detach().numpy()

correctly_classified = 0
for target, prediction in zip(np_targets, np_predictions):
    if (target == prediction):
        correctly_classified += 1

accuracy = (correctly_classified / np.shape(np_targets)[0])
print('Accuracy on Test Data: {}'.format(accuracy))
