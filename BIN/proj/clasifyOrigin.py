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

        #self.layer_1 = nn.Linear(D_in, 512)
        self.layer_2 = nn.Linear(D_in, 128)
        self.layer_3 = nn.Linear(128, 64)
        self.layer_out = nn.Linear(64, D_out)

        self.relu = nn.ReLU()
        self.softmax = nn.Softmax(dim=1)
        self.dropout = nn.Dropout(p=0.5)
        self.batchnorm1 = nn.BatchNorm1d(512)
        self.batchnorm2 = nn.BatchNorm1d(128)
        self.batchnorm3 = nn.BatchNorm1d(64)


    def forward(self,x):
        #x=torch.nn.sigmoid(self.linear1(x))
        #x = torch.nn.functional.softmax (self.linear1(x))
        #x = self.linear2(x)
        #return x
        '''
        x = self.layer_1(x)
        x = self.batchnorm1(x)
        x = self.relu(x)
        '''

        x = self.layer_2(x)
        #x = self.batchnorm2(x)
        x = self.relu(x)
        x = self.dropout(x)

        x = self.layer_3(x)
        #x = self.batchnorm3(x)
        x = self.softmax(x)
        x = self.dropout(x)

        x = self.layer_out(x)

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

def replaceClasses(targets):
    print(targets)
    targets[targets==3]=2
    targets[targets==4]=1
    targets[targets==5]=1
    print(targets)
    return targets

vectors = np.genfromtxt('./csvFiles/allVectorsO15.csv',delimiter=",", dtype=int, skip_header=1)
targets = np.genfromtxt('./csvFiles/allClassesO15.csv',dtype=int, skip_header=1)
print(vectors.dtype)
print(targets.dtype)
#targets = replaceClasses(targets)

#vectors, targets = reduceData(vectors, targets)
print(vectors.dtype)
print(targets.dtype)

transformer = Normalizer().fit(vectors)
vectors = transformer.transform(vectors)



print(vectors)
print(targets)

# split to train and val dataset
x, x_val, y, y_val = train_test_split(vectors, targets, test_size=0.30, random_state=42)
# split val dataset to val dataset and test dataset
x_val, x_test, y_val, y_test = train_test_split(x_val, y_val, test_size=0.50, random_state=42)

print(x.shape)
print(y.shape)
print(x_val.shape)
print(y_val.shape)

x_train = x.reshape(-1, x.shape[1]).astype('float32')
y_train = y

x_val = x_val.reshape(-1, x_val.shape[1]).astype('float32')
y_val = y_val

x_test = x_test.reshape(-1, x_test.shape[1]).astype('float32')
y_test = y_test

print(x_train)

x_val = torch.from_numpy(x_val)
y_val = torch.from_numpy(y_val)

x_test = torch.from_numpy(x_test)
y_test = torch.from_numpy(y_test)

data_set = Data()
trainloader = DataLoader(dataset=data_set,batch_size=32)

print(data_set.x[1:10])

input_dim = 8     # how many Variables are in the dataset
hidden_dim = 4 # hidden layers
output_dim = 6   # number of classes

model = Net(input_dim,hidden_dim,output_dim)
print('W:',list(model.parameters())[0].size())
print('b',list(model.parameters())[1].size())

criterion = nn.CrossEntropyLoss()
learning_rate = 0.001
optimizer = torch.optim.Adam(model.parameters(), lr=learning_rate)

n_epochs = 1000
loss_list = []

epochs_list = []
accuracies = []
losses = []
accuracy = 0
for epoch in range(n_epochs):
    epochs_list.append(epoch)

    minibatches_iterations = 0
    losses_minibatches_sum = 0
    for x, y in trainloader:
        #clear gradient
        optimizer.zero_grad()
        #make a prediction
        z = model(x)
        loss = criterion(z,y)
        # calculate gradients of parameters
        loss.backward()
        # update parameters
        optimizer.step()

        losses_minibatches_sum = losses_minibatches_sum + loss.item() # accumulate losses of minibatches
        minibatches_iterations += 1


    z = model(x_val)
    values, indices = torch.max(z.data,1)

    np_predictions = indices.detach().numpy()
    np_targets = y_val.detach().numpy()

    correctly_classified = 0
    for target, prediction in zip(np_targets, np_predictions):
        if (target == prediction):
            correctly_classified += 1

    #print(correctly_classified)
    new_accuracy = (correctly_classified / np.shape(np_targets)[0])

    #print(new_accuracy)
    accuracies.append(new_accuracy)

    if (new_accuracy > accuracy):
        best_model = copy.deepcopy(model)

    loss_average = losses_minibatches_sum / minibatches_iterations

    losses.append(loss_average)

    print('epoch {}, loss {}, accuracy {}'.format(epoch, loss_average, new_accuracy))

figure = plt.figure(figsize=(10, 10))
performance_plot = figure.add_subplot(2,1,1)
performance_plot.plot(epochs_list, accuracies, color = "orchid", label="accuracy development")
performance_plot.set_xlabel('Count of epochs', fontsize=8, horizontalalignment='right', x=1.0)
performance_plot.legend(prop={'size': 10})
performance_plot.set_title('Single Feature Logistic Regression Performance', fontsize=10)

performance_plot2 = figure.add_subplot(2,1,2)
performance_plot2.plot(epochs_list, losses, color = "indigo", label="loss development")
performance_plot2.set_xlabel('Count of epochs', fontsize=8, horizontalalignment='right', x=1.0)
performance_plot2.legend(prop={'size': 10})
plt.savefig('./mlpGraphs/mlpGraphs.png')

z = best_model(x_test)
values, indices = torch.max(z.data,1)

# finally predict on test data
np_predictions = indices.detach().numpy()
np_targets = y_test.detach().numpy()
print(np_predictions)
print(np_targets)

correctly_classified = 0
for target, prediction in zip(np_targets, np_predictions):
    if (target == prediction):
        correctly_classified += 1

print(correctly_classified)
accuracy = (correctly_classified / np.shape(np_targets)[0])
print(accuracy)
