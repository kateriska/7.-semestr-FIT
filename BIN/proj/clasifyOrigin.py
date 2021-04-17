import torch.nn as nn
import torch
import matplotlib.pyplot as plt
import numpy as np
from sklearn.model_selection import train_test_split
from torch.utils.data import Dataset, DataLoader
from sklearn.preprocessing import Normalizer

class Data(Dataset):
    def __init__(self):
        self.x=torch.from_numpy(x_train)
        self.y=torch.from_numpy(y_train)
        self.len=self.x.shape[0]
    def __getitem__(self,index):
        return self.x[index], self.y[index]
    def __len__(self):
        return self.len

class Net(nn.Module):
    def __init__(self,D_in,H,D_out):
        super(Net,self).__init__()
        self.linear1=nn.Linear(D_in,H)
        self.linear2=nn.Linear(H,D_out)


    def forward(self,x):
        x=torch.sigmoid(self.linear1(x))
        x=self.linear2(x)
        return x


vectors = np.genfromtxt('./csvFiles/allVectors.csv',delimiter=",", dtype=int, skip_header=1)
targets = np.genfromtxt('./csvFiles/allClasses.csv',dtype=int, skip_header=1)

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

data_set=Data()
trainloader=DataLoader(dataset=data_set,batch_size=64)

print(data_set.x[1:10])

input_dim=8     # how many Variables are in the dataset
hidden_dim = 25 # hidden layers
output_dim=6   # number of classes

model=Net(input_dim,hidden_dim,output_dim)
print('W:',list(model.parameters())[0].size())
print('b',list(model.parameters())[1].size())

criterion=nn.CrossEntropyLoss()
learning_rate=0.1
optimizer=torch.optim.Adam(model.parameters(), lr=learning_rate)

n_epochs=1000
loss_list=[]

#n_epochs
for epoch in range(n_epochs):
    for x, y in trainloader:


        #clear gradient
        optimizer.zero_grad()
        #make a prediction
        z=model(x)
        # calculate loss, da Cross Entropy benutzt wird muss ich in den loss Klassen vorhersagen,
        # also Wahrscheinlichkeit pro Klasse. Das mach torch.max(y,1)[1])
        loss=criterion(z,y)
        # calculate gradients of parameters
        loss.backward()
        # update parameters
        optimizer.step()

        loss_list.append(loss.data)


        print('epoch {}, loss {}'.format(epoch, loss.item()))

z=model(x_val)
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
