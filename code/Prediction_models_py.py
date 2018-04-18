import os
import time
import pandas as pd
import calendar
import matplotlib.pyplot as plt
import numpy as np
from sklearn import linear_model
from sklearn import metrics
from sklearn.cross_validation import train_test_split
from sklearn.metrics import confusion_matrix
from sklearn.linear_model import LogisticRegression
from sklearn.metrics import accuracy_score
from sklearn.learning_curve import validation_curve

# import dataset into a dataframe df
name = "usag.csv" #change this to the file emplacement of 'usag1'
df = pd.read_csv(name, sep = ";", encoding='latin-1')

# Choose the department on wich you want to make the predictions
df_45=df.loc[df['dep'] == 450 ]

# Choose the features that you want to work with
df1 = df_45[['mois','lum','hrmn','atm','agg','catu','sexe','trajet','age','grav']]
#df1 = df_45[['mois','hrmn','long','lat','lum','atm','agg','catu','sexe','trajet','age','grav']]

# Cleaning df1 from missing data
df1=df1.dropna(axis=0,how='any')

# Stock the headers or features names chosen
df1_headers=df1.columns.values
print ("Number of observations :: ", len(df1.index))
print ("Number of columns :: ", len(df1.columns))
print ("Headers :: ", df1.columns.values)
#print ("Target :: ", df1[df1_headers[-1]])

# Features engineering:

# 1.categorizing age
for index, row in df1.iterrows(): 
    if (row['age']<19):
        df1.at[index,'age']=1
    elif (row['age']<30):
        df1.at[index,'age']=2
    elif (row['age']<60):
        df1.at[index,'age']=3
    else:
        df1.at[index,'age']=4
        
# 2.features scaling
from sklearn.preprocessing import StandardScaler
scale = StandardScaler()
X_scaled = scale.fit_transform(df1[df1_headers[:-1]].as_matrix())
df_scaled = pd.DataFrame(X_scaled, columns=df1_headers[:-1])

# Split data into training data and testing data
train_x, test_x, train_y, test_y = train_test_split(df_scaled,df1[df1_headers[-1]], train_size=0.7, random_state=0)

 
# Training multinomial logistic regression model
startTime = time.time()
mul_lr = linear_model.LogisticRegression(multi_class='multinomial', solver='newton-cg').fit(train_x, train_y)
elapsedTime = time.time() - startTime
print ("predictions time LR = ",elapsedTime)

# Training a DescisionTreeClassifier
from sklearn.tree import DecisionTreeClassifier
startTime = time.time()
dtree_model = DecisionTreeClassifier(max_depth = 130, min_samples_split= 0.001, min_samples_leaf=10, max_features=8, max_leaf_nodes=35).fit(train_x, train_y)
elapsedTime = time.time() - startTime
print ("predictions time DT = ",elapsedTime)
 
# Training a linear SVM classifier
from sklearn.svm import SVC
startTime = time.time()
svm_model_linear = SVC(kernel = 'linear', C = 1).fit(train_x, train_y)
elapsedTime = time.time() - startTime
print ("predictions time SVM = ",elapsedTime)

# training a KNN classifier
from sklearn.neighbors import KNeighborsClassifier
startTime = time.time()
knn = KNeighborsClassifier(n_neighbors = 7).fit(train_x, train_y)
elapsedTime = time.time() - startTime
print ("predictions time KNN = ",elapsedTime)

# training a Naive Bayes classifier
from sklearn.naive_bayes import GaussianNB
startTime = time.time()
gnb = GaussianNB().fit(train_x, train_y)
elapsedTime = time.time() - startTime
print ("predictions time Naive = ",elapsedTime,"\n")

# Calculate accuracy
 
print ("Multinomial Logistic regression Test Accuracy :: ", metrics.accuracy_score(test_y, mul_lr.predict(test_x)))

print ("decision tree Test Accuracy :: ", metrics.accuracy_score(test_y, dtree_model.predict(test_x)))

print ("SVM Test Accuracy :: ", metrics.accuracy_score(test_y, svm_model_linear.predict(test_x)))

print ("knn Test Accuracy :: ", metrics.accuracy_score(test_y, knn.predict(test_x)))

print ("naive bayes Test Accuracy :: ", metrics.accuracy_score(test_y, gnb.predict(test_x)))
