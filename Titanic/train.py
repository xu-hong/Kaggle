# -*- coding: utf-8 -*-
"""
Created on Tue Feb 10 01:33:18 2015

@author: xuhong
"""

import pandas as pd
import numpy as np
import pylab as P

df = pd.read_csv("train.csv", header=0)


pd.pivot_table(df, columns = ['Sex'], values = ['Age'], index = ['Survived'], aggfunc=np.mean)

df['Age'].dropna().hist(bins=16, alpha=0.5)
P.show()


df['Gender'] = df['Sex'].map({'female':1, 'male':0}).astype(int)

pd.pivot_table(df, columns = ['Sex'], values = ['Age'], index = ['Pclass'], aggfunc=np.median)

df.Cabin.dropna().map(lambda x: x[0]).value_counts()



###############

df.t = df.iloc[:, [0,1,2,4,5,6,7,9,10,11,12]]

df.t['CabinClass'] = df.t['Cabin'].map(lambda x: x[0] if pd.notnull(x) else x)
pd.pivot_table(df.t, columns=['Pclass'], index=['Embarked', 'CabinClass'], values=['Fare'], aggfunc=len)
### I cannot find a good estimate for Cabin missing value
### drop it.
df.t.drop(['CabinClass', 'Cabin'], axis=1, inplace=True)


### check if any columns contain NaN i.e. missing value
for c in df.t.columns: 
    print c, True in pd.isnull(df.t[c].unique())
    
len(df.t.Age[df.t.Age.isnull()])
len(df.t.Embarked[df.t.Embarked.isnull()])

df.t['Gender'] = df.t.Sex.map({'female': 1, 'male':0}).astype(float)
df.t['Embarkon'] = df.t.Embarked.map({'S': 0, 'C': 1, 'Q': 2}).astype(float)
df.t.drop(['Sex', 'Embarked'], axis=1, inplace=True)

median_ages = pd.pivot_table(df.t, columns = ['Gender'], index = ['Pclass'], values = ['Age'], aggfunc=np.median)
'''
median_ages = np.zeros([3,2])
for i in range(3):
    for j in range(2):
        median_ages[i, j] = df.t[(df.t.Pclass == i+1) & (df.t.Gender == j)]['Age'].dropna().median()
'''       
### or just using
median_ages = np.array(median_ages)

df.t['Agefill'] = df.t['Age']
oldindex = df.t.Agefill[df.t.Agefill.isnull()].index
for i in range(0, 3):
    for j in range(0, 2):
        df.t.loc[(df.t.Pclass == i+1) & (df.t.Gender == j) & (df.t.Age.isnull()), 'Agefill'] = median_ages[i, j]

df.t.loc[oldindex, ['Age', 'Agefill']]'

### 
df.t['Ageisnull'] = df.t.Age.map(lambda x: 1 if pd.isnull(x) else 0)
df.t.drop(['Age', 'PassengerId'], axis=1, inplace=True)
df.t = df.t.dropna()

###### done data munging
# Import the random forest package
from sklearn.ensemble import RandomForestClassifier 
from sklearn.cross_validation import cross_val_score
# Create the random forest object which will include all the parameters
# for the fit
forest = RandomForestClassifier(n_estimators = 100, max_features='sqrt')

# Fit the training data to the Survived labels and create the decision trees
forest = forest.fit(df.t.ix[0::,1::],df.t.ix[0::,0])
score = cross_val_score(forest, df.t.ix[0::,1::], df.t.ix[0::,0])
score.mean()
# Take the same decision trees and run it on the test data
### prepare test data
test_data = pd.read_csv("test.csv", header=0)
test_data['Gender'] = test_data.Sex.map({'female': 1, 'male':0}).astype(float)
test_data['Embarkon'] = test_data.Embarked.map({'S': 0, 'C': 1, 'Q': 2}).astype(float)
test_data['Agefill'] = test_data['Age']
median_ages_t = pd.pivot_table(test_data, columns = ['Gender'], index = ['Pclass'], values = ['Age'], aggfunc=np.median)
median_ages_t = np.array(median_ages_t)

for i in range(0, 3):
    for j in range(0, 2):
        test_data.loc[(test_data.Pclass == i+1) & (test_data.Gender == j) & (test_data.Age.isnull()), 'Agefill'] = median_ages_t[i, j]

test_data['Ageisnull'] = test_data.Age.map(lambda x: 1 if pd.isnull(x) else 0)

for c in test_data.columns: 
    print c, True in pd.isnull(test_data[c].unique())
## One fare is NaN
median_fare_t = pd.pivot_table(test_data, columns = ['Gender'], index = ['Pclass'], values = ['Fare'], aggfunc=np.median)
test_data.loc[test_data.Fare.isnull(),'Fare'] = median_fare_t.ix[3, 0]

test_data.drop(['Age', 'Name', 'Sex', 'Ticket', 'Cabin', 'Embarked'], axis=1, inplace=True)
test_pid = test_data.ix[:, 0]
test_data = test_data[df.t.columns[1::]]
output = forest.predict(test_data)

output = pd.DataFrame(output)
output.columns = ['Survived']
output.df = pd.concat([pd.DataFrame(test_pid), output], axis=1)

output.df.to_csv("submission.csv", index=False)



