# -*- coding: utf-8 -*-
"""
Created on Thu Jan 27 09:15:08 2022

@author: UP math
"""
import numpy as np
import pandas as pd
from random import randrange
from sklearn.metrics import classification_report,confusion_matrix

# libraries for random forest
from sklearn.ensemble import RandomForestClassifier

##################################
# Chargement du dataset avec keras
##################################
# Création des jeux de données

file = "E:/Master 2/Simulateddata/data/data_fix_finales/data_python_explicites.csv"

df = pd.read_csv(file, encoding='latin-1')

acc = 0

for i in range(100): 
    
    sample = randrange(len(df))
    
    df_test = df.iloc[[sample],:]
    
    df_train = df.drop(sample, axis = 0)
    
    X_train = df_train.drop('Classe',axis = 1)
    # create df features
    y_train = df_train['Classe']
    # create df var to predict
    
    # split df in train and test df
    
    rfc = RandomForestClassifier()
    n_estimators=200
    # instatiate model
    rfc.fit(X_train,y_train)
    
    # train/fit the model
    
    X_test = df_test.drop('Classe',axis = 1)
    y_test = df_test['Classe']
    
    rfc_pred = rfc.predict(X_test)
    
    if rfc_pred[0] == np.array(y_test)[0] : 
        acc = acc + 1
    else :
        acc = acc + 0

print(acc, "% d'accuracy")
# EVAUATE MODEL

# print(confusion_matrix(y_test,rfc_pred))
# print(classification_report(y_test,rfc_pred))
