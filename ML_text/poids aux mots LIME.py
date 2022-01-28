#### LIBRARIES
import pandas as pd
import numpy as np
import sklearn
import sklearn.ensemble
import sklearn.metrics
from sklearn.utils import shuffle
from io import StringIO
import re
from bs4 import BeautifulSoup
from nltk.tokenize import word_tokenize
from nltk.corpus import stopwords
from sklearn import svm
from sklearn.ensemble import RandomForestClassifier
from sklearn.model_selection import train_test_split
from sklearn.feature_extraction.text import CountVectorizer
from sklearn.linear_model import LogisticRegression
from sklearn.metrics import accuracy_score, f1_score, precision_score, recall_score
import lime
from lime import lime_text
from lime.lime_text import LimeTextExplainer
from sklearn.pipeline import make_pipeline
import nltk
from sklearn.naive_bayes import MultinomialNB
import os
os.chdir('D:/Eye-tracking/Simulateddata/ML_text')

######################################################################################
##
## CALCUL DE CHAQUE POIDS AU MOTS POUR LE RECODAGE 2 AVEC LE CLASSIFIEUR RF
##
######################################################################################

##### DATA
data = pd.read_csv('Donnees_explicites.csv', sep = ';', encoding = 'latin1')

####

vectorizer = CountVectorizer(analyzer='word',token_pattern=r'\w{1,}', ngram_range=(1, 3)) #cree un dtm

RF = RandomForestClassifier(random_state=0)
l1 = [[0 for i in range(13)] for j in range(616)]
c = make_pipeline(vectorizer, RF)
class_names=[1,2,3]
explainer = LimeTextExplainer(class_names=class_names)

x1 = set(data["Num_Sujet"].tolist())
a=0

for i in x1 :
    juge_train=set(data["Num_Sujet"].tolist())
    juge_train.remove(i)
    juge_test=[i]
    
    data_test = data[data.Num_Sujet.isin(juge_test)]
    X_test = data_test["Comm_net"].tolist()
    y_test = data_test["Classe_modif"].tolist()
    
    data_train = data[data.Num_Sujet.isin(juge_train)]
    X_train = data_train["Comm_net"].tolist()
    y_train = data_train["Classe_modif"].tolist()

    train_vectors = vectorizer.fit_transform(X_train)
    test_vectors = vectorizer.transform(X_test)
    RF.fit(train_vectors,y_train)
    
    p=data_test.shape[0]
    
    for j in range(0,p):
        exp = explainer.explain_instance(X_test[j], c.predict_proba, num_features=25, labels=[0,1,2])
        n=len(exp.as_list(label=0))
        for k in range(0,n):
            l1[a+j][0] = X_test[j]
            l1[a+j][1] = class_names[(RF.predict(test_vectors[j]).reshape(1,-1)[0,0])-1]
            l1[a+j][2] = class_names[y_test[j]-1]
            l1[a+j][3+k] = exp.as_list(label=0)[k]
    a=a+p
   
print(l1)

res_1 = np.asarray(l1)
res_1_df = pd.DataFrame(res_1)
res_1_df.to_csv("res_poids_LIME_Comm_net_colle_sansunique.csv",sep=";",encoding="latin1")


######################################################################################
##
## CALCUL DE CHAQUE POIDS AU MOTS POUR LE RECODAGE 3 AVEC LE CLASSIFIEUR RF
##
######################################################################################

# ##### DATA
# data = pd.read_csv('data_Recodage3_lemm_NEW_NEW_colle.csv', sep = ';', encoding = 'latin1')

# ####

# vectorizer = CountVectorizer(analyzer='word',token_pattern=r'\w{1,}', ngram_range=(1, 3))

# RF = RandomForestClassifier(random_state=0)
# l1 = [[0 for i in range(13)] for j in range(616)]
# c = make_pipeline(vectorizer, RF)
# class_names=[1,2,3]
# explainer = LimeTextExplainer(class_names=class_names)

# x1 = range(0, 616, 8)
# a=0

# for i in x1 :
#     X_train = data["Recodage3"].tolist()
#     y_train = data["Classe.hedonique"].tolist()
#     X_test = X_train[i:(i+8)]
#     y_test = y_train[i:(i+8)]
#     train_vectors = vectorizer.fit_transform(X_train)
#     test_vectors = vectorizer.transform(X_test)
#     RF.fit(train_vectors,y_train)
#     for j in range(0,8):
#         exp = explainer.explain_instance(X_test[j], c.predict_proba, num_features=25, labels=[0,1,2])
#         n=len(exp.as_list(label=0))
#         for k in range(0,n):
#             l1[i+j][0] = X_test[j]
#             l1[i+j][1] = class_names[(RF.predict(test_vectors[j]).reshape(1,-1)[0,0])-1]
#             l1[i+j][2] = class_names[y_test[j]-1]
#             l1[i+j][3+k] = exp.as_list(label=0)[k]
   
# print(l1)

# res_1 = np.asarray(l1)
# res_1_df = pd.DataFrame(res_1)
# res_1_df.to_csv("res_poids_LIME_Recodage3_colle.csv",sep=";",encoding="latin1")






