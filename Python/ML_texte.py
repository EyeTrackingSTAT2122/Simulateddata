# -*- coding: utf-8 -*-
"""
Created on Tue Jan 11 13:53:56 2022

@author: UP math
"""

# Import des packages

#import os #import des fichiers
import pandas as pd # création de df
import matplotlib.pyplot as plt # plots
import re # nettoyage des données

# repartition des echantillon train et test
from sklearn.model_selection import train_test_split
from tensorflow.keras.preprocessing.text import Tokenizer # tokenization
import tensorflow as tf # word to seq

import numpy as np

# evaluation des modèles
from sklearn.metrics import accuracy_score, confusion_matrix, precision_score
import seaborn as sns # representation graphique de la matrice de confusion

import os

# Renseignement du répertoire de travail
os.chdir('E:/Master 2/Simulateddata/scriptR/study')

# paramètres graphiques
plt.style.use('ggplot')

# -----------------------------------------------------------------------------------------

# definition des fonctions

# normalisation on enlève les url, les espaces en trop, etc. 
def normalize(df):
    normalized = []
    for i in df:
        i = i.lower()
        # get rid of urls
        i = re.sub('https?://\S+|www\.\S+', '', i)
        # get rid of non words and extra spaces
        i = re.sub('\\W', ' ', i)
        i = re.sub('\n', '', i)
        i = re.sub(' +', ' ', i)
        i = re.sub('^ ', '', i)
        i = re.sub(' $', '', i)
        normalized.append(i)
    return normalized

# préparation du jeu d'entrainement

def prep_train(data, max_vocab) : 
    data = normalize(data)
    tokenizer = Tokenizer(num_words=max_vocab)
    tokenizer.fit_on_texts(data) 
    # tokenize the text into vectors 
    data = tokenizer.texts_to_sequences(data)
    #mise sous forme d'une matrice de longeur 100, les séquences plus courtes sont ralongées avec une valeur 
    data = tf.keras.preprocessing.sequence.pad_sequences(data, 
                                                         padding='post', 
                                                         maxlen=256)
    return (data)
       

def test(data, y_res, mod, max_vocab): # data = le X à tester, res = la var réponse à comparer, model = modèle entrainé
    data = normalize(data)
    # tokenization
    tokenizer = Tokenizer(num_words=max_vocab)
    tokenizer.fit_on_texts(data)
    # création du vecteur
    data = tokenizer.texts_to_sequences(data)
    # tensorflow
    data = tf.keras.preprocessing.sequence.pad_sequences(data, 
                                                         padding='post', 
                                                         maxlen=256)
    # prediction 
    predicted_value = mod.predict(data)
    binary_predictions = []
    for i in predicted_value:
        if i >= 0.5:
            binary_predictions.append(1)
        else:
            binary_predictions.append(0) 
    #Matrice de confusion
    matrix = confusion_matrix(binary_predictions, np.asarray(y_test).astype('float32'), normalize='all')
    ## Valeur d'accuracy et de précision      
    print('Accuracy on testing set:', accuracy_score(binary_predictions, np.asarray(y_test).astype('float32')))
    print('Precision on testing set:', precision_score(binary_predictions, np.asarray(y_test).astype('float32')))
    return(matrix)

# création des graphs (matrice de confusion)

def graph(conf_matrix): # conf_matrix = matrice de confusion
    # params graphiques
    plt.figure(figsize=(16, 10))
    ax= plt.subplot()
    # labels, title and ticks
    ax.set_xlabel('Predicted Labels', size=20)
    ax.set_ylabel('True Labels', size=20)
    ax.set_title('Confusion Matrix', size=20) 
    ax.xaxis.set_ticklabels([0,1], size=15)
    ax.yaxis.set_ticklabels([0,1], size=15)
    # heatmap
    sns.heatmap(conf_matrix, annot=True, ax = ax)
    
# Concatenation des colones text et titre
def clean_data(df):
    df['Coms'] = df['Commentaires harmonises']
    del df['Commentaires harmonises']
    del df['ID']
    del df['Num_sti']
    del df['Pays']
    return df


# ------------------------------------------------------------------------------------------   

max_vocab =10000  

# -------------------------------------------------------------------------------------------

# import des données

df = pd.read_csv("E:/Master 2/Simulateddata/scriptR/study/ML_Comm.csv")

clean_data(df)


for i in range(len(df)):
    if df.iloc[i,0] == 'Pas equilibre' :
        df.iloc[i,0] = 0
    elif df.iloc[i,0] == 'Equilibre' : 
        df.iloc[i,0] = 2
    else :
        df.iloc[i,0] = 1
        




# -------------------------------------------------------------------------------------------

# repartition des echantillon train et test
max_vocab =10000
X_train, X_test, y_train, y_test = train_test_split(df.Coms, 
                                                    df.Classe, 
                                                    test_size = 0.2,
                                                    random_state=2)

X_train = prep_train(X_train, max_vocab)

X_test = prep_train(X_test, max_vocab)

# --------------------------------------------------------------------------------------------
# Méthode de machine learning : random forest

# Modèle de Deep Learning

print('Méthode de deep learning: RNN from scratch')

mod_RNN = tf.keras.Sequential([
    tf.keras.layers.Embedding(max_vocab, 32),
    tf.keras.layers.Bidirectional(tf.keras.layers.LSTM(64)),
    tf.keras.layers.Dense(2, activation='softmax'), # fonction sigmoïde car 2 classes 
    tf.keras.layers.Dropout(0.2), # aide à réduire le risque de sur-apprentissage 
])
# mod_RNN.summary()

#on stop le modèle quand l'accuracy n'augmente plus
early_stop = tf.keras.callbacks.EarlyStopping(monitor='val_loss', 
                                              patience=2, 
                                              restore_best_weights=True)

mod_RNN.compile(loss=tf.keras.losses.BinaryCrossentropy(from_logits=False), # changer l'argument from logits quand sigmoid --> FALSE
              optimizer=tf.keras.optimizers.Adam(1e-4),
              metrics=['accuracy'])

mod_RNN.fit(np.asarray(X_train).astype('float32'), 
            np.asarray(y_train).astype('float32'), 
            epochs=100, 
            validation_split=0.1, 
            batch_size=30, 
            shuffle=True,)
            # callbacks=[early_stop])


print('##################################################################')
# -------------------------------------------------------------------------------------------
##### Tests du modèle -----------------------------------------------------------------------
print ('test des modèles')

print("méthode : Deep Learning - jeu test : échantillon du jeu de données")
# test du modèle de Deep Learning sur un échantillon issu du jeu de données


# prediction 
predicted_value = mod_RNN.predict(np.asarray(X_test).astype('float32'))

pred_class = []

for i in range(len(predicted_value)):
    if predicted_value[i,0] > predicted_value[i,1]:
        if predicted_value[i,0] > predicted_value[i,2] :
            pred_class.append(0)
        else:
            pred_class.append(2)
    if predicted_value[i,1] > predicted_value[i,0]:
        if predicted_value[i,1] > predicted_value[i,2] :
            pred_class.append(1)
        else:
            pred_class.append(2)
    #Matrice de confusion
matrix = confusion_matrix(predicted_value, np.asarray(y_test).astype('float32'), normalize='all')


predicted_value = mod_RNN.predict(np.asarray(X_test).astype('float32'))
binary_predictions = []
for i in range(len(predicted_value)):
    if predicted_value[i,0] > predicted_value[i,1]:
        binary_predictions.append(0)
    else:
        binary_predictions.append(1)
#Matrice de confusion
matrix = confusion_matrix(binary_predictions, y_test)


test_dataset_DL = test(X_test,y_test,mod_RNN, 10000)
graph(matrix)

print('##################################################################')
print("méthode : Deep Learning - jeu test : jeu de données personnelles traduites")
# test sur un jeu de données personnel

own_data = pd.read_excel("notre_base.xlsx", header=None)
categorie = [0] * 16 + [1] * 15
own_data['category'] = categorie
own_data = own_data.rename(columns={0: 'text'})
own_data = own_data.sample(frac = 1)


test_own_data = test(own_data.text,own_data.category,mod_RNN, 10000)
graph(test_own_data)

print('##################################################################')