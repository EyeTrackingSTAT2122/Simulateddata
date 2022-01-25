# -*- coding: utf-8 -*-
"""
Created on Wed Oct 13 16:53:30 2021

@author: Clara Laudine Elias
"""
# Problématique : Déterminer si le classifieur est 

##################################
# Importation des packages 
##################################
import tensorflow as tf
import numpy as np
import random 
import cv2 
import os

##################################
# Chargement du dataset avec keras
##################################
# Création des jeux de données
data_dir = "D:/Eye-tracking/Simulateddata/C_heat_map"
batch_size = 10
img_height = 256
img_width = 256

# Jeu de données d'apprentissage
train = tf.keras.utils.image_dataset_from_directory(
  data_dir,
  labels="inferred",
  validation_split=0.2,
  subset="training",
  seed=123,
  image_size=(img_height, img_width),
  batch_size=batch_size)

# Jeu de données de validation
test = tf.keras.utils.image_dataset_from_directory(
  data_dir,
  labels="inferred",
  validation_split=0.2,
  subset="validation",
  seed=123,
  image_size=(img_height, img_width),
  batch_size=batch_size)

##################################
# Construction du modèle
##################################
num_classes = 2
model = tf.keras.Sequential([
  tf.keras.layers.Rescaling(1./255), # Normalisation des valeurs des canaux RVB
  tf.keras.layers.Conv2D(filters=4, kernel_size=(2, 2), activation="relu"),
  tf.keras.layers.MaxPooling2D(),
  tf.keras.layers.Conv2D(filters=16, kernel_size=(3, 3), activation="relu"),
  tf.keras.layers.MaxPooling2D(),
  tf.keras.layers.Conv2D(filters=32, kernel_size=(3, 3), activation="relu"),
  tf.keras.layers.MaxPooling2D(),
  tf.keras.layers.Flatten(),
  tf.keras.layers.Dense(num_classes, activation="softmax")
])

##################################
# Entrainement du modèle (train)
##################################
model.compile(
  optimizer='adam',
  loss=tf.losses.SparseCategoricalCrossentropy(from_logits=True),
  metrics=['accuracy'])

epochs = 8
history = model.fit(
  train,
  epochs=epochs)

##################################
# Evaluation de la performance du modèle (test)
##################################
print("Evaluation de la performance avec le jeu test")
model.evaluate(
    test, 
    verbose=2)

##################################
# Prédictions pour des nouvelles images 
##################################
data_dir = "D:/Eye-tracking/Simulateddata/data_test_C"
batch_size = 10
img_height = 256
img_width = 256

bruit = tf.keras.utils.image_dataset_from_directory(
  data_dir,
  labels=None,
  seed=123,
  image_size=(img_height, img_width),
  batch_size=batch_size)

predictions = model.predict(bruit)

count = 0
for i in range(len(predictions)):
    print("Photo C-",i)
    print(predictions[i].round(3)) 
    print(np.argmax(predictions[i]))
    # count = count + np.argmax(predictions[i])

# print("================")
# print(f"Il y a {count} erreurs pour {len(predictions)} photos")

    # if np.argmax(predictions[i]) == label[i] :
    #     print("Ok")
    # else:
    #     print("Pas ok")
    