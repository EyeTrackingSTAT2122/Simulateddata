# -*- coding: utf-8 -*-
"""
Created on Wed Oct 13 16:53:30 2021

@author: Clara Laudine Elias
"""
# Problématique : Déterminer si le bruit autour d'un stimulus influence la classe d'affectation

##################################
# Importation des packages 
##################################
import tensorflow as tf
import numpy as np
from PIL import Image

##################################
# Chargement du dataset avec keras
##################################
# Création des jeux de données
data_dir = "C:/Simulateddata/Photos"
batch_size = 50
img_height = 64
img_width = 64

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
  tf.keras.layers.Conv2D(filters=16, kernel_size=(3, 3), activation="relu"),
  tf.keras.layers.MaxPooling2D(),
  tf.keras.layers.Conv2D(filters=32, kernel_size=(3, 3), activation="relu"),
  tf.keras.layers.MaxPooling2D(),
  tf.keras.layers.Conv2D(filters=64, kernel_size=(3, 3), activation="relu"),
  tf.keras.layers.MaxPooling2D(),
  tf.keras.layers.Flatten(),
  tf.keras.layers.Dense(128, activation='relu'),
  tf.keras.layers.Dense(num_classes, activation="softmax")
])

##################################
# Entrainement du modèle (train)
##################################
model.compile(
  optimizer='adam',
  loss=tf.losses.SparseCategoricalCrossentropy(from_logits=True),
  metrics=['accuracy'])

epochs = 10
history = model.fit(
  train,
  epochs=epochs)

##################################
# Evaluation de la performance du modèle (test)
##################################
print("Evaluation de la performance avec le jeu de données test")
model.evaluate(
    test, 
    verbose=2)

##################################
# Ajouter du bruit aux lettres
##################################
import random 
import cv2 

def add_noise(img): 
    row , col = 64, 64
    number_of_pixels = random.randint(2000 , 3000) 
    for i in range(number_of_pixels): 
        y_coord=random.randint(0, row - 1) 
        x_coord=random.randint(0, col - 1) 
        img[y_coord][x_coord] = 0       
    return img 

nb_images = 10
for i in range(0,nb_images-1):
    name = "Image"+str(i)+".png"
    img = cv2.imread('C:/Simulateddata/A.jpg')
    img_bruit = add_noise(img)
    cv2.imwrite(name, img_bruit)


##################################
# Prédictions pour des images avec du bruit
##################################
    
# probability_model = tf.keras.Sequential([model, 
#                                           tf.keras.layers.Softmax()]) # Conversion des logits en probabilités

# predictions = probability_model.predict(bruit)

# print(predictions[0]) # 1ère prédiction
# print(np.argmax(predictions[0])) # Etiquette avec la valeur de confiance la plus élevée pour la 1ère prédiction

 

