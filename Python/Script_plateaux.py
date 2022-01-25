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
from numpy import append
import matplotlib.pyplot as plt
import random 
import cv2 
import os

##################################
# Chargement du dataset avec keras
##################################
# Création des jeux de données

data_dir = "E:/Master 2/Simulateddata/data/plateau/Connu/"

batch_size = 50
img_height = 201
img_width = 256

# Jeu de données d'apprentissage
train = tf.keras.utils.image_dataset_from_directory(
  data_dir,
  labels="inferred",
  validation_split=0.2,
  subset="training",
  seed=123,
  shuffle = True,
  image_size=(img_height, img_width),
  batch_size=batch_size)

# Jeu de données de validation
test = tf.keras.utils.image_dataset_from_directory(
  data_dir,
  labels="inferred",
  validation_split=0.2,
  subset="validation",
  seed=123,
  shuffle = True,
  image_size=(img_height, img_width),
  batch_size=batch_size)

for i in range(1,len(test)+1):
    for images, labels in test.take(i):
        imgg = images.numpy()
        labs = labels.numpy()
    if i == 1:
        test_images = imgg
        test_labels = labs
    if i == 2:
        test_images_fin = np.append(test_images,imgg)
        test_labels_fin = np.append(test_labels,labs)
    if i > 2 :
        test_labels_fin = np.append(test_labels_fin,labs)   
        test_images_fin = np.append(test_images_fin,imgg)

test.take(1)

test_images_fin = test_images_fin.reshape(160,201,256,3)

##################################
# Construction du modèle
##################################
num_classes = 2
model = tf.keras.Sequential([
  # tf.keras.layers.Rescaling(1./255), # Normalisation des valeurs des canaux RVB
  tf.keras.layers.Conv2D(filters=16, kernel_size=(2, 2), activation="relu"),
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
  loss=tf.losses.SparseCategoricalCrossentropy(from_logits=False),
  metrics=['accuracy'])

epochs = 10
history = model.fit(
  train, validation_data = test,
  epochs=epochs)

test_pred = model.predict(test_images_fin)

##################################
# Evaluation de la performance du modèle (test)
##################################
print("Evaluation de la performance avec le jeu test")
model.evaluate(
    test, 
    verbose=2)

##################################
# Prédictions pour des images avec des heatmaps non labellisées
##################################
    
# data_dir = "E:/Master 2/Simulateddata/data/fake/unknowed"
# batch_size = 20
# img_height = 64
# img_width = 64

# Img_gen = tf.keras.utils.image_dataset_from_directory(
#   data_dir,
#   labels=None,
#   seed=123,
#   image_size=(img_height, img_width),  
#   batch_size=batch_size,
#   shuffle = False)

# predictions = model.predict(Img_gen)

# # for i in range(len(predictions)):
# #     print("Photo A-",i+1)
# #     print(predictions[i].round(3)) 
# #     print(np.argmax(predictions[i]))

# count = 0

# for i in range(len(test_images_fin)):
#     if np.argmax(test_pred[i]) != test_labels_fin[i]:
#         count = count + 1
#     plt.imshow(test_images_fin[i].astype("uint8"))
#     plt.title("Predict : " + str(np.argmax(test_pred[i])) +
#               " | True :" +
#                 str(test_labels_fin[i]) +
#                 "\n " + "Errors : " + str(count), fontstyle='italic')
#     plt.axis("off")
#     dirname = "E:/Master 2/Simulateddata/data/plateau/test_verif"
#     name = 'Image' + str(i+1)
    # plt.savefig(os.path.join(dirname,name))
    


# for j in range(1,len(Img_gen)+1):
#     for images in Img_gen.take(j):
#         for i in range(batch_size):
#             # ax.set_title(np.argmax(predictions[i]), fontstyle='italic')
#             plt.imshow(images[i].numpy().astype("uint8"))
#             plt.title("Predict : " + str(np.argmax(predictions[i])), fontstyle='italic')
#             plt.axis("off")
#             dirname = "E:/Master 2/Simulateddata/data/fake/predict"
#             name = 'Image' + str(i) + 'batch_n' + str(j)
#             plt.savefig(os.path.join(dirname,name))
    

