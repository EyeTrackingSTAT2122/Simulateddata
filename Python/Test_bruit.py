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
import random 
import cv2 
import os
import matplotlib.pyplot as plt

##################################
# Chargement du dataset avec keras
##################################
# Création des jeux de données

data_dir = "C:/Simulateddata/Photos_initiales"

batch_size = 10 #On en a 400 au total et on en enlève 20% pour le test

batch_size = 10
img_height = 64
img_width = 64

# Jeu de données d'apprentissage
train = tf.keras.utils.image_dataset_from_directory(
  data_dir,
  labels="inferred",
  label_mode = "binary",
   validation_split=0.2,
   subset="training",
  seed=123,
  image_size=(img_height, img_width),
  batch_size=batch_size)


for images, labels in train.take(-1):  # only take first element of dataset
    train_images = images.numpy()
    train_labels = labels.numpy()

# Jeu de données de validation
test = tf.keras.utils.image_dataset_from_directory(
  data_dir,
  labels="inferred",
  label_mode = "binary",
  validation_split=0.2,
  subset="validation",
  seed=123,
  image_size=(img_height, img_width),
  batch_size=batch_size)


for images, labels in test[1]:  # only take first element of dataset
    test_images = images.numpy()
    test_labels = labels.numpy()
    
    
##################################
# Construction du modèle
##################################
num_classes = 2
model = tf.keras.Sequential([
  tf.keras.layers.Rescaling(1./255), # Normalisation des valeurs des canaux RVB
  tf.keras.layers.Conv2D(filters=4, kernel_size=(2, 2), activation="relu"),
  tf.keras.layers.MaxPooling2D(),
  # tf.keras.layers.Conv2D(filters=32, kernel_size=(3, 3), activation="relu"),
  # tf.keras.layers.MaxPooling2D(),
  # tf.keras.layers.Conv2D(filters=64, kernel_size=(3, 3), activation="relu"),
  # tf.keras.layers.MaxPooling2D(),
  tf.keras.layers.Flatten(),
  # tf.keras.layers.Dense(128, activation='relu'),
   tf.keras.layers.Dense(num_classes,activation="softmax")
  # tf.keras.layers.Dense(num_classes,activation="sigmoid")
])

##################################
# Entrainement du modèle (train)
##################################
model.compile(
  optimizer='adam',
  # loss=tf.keras.losses.BinaryCrossentropy(from_logits=True),
  loss=tf.keras.losses.SparseCategoricalCrossentropy(from_logits=True),
  metrics=['accuracy'])

epochs = 10
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
# Ajouter du bruit aux lettres
##################################
def add_noise(img): 
    row , col = 64, 64
    number_of_pixels = random.randint(300 , 10000) 
    for i in range(number_of_pixels): 
        y_coord=random.randint(0, row - 1) 
        x_coord=random.randint(0, col - 1) 
        img[y_coord][x_coord] = 0       
    return img 

img = cv2.imread('D:/Eye-tracking/Simulateddata/A.jpg')
    

dirname = "D:/Eye-tracking/Simulateddata/Photos_bruit"
nb_images = 10
for i in range(0,nb_images):
    name = "Image"+str(i)+".png"
    img = cv2.imread('C:/Simulateddata/A.jpg')
    img_bruit = add_noise(img)
    cv2.imwrite(os.path.join(dirname, name), img_bruit)

##################################
# Prédictions pour des images avec du bruit
##################################
    
data_dir = "C:/Simulateddata/Vertical_wave"
batch_size = 50
data_dir = "D:/Eye-tracking/Simulateddata/Photos_bruit"
batch_size = 10

img_height = 64
img_width = 64


bruit = tf.keras.utils.image_dataset_from_directory(
  data_dir,
  labels=None,
  seed=123,
  image_size=(img_height, img_width),  
  batch_size=batch_size)

predictions = model.predict(bruit)

count = 0
for i in range(len(predictions)):
    print("Photo A-",i+1)
    print(predictions[i].round(3)) 
    print(np.argmax(predictions[i]))
    count = count + np.argmax(predictions[i])
print(count)


import cv2
import numpy as np
import math

img = cv2.imread('images/input.jpg', cv2.IMREAD_GRAYSCALE)
rows, cols = img.shape


#####################
# Vertical wave
img = cv2.imread('C:/Simulateddata/A.jpg',cv2.IMREAD_GRAYSCALE)
img_output = np.zeros(img.shape, dtype=img.dtype)
rows, cols = img.shape

dirname = "C:/Simulateddata/Vertical_wave/0"


for k in range(1,101):
    if random.randint(0,1) == 0 :
        for i in range(rows):
            for j in range(cols):
                offset_x = int(25 * math.sin(2 * 3.14 * i / k))
                offset_y = int(16.0 * math.sin(2 * 3.14 * j / k))
                if j+offset_x < rows:
                    img_output[i,j] = img[i,(j+offset_x)%cols]
                else:
                    img_output[i,j] = 254
            name = "Vertical wave" + str(k)+".jpg"
            cv2.imwrite(os.path.join(dirname,name), img_output)
    else:
         for i in range(rows):
            for j in range(cols):
                offset_x = int(25 * math.sin(2 * 3.14 * i / (- k)))
                offset_y = int(16.0 * math.sin(2 * 3.14 * j / k))
                if j+offset_x < rows:
                    img_output[i,j] = img[i,(j+offset_x)%cols]
                else:
                    img_output[i,j] = 254
    name = "Vertical wave" + str(k)+".jpg"
    cv2.imwrite(os.path.join(dirname,name), img_output)

# Vertical wave
img = cv2.imread('C:/Simulateddata/C.jpg',cv2.IMREAD_GRAYSCALE)
img_output = np.zeros(img.shape, dtype=img.dtype)
rows, cols = img.shape

dirname = "C:/Simulateddata/Vertical_wave/1"


for k in range(1,101):
    if random.randint(0,1) == 0 :
        for i in range(rows):
            for j in range(cols):
                offset_x = int(25 * math.sin(2 * 3.14 * i / k))
                offset_y = int(16.0 * math.sin(2 * 3.14 * j / k))
                if j+offset_x < rows:
                    img_output[i,j] = img[i,(j+offset_x)%cols]
                else:
                    img_output[i,j] = 254
            name = "Vertical wave" + str(k)+".jpg"
            cv2.imwrite(os.path.join(dirname,name), img_output)
    else:
         for i in range(rows):
            for j in range(cols):
                offset_x = int(25 * math.sin(2 * 3.14 * i / (- k)))
                offset_y = int(16.0 * math.sin(2 * 3.14 * j / k))
                if j+offset_x < rows:
                    img_output[i,j] = img[i,(j+offset_x)%cols]
                else:
                    img_output[i,j] = 254
    name = "Vertical wave" + str(k)+".jpg"
    cv2.imwrite(os.path.join(dirname,name), img_output)
    

data_dir = "C:/Simulateddata/Vertical_wave"
batch_size = 50
img_height = 64
img_width = 64


bruit = tf.keras.utils.image_dataset_from_directory(
  data_dir,
  labels="inferred",
  label_mode = "binary",
  seed=123,
  shuffle = True,
  image_size=(img_height, img_width),  
  color_mode = "rgb",
  batch_size=batch_size)


bruit_images= []
bruit_labels= []
bruits_label_tot = []

for i in range(1,len(bruit)):
        for images, labels in bruit.batch(i):  # only take first element of dataset
            bruit_images = images.numpy()
            bruit_labels = labels.numpy().tolist()
        bruits_label_tot = bruits_label_tot + bruit_labels

bruit_labs = []
for i in (range(len (bruits_label_tot))):
    bruit_labs.extend(bruits_label_tot[i])

predictions = model.predict(bruit)

juste = 0
count = 0
for i in range(len(bruit_labs)):
    print("Photo A",i+1)
    print(predictions[i].round(3)) 
    print("Prediction :", np.argmax(predictions[i]), "\nTrue class :", int(bruit_labs[i][0]))
    if bruit_labs[i] ==  np.argmax(predictions[i]) :
        juste = juste + 1
    count = count + np.argmax(predictions[i])
print(count)
print(juste)

# plt.figure(figsize=(10, 10))
# for i in range(100):
#     ax = plt.subplot(10, 10, i + 1)
#     plt.imshow(bruit_images[i].astype("uint8"))
#     plt.axis("off")
# plt.savefig("da_autre.png", dpi = 200)

#####################
# Horizontal wave

img_output = np.zeros(img.shape, dtype=img.dtype)

for i in range(rows):
    for j in range(cols):
        offset_x = 0
        offset_y = int(16.0 * math.sin(2 * 3.14 * j / 150))
        if i+offset_y < rows:
            img_output[i,j] = img[(i+offset_y)%rows,j]
        else:
            img_output[i,j] = 0

cv2.imshow('Horizontal wave', img_output)

#####################
# Both horizontal and vertical 

img_output = np.zeros(img.shape, dtype=img.dtype)

for i in range(rows):
    for j in range(cols):
        offset_x = int(20.0 * math.sin(2 * 3.14 * i / 150))
        offset_y = int(20.0 * math.cos(2 * 3.14 * j / 150))
        if i+offset_y < rows and j+offset_x < cols:
            img_output[i,j] = img[(i+offset_y)%rows,(j+offset_x)%cols]
        else:
            img_output[i,j] = 0

cv2.imshow('Multidirectional wave', img_output)

#####################
# Concave effect

img_output = np.zeros(img.shape, dtype=img.dtype)

for i in range(rows):
    for j in range(cols):
        offset_x = int(128.0 * math.sin(2 * 3.14 * i / (2*cols)))
        offset_y = 0
        if j+offset_x < cols:
            img_output[i,j] = img[i,(j+offset_x)%cols]
        else:
            img_output[i,j] = 0

cv2.imshow('Concave', img_output)

cv2.waitKey()
