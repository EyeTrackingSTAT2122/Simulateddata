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
import os
from tensorflow import keras
from tensorflow.keras import layers
from tensorflow.keras.layers import Conv2D, MaxPooling2D, Flatten, Dense, Dropout, Activation, BatchNormalization, Rescaling, RandomFlip, RandomRotation
from tensorflow.keras.models import Sequential, Model
# from tensorflow.keras.optimizers import Adam
# from tensorflow.keras.preprocessing.image import ImageDataGenerator,DirectoryIterator
# from tensorflow.keras.datasets import mnist
# import matplotlib.image as mpimg
from keras import backend as K
# import skimage
# from skimage import io
# from skimage import transform
from tensorflow.keras.preprocessing import image
from skimage.segmentation import mark_boundaries
from lime import lime_image

##################################
# Chargement du dataset avec keras
##################################
# Création des jeux de données


data_augmentation = keras.Sequential(
    [
        RandomFlip("horizontal_and_vertical"),
        RandomRotation(0.2),
        # layers.RandomZoom(.5,.2)
    ]
)

data_dir = "E:/Master 2/Simulateddata/data/plateau/inter/"

batch_size = 2
img_height = int(3840/10)
img_width = int(11520/10)

# Jeu de données d'apprentissage
train = tf.keras.utils.image_dataset_from_directory(
  data_dir,
  labels="inferred",
  validation_split=0.2,
  subset="training",
  label_mode = "binary",
  seed=123,
  shuffle = True,
  image_size=(img_height,img_width),
  batch_size=batch_size)

# Jeu de données de validation
test = tf.keras.utils.image_dataset_from_directory(
  data_dir,
  labels="inferred",
  validation_split=0.2,
  subset="validation",
  label_mode = "binary",
  seed=123,
  shuffle = True,
  image_size=(img_height,img_width),
  batch_size=batch_size)


plt.figure(figsize=(10, 10))
for images, labels in train.take(1):
    for i in range(9):
        ax = plt.subplot(3, 3, i + 1)
        plt.imshow(images[i].numpy().astype("uint8"))
        plt.axis("off")


cv_img = []

for i in range(1,len(test)+1):
    for images, labels in test.take(i):
        imgg = images.numpy()
        labs = labels.numpy()
    for j in range(batch_size):
        current = imgg[j]
        current = image.img_to_array(current)
        current = np.expand_dims(current, axis=0)
        current = current / 255
        cv_img.append(current)
    if i == 1:
        test_images = imgg
        test_labels = labs
    if i == 2:
        test_images_fin = np.append(test_images,imgg)
        test_labels_fin = np.append(test_labels,labs)
    if i > 2 :
        test_labels_fin = np.append(test_labels_fin,labs)   
        test_images_fin = np.append(test_images_fin,imgg)
    print(i," sur ", len(test))
    
    
del test_images,test_labels,imgg,labs, j, i, labels, images, current

test_images_fin = test_images_fin.reshape(160,img_width,img_height,3)

##################################
# Construction du modèle 
##################################

if K.image_data_format() == 'channels_first':
    input_shape = (3,img_height, img_width)
else:
    input_shape = (img_width, img_height, 3)


num_classes = 2

inputs = keras.Input(shape=input_shape)

# model = Sequential()
x = (Rescaling(1./255))(inputs)
x = data_augmentation(x)
x = BatchNormalization()(x)
x = Conv2D(32, (3, 3))(x)

x = Activation('relu')(x)
x = MaxPooling2D(pool_size=(2, 2))(x)

x = Conv2D(32, (3, 3))(x)
x = Activation('relu')(x)
x = MaxPooling2D(pool_size=(2, 2))(x)

x = Conv2D(64, (3, 3))(x)
x = Activation('relu')(x)
x = MaxPooling2D(pool_size=(2, 2))(x)

x = Flatten()(x)
x = Dense(64)(x)
x = Activation('relu')(x)
x = Dropout(0.4)(x)
outputs = layers.Dense(activation="sigmoid")(x)

model = keras.Model(inputs=inputs, outputs=outputs)

##################################
# Entrainement du modèle (train)
##################################
model.compile(optimizer='adam', loss='binary_crossentropy', metrics=['accuracy'])


epochs = 10
history = model.fit(
  train, validation_data = test,
  epochs=epochs, batch_size = batch_size)

plt.plot(history.history["loss"])
plt.ylabel("Loss")
plt.xlabel("Epoch")
plt.plot(history.history["val_loss"])
# plt.savefig("loss.png", dpi=200)
plt.show()

# accuracies du modèle
plt.plot(history.history["accuracy"], color="purple", label="Train")
plt.legend("Accuracy")
plt.ylabel("Accuracy")
plt.xlabel("Epoch")
plt.plot(history.history["val_accuracy"], color="forestgreen", label="Test")
plt.legend()
# plt.savefig("accuracy.png", dpi=200)
plt.show()



test_pred = model.predict(test)

##################################
# Evaluation de la performance du modèle (test)
##################################
# print("Evaluation de la performance avec le jeu test")
# model.evaluate(
#     test, 
#     verbose=2)

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
#     plt.savefig(os.path.join(dirname,name))
    


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
    

#############################################################################


explainer = lime_image.LimeImageExplainer()

dirname = "E:/Master 2/Simulateddata/data/plateau/explication_inter"


cut = cv_img[1:5]
i = 0
for img in cut:
    explanation = explainer.explain_instance(img[0].astype('double'), model.predict, top_labels=2, hide_color=0, num_samples=100)
    temp_1, mask_1 = explanation.get_image_and_mask(explanation.top_labels[0], positive_only=True, num_features=5, hide_rest=True)
    temp_2, mask_2 = explanation.get_image_and_mask(explanation.top_labels[0], positive_only=False, num_features=10, hide_rest=False)
    fig, (ax1, ax2) = plt.subplots(1, 2, figsize=(15,15))
    ax1.imshow(mark_boundaries(temp_1, mask_1))
    ax2.imshow(mark_boundaries(temp_2, mask_2))
    ax1.axis('off')
    ax2.axis('off')
    name = 'Explication_Image' + str(i+1)
    plt.savefig(os.path.join(dirname,name))