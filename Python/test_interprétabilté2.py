# -*- coding: utf-8 -*-
"""
Created on Mon Dec  6 14:24:34 2021

@author: Clara
"""

from tensorflow.keras.layers import Conv2D, MaxPooling2D, Flatten, Dense, Dropout, Activation, BatchNormalization
from tensorflow.keras.models import Sequential, Model
from tensorflow.keras.optimizers import Adam
from tensorflow.keras.preprocessing.image import ImageDataGenerator,DirectoryIterator
from tensorflow.keras.datasets import mnist
import numpy as np
import matplotlib.pyplot as plt
import matplotlib.image as mpimg
from keras import backend as K
import os
import tensorflow as tf
import skimage
from skimage import io
from skimage import transform
from tensorflow.keras.preprocessing import image



# Path to train and test directory
dir_ = os.path.join('C:/Users/Clara/Documents/Agro/5A/Projet Ingé/Simulateddata/data/plateau/Connu')


# Generate training and test data with Image Generator
train_datagen = ImageDataGenerator(validation_split = 0.2)


train_generator = train_datagen.flow_from_directory(dir_,target_size=(64, 64),
                                                   batch_size= 10,
                                                   class_mode='categorical',
                                                   shuffle=False,
                                                   subset = 'training')

test_generator = train_datagen.flow_from_directory(dir_,
                                                          target_size = (64,64),
                                                          batch_size = 10,
                                                          class_mode = 'categorical',
                                                          shuffle=False,
                                                          subset = 'validation')


# Fetch the data and the labels
x_train, y_train = next(train_generator)
x_test, y_test  = next(test_generator)

# Fix the filepath
test_filepath = []
for filepath in test_generator.filepaths:
    filepath = filepath.replace('\\', '/')
    test_filepath.append(filepath)
    
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

model.compile(optimizer='adam', loss='categorical_crossentropy', metrics=['accuracy'])

history = model.fit(
      train_generator,  
      epochs=20,
      verbose=1)

def read_and_transform_img(path):

    img = skimage.io.imread(path)
    img = skimage.transform.resize(img, (64,64))
    
    img = image.img_to_array(img)
    img = np.expand_dims(img, axis=0)

    return img


import glob
from skimage.segmentation import mark_boundaries

path = glob.glob("C:/Users/Clara/Documents/Agro/5A/Projet Ingé/test/*.png")
cv_img = []
for img in path:
    n = read_and_transform_img(img)
    cv_img.append(n)
print(cv_img)

from lime import lime_image

explainer = lime_image.LimeImageExplainer()

for i in range (len(cv_img)) :
    preds = model.predict(cv_img[i])
    prediction = np.argmax(preds)
    pct = np.max(preds)
    print(prediction)
    print(pct)
    
for img in cv_img:   
    explanation = explainer.explain_instance(img[0].astype('double'), model.predict,  
                                         top_labels=2, hide_color=0, num_samples=100)
    temp_1, mask_1 = explanation.get_image_and_mask(explanation.top_labels[0], positive_only=True, num_features=5, hide_rest=True)
    temp_2, mask_2 = explanation.get_image_and_mask(explanation.top_labels[0], positive_only=False, num_features=10, hide_rest=False)
    fig, (ax1, ax2) = plt.subplots(1, 2, figsize=(15,15))
    ax1.imshow(mark_boundaries(temp_1, mask_1))
    ax2.imshow(mark_boundaries(temp_2, mask_2))
    ax1.axis('off')
    ax2.axis('off')

