# -*- coding: utf-8 -*-
"""
Created on Wed Dec  1 10:51:56 2021

@author: UP math
"""
import tensorflow as tf

# Création des jeux de données
path = 'C:\Simulateddata\A.jpg'
batch_size = 50
img_height = 64
img_width = 64

import imageio as iio
from scipy import misc
import numpy as np
import matplotlib.pyplot as plt

from matplotlib import pyplot as plt


test = tf.keras.preprocessing.image.load_img(
    path,grayscale=True, color_mode="grayscale", target_size=(img_height, img_width)
)

test2 = tf.keras.preprocessing.image.img_to_array(test, data_format=None, dtype=None)

salt_value = 40

noise = np.random.randint(salt_value+1, size=(64, 64))

#---------- Pepper ----------#

indexe = np.where(noise == 0)

for i in (range(len(test2))):
    for j in (range(len(test2))):
        if test2[j,i,0] + noise[j,i] < 254:
            M[j,i] = test2[j,i,0] + noise [j,i]
        else: 
            M[j,i] = test2[j,i,0]     
    print(M)




M = test2[:,:,0] + noise

plt.imshow(M, interpolation='nearest')
plt.show()
