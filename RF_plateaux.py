import tensorflow as tf
import numpy as np
import pandas as pd

from sklearn.metrics import classification_report,confusion_matrix

# libraries for random forest
from sklearn.ensemble import RandomForestClassifier

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


for i in range(1,len(train)+1):
    for images, labels in train.take(i):
        imgg = images.numpy()
        labs = labels.numpy()
    if i == 1:
        train_images = imgg
        train_labels = labs
    if i == 2:
        train_images_fin = np.append(train_images,imgg)
        train_labels_fin = np.append(train_labels,labs)
    if i > 2 :
        train_labels_fin = np.append(train_labels_fin,labs)   
        train_images_fin = np.append(train_images_fin,imgg)

train_images_fin = train_images_fin.reshape(640,201,256,3)

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

test_images_fin = test_images_fin.reshape(160,201,256,3)





xtrain = np.reshape(train_images_fin, (640,154368))
xtest = np.reshape(test_images_fin, (160, 154368))

df_train = pd.DataFrame(data=xtrain)
df_test = pd.DataFrame(data=xtest)

tentative_train = pd.DataFrame(train_labels_fin)

value = range(640)
print(value)

Label = []

for i in value:
        if tentative_train.iloc[i,0]==1:
            Label=Label+[0]
        else:
            Label=Label+[1]



df_train['Label']=Label

val = range(154368)
cols = [0]
for i in val:
    cols = cols + [i]


df_train.head()

tan = df_train[cols]

X_train = tan
# create df features
y_train = df_train['Label']
# create df var to predict

# split df in train and test df

rfc = RandomForestClassifier()
n_estimators=200
# instatiate model
rfc.fit(X_train,y_train)

# train/fit the model

tentative_test = pd.DataFrame(test_labels_fin)

value = range(160)
print(value)

Label = []

for i in value:
        if tentative_test.iloc[i,0]==1:
            Label=Label+[0]
        else:
            Label=Label+[1]

df_test['Label']=Label

val = range(154368)
cols = [0]
for i in val:
    cols = cols + [i]

ten = df_test[cols]

X_test = ten
y_test = df_test['Label']

rfc_pred = rfc.predict(X_test)

# EVAUATE MODEL

print(confusion_matrix(y_test,rfc_pred))
print(classification_report(y_test,rfc_pred))
