library(keras)
library(tidyr)
library(ggplot2)
require(glmnet)
require("e1071") ## Include svm function
require("kernlab") ## Function ksvm
require("nnet") ## Function multinom
require("MASS") ## Function LDA and QDA
require("class") ## Function knn
require("caret") ## Function train and tools for assessing classification performances

fashion_mnist <- dataset_fashion_mnist()

c(train_images, train_labels) %<-% fashion_mnist$train
c(test_images, test_labels) %<-% fashion_mnist$test

train_images <- train_images / 255
test_images <- test_images / 255

class_names = c('T-shirt/top',
                'Trouser',
                'Pullover',
                'Dress',
                'Coat', 
                'Sandal',
                'Shirt',
                'Sneaker',
                'Bag',
                'Ankle boot')

n.ind <- 10000
train.2D <- train_images[1:n.ind,,]
my.train.2D <- array(NA,dim=c(dim(train.2D)[1],dim(train.2D)[2],dim(train.2D)[3],1))
my.train.2D[,,,1] <- train.2D
test.2D <- test_images[,,]
my.test.2D <- array(NA,dim=c(dim(test.2D)[1],dim(test.2D)[2],dim(test.2D)[3],1))
my.test.2D[,,,1] <- test.2D
train.2D.Y <- train_labels[1:n.ind]
train.2D.Y.cat <- to_categorical(train.2D.Y)
#transforme les cat de 1 à 9 quanti en factorielle (tableau disjonctif)
test.2D.Y <- test_labels[1:n.ind]

train.1D <- matrix(NA,ncol=28*28,nrow=dim(train.2D)[1])
#on passe dans un tableau de données classique (avec des variables) : on multiplie les pixels entre eux
train.1D.all <- matrix(NA,ncol=28*28,nrow=dim(train_images)[1])
test.1D <- matrix(NA,ncol=28*28,nrow=dim(test.2D)[1])
for (i in 1:dim(train.2D)[1]){
  train.1D[i,] <- as.numeric(train.2D[i,,])
}
for (i in 1:dim(train_images)[1]){
  train.1D.all[i,] <- as.numeric(train_images[i,,])
}
for (i in 1:dim(test.2D)[1]){
  test.1D[i,] <- as.numeric(test.2D[i,,])
}
train.1D <- as.data.frame(train.1D)
train.1D$Y <- as.factor(train.2D.Y[1:n.ind])
train.1D.all <- as.data.frame(train.1D.all)
train.1D.all$Y <- as.factor(train_labels)
test.1D <- as.data.frame(test.1D)
test.1D$Y <- as.factor(test.2D.Y[1:n.ind])

################
################
## One hidden layer with size 1
################
################

## MLP initialisation
model1 <- keras_model_sequential()

## Architecture definition
model1 %>%
  layer_flatten(input_shape = c(28, 28)) %>% #Les images sont résumés en colonnes (on passe à 784 colonnes, soit 28*28)
  layer_dense(units = 1, ) %>% #Complétement connecté
  layer_activation(activation = 'relu') %>% #fonction d'activation 'relu' (d'autres existent)
  layer_dense(units = 10) %>% #connection à la sortie 
  layer_activation(activation = 'softmax') #forcément du softmax dès qu'on a plus de deux catégories (catégories pas variables)

## Compiling options
model1 %>% compile(
  optimizer = 'adam', 
  loss = 'categorical_crossentropy',
  metrics = c('accuracy')
)

## Model training
#history1 <- model1 %>% fit(as.matrix(train.1D[,1:784]),as.matrix(train.1D$Y), epochs = 100)
h1 <- model1 %>% fit(#train_images,
               #train_labels,
              train.2D,
               train.2D.Y.cat, 
               #my.train.Y,
               epochs = 200, #en gros nombre d'itérations
               batch_size = 300,
               validation_split = 0.2
               )
plot(h1)

## Model evaluation and prediction
score <- model1 %>% evaluate(as.matrix(test.1D[,1:784]), as.matrix(test.1D$Y))
cat('Test loss:', score$loss, "\n")
cat('Test accuracy:', score$acc, "\n")

predictions1 <- model1 %>% predict(test_images) #donne une proba de prédiction pour chaque classe
class_pred1 <- model1 %>% predict_classes(test_images) #predit la classe d'appartenance
cM.DL1 <- caret::confusionMatrix(data = factor(class_pred1,levels=levels(as.factor(test_labels))), reference = as.factor(test_labels)) #Utilisation de la fonction matrice de confusion pour l'accuracy
cM.DL1$overall[1]


################
################
## One hidden layer with size 128
################
################

model2 <- keras_model_sequential()
model2 %>%
  layer_flatten(input_shape = c(28, 28)) %>%
  layer_dense(units = 128) %>%
  layer_activation(activation = 'relu') %>%
  layer_dense(units = 10) %>%
  layer_activation(activation = 'softmax')

model2 %>% compile(
  optimizer = 'adam', 
  loss = 'categorical_crossentropy',
  metrics = c('accuracy')
)

#model2 %>% fit(as.matrix(train.1D[,1:784]),as.matrix(train.1D$Y), epochs = 100)
h2 <- model2 %>% fit(#train_images,
               #train_labels,
               train.2D,
               train.2D.Y.cat, 
               batch_size = 100,
               validation_split = 0.2,
               epochs = 50)
plot(h2)

#predictions <- model2 %>% predict(as.matrix(test.1D[,1:784]))
#class_pred <- model2 %>% predict_classes(as.matrix(test.1D[,1:784]))
#cM.Nnet <- caret::confusionMatrix(data = factor(class_pred,levels=levels(factor(test.1D$Y))), reference = factor(test.1D$Y))
#cM.Nnet$overall[1]
predictions2 <- model2 %>% predict(test_images)
class_pred2 <- model2 %>% predict_classes(test_images)
cM.DL2 <- caret::confusionMatrix(data = factor(class_pred2,levels=levels(factor(test_labels))), reference = factor(test_labels))
cM.DL2$overall[1]


################
################
## One hidden layer with size 256
################
################

model3 <- keras_model_sequential()
model3 %>%
  layer_flatten(input_shape = c(28, 28)) %>%
  layer_dense(units = 1000) %>%
  layer_activation(activation = 'relu') %>%
  layer_dense(units = 10) %>%
  layer_activation(activation = 'softmax')

model3 %>% compile(
  optimizer = 'adam', 
  loss = 'categorical_crossentropy',
  metrics = c('accuracy')
)

h3 <- model3 %>% fit(#train_images,
               #train_labels,
               train.2D,
               train.2D.Y.cat, 
               batch_size = 100,
               validation_split = 0.2,
               epochs = 50)

plot(h3)
#predictions <- model2 %>% predict(as.matrix(test.1D[,1:784]))
#class_pred <- model2 %>% predict_classes(as.matrix(test.1D[,1:784]))
#cM.Nnet <- caret::confusionMatrix(data = factor(class_pred,levels=levels(factor(test.1D$Y))), reference = factor(test.1D$Y))
#cM.Nnet$overall[1]
predictions3 <- model3 %>% predict(test_images)
class_pred3 <- model3 %>% predict_classes(test_images)
cM.DL3 <- caret::confusionMatrix(data = factor(class_pred3,levels=levels(factor(test_labels))), reference = factor(test_labels))
cM.DL3$overall[1]


################
################
## 3 hidden layers
################
################

model4.0 <- keras_model_sequential()
model4.0 %>%
  layer_flatten(input_shape = c(28, 28)) %>%
  layer_dense(units = 256, activation="relu") %>%
  layer_dense(units = 128,activation="relu") %>%
  layer_dense(units = 64,activation="relu") %>%
  layer_dense(units = 10) %>%
  layer_activation(activation = 'softmax')

model4.0 %>% compile(
  optimizer = 'adam', 
  loss = 'categorical_crossentropy',
  metrics = c('accuracy')
)

h4.0 <- model4.0 %>% fit(#train_images,
  #train_labels,
  train.2D,
  train.2D.Y.cat, 
  batch_size = 100,
  validation_split = 0.2,
  epochs = 50)
plot(h4.0)
predictions4.0 <- model4.0 %>% predict(test_images)
class_pred4.0 <- model4.0 %>% predict_classes(test_images)
cM.DL4.0 <- caret::confusionMatrix(data = factor(class_pred4.0,levels=levels(factor(test_labels))), reference = factor(test_labels))
cM.DL4.0$overall[1]

### Dropout

model4 <- keras_model_sequential()
model4 %>%
  layer_flatten(input_shape = c(28, 28)) %>%
  layer_dense(units = 256, activation="relu") %>%
  layer_dropout(0.4) %>%
  layer_dense(units = 128,activation="relu") %>%
  layer_dropout(0.4) %>%
  layer_dense(units = 64,activation="relu") %>%
  layer_dropout(0.4) %>%
  layer_dense(units = 10) %>%
  layer_activation(activation = 'softmax')

model4 %>% compile(
  optimizer = 'adam', 
  loss = 'categorical_crossentropy',
  metrics = c('accuracy')
)

h4 <- model4 %>% fit(#train_images,
               #train_labels,
               train.2D,
               train.2D.Y.cat, 
               batch_size = 100,
               validation_split = 0.2,
               epochs = 50)
plot(h4)
predictions4 <- model4 %>% predict(test_images)
class_pred4 <- model4 %>% predict_classes(test_images)
cM.DL4 <- caret::confusionMatrix(data = factor(class_pred4,levels=levels(factor(test_labels))), reference = factor(test_labels))
cM.DL4$overall[1]

################
################
## CNN : 1 Convolution layer
################
################

model5 <- keras_model_sequential()

model5 %>%
  # Start with hidden 2D convolutional layer being fed 28x28 pixel images
  layer_conv_2d(
    filter = 28, kernel_size = c(3,3), padding = "same", 
    input_shape = c(28, 28,1)
  ) %>%
  layer_activation("relu") %>%
  
  # Use max pooling
  layer_max_pooling_2d(pool_size = c(2,2)) %>%
  layer_dropout(0.25) %>%

  layer_flatten() %>%
  layer_dense(5488) %>%
  layer_activation("relu") %>%
  layer_dropout(0.5) %>%
  
  # Outputs from dense layer are projected onto 10 unit output layer
  layer_dense(10) %>%
  layer_activation("softmax")

model5 %>% compile(
  optimizer = 'adam', 
  loss = 'categorical_crossentropy',
  metrics = c('accuracy')
)

h5 <- model5 %>% fit(#train_images,
  #train_labels,
  my.train.2D,
  train.2D.Y.cat, 
  batch_size = 100,
  validation_split = 0.2,
  epochs = 50)

plot(h5)

predictions5 <- model5 %>% predict(my.test.2D)
class_pred5 <- model5 %>% predict_classes(my.test.2D)
cM.DL5 <- caret::confusionMatrix(data = factor(class_pred5,levels=levels(factor(test.2D.Y))), reference = factor(test.2D.Y))
cM.DL5$overall[1]

#table(class_pred,train.2D.Y)

################
################
## CNN : 3 Convolution layers
################
################

model6 <- keras_model_sequential()

model6 %>%
  # Start with hidden 2D convolutional layer being fed 28x28 pixel images
  layer_conv_2d(
    filter = 32, kernel_size = c(3,3), padding = "same", 
    input_shape = c(28, 28,1)
  ) %>%
  layer_activation("relu") %>%
  
  layer_conv_2d(
    filter = 64, kernel_size = c(3,3), padding = "same") %>%
  layer_activation("relu") %>%
  
  # Use max pooling
  layer_max_pooling_2d(pool_size = c(3,3)) %>%
  layer_dropout(0.35) %>%
  
  layer_conv_2d(
    filter = 64, kernel_size = c(3,3), padding = "same") %>%
  layer_activation("relu") %>%
  
  layer_max_pooling_2d(pool_size = c(2,2)) %>%
  layer_dropout(0.3) %>%
  
  
  layer_flatten() %>%
  layer_dense(256) %>%
  layer_activation("relu") %>%
  layer_dropout(0.4) %>%
  
  # Outputs from dense layer are projected onto 10 unit output layer
  layer_dense(10) %>%
  layer_activation("softmax")

model6 %>% compile(
  optimizer = 'adam', 
  loss = 'categorical_crossentropy',
  metrics = c('accuracy')
)

h6 <- model6 %>% fit(#train_images,
  #train_labels,
  my.train.2D,
  train.2D.Y.cat, 
  batch_size = 100,
  validation_split = 0.2,
  epochs = 50)

plot(h6)

predictions6 <- model6 %>% predict(my.test.2D)
class_pred6 <- model6 %>% predict_classes(my.test.2D)
cM.DL6 <- caret::confusionMatrix(data = factor(class_pred6,levels=levels(factor(test.2D.Y))), reference = factor(test.2D.Y))
cM.DL6$overall[1]
