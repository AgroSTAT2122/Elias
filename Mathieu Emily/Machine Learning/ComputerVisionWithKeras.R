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
test.2D <- test_images[,,]
train.2D.Y <- train_labels[1:n.ind]
train.2D.Y.cat <- to_categorical(train.2D.Y)
#transforme les cat de 1 à 9 quanti en factorielle (tableau disjonctif)
test.2D.Y <- test_labels

## MLP initialisation
model1 <- keras_model_sequential()

## Architecture definition
model1 %>%
  layer_flatten(input_shape = c(28, 28)) %>%
  layer_dense(units = 1) %>%
  layer_activation(activation = 'relu') %>%
  layer_dense(units = 10) %>%
  layer_activation(activation = 'softmax')

## Compiling options
model1 %>% compile(
  optimizer = 'adam', 
  loss = 'categorical_crossentropy',
  metrics = c('accuracy')
)

## Model training
h1 <- model1 %>% fit(#train_images,
  train.2D,
  train.2D.Y.cat, 
  epochs = 50,
  batch_size = 100,
  validation_split = 0.2
)

plot(h1)

class_pred1 <- model1 %>% predict_classes(test_images)
cM.DL1 <- caret::confusionMatrix(data = factor(class_pred1,levels=levels(as.factor(test_labels))), reference = as.factor(test_labels))
cM.DL1$overall[1]

################################
################################
################################

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

h2 <- model2 %>% fit(
  train.2D,
  train.2D.Y.cat, 
  batch_size = 100,
  validation_split = 0.2,
  epochs = 50)
plot(h2)

class_pred2 <- model2 %>% predict_classes(test_images)
cM.DL2 <- caret::confusionMatrix(data = factor(class_pred2,levels=levels(factor(test_labels))), reference = factor(test_labels))
cM.DL2$overall[1]


################################
################################
################################

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
class_pred3 <- model3 %>% predict_classes(test_images)
cM.DL3 <- caret::confusionMatrix(data = factor(class_pred3,levels=levels(factor(test_labels))), reference = factor(test_labels))
cM.DL3$overall[1]

################################
################################
################################

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

h4.0 <- model4.0 %>% fit(
  train.2D,
  train.2D.Y.cat, 
  batch_size = 100,
  validation_split = 0.2,
  epochs = 50)
plot(h4.0)

class_pred4.0 <- model4.0 %>% predict_classes(test_images)
cM.DL4.0 <- caret::confusionMatrix(data = factor(class_pred4.0,levels=levels(factor(test_labels))), reference = factor(test_labels))
cM.DL4.0$overall[1]

################################
################################
################################

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

h4 <- model4 %>% fit(
  train.2D,
  train.2D.Y.cat, 
  batch_size = 100,
  validation_split = 0.2,
  epochs = 50)
plot(h4)

class_pred4 <- model4 %>% predict_classes(test_images)
cM.DL4 <- caret::confusionMatrix(data = factor(class_pred4,levels=levels(factor(test_labels))), reference = factor(test_labels))
cM.DL4$overall[1]

################################
################################
################################

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

h5 <- model5 %>% fit(
  my.train.2D,
  train.2D.Y.cat, 
  batch_size = 100,
  validation_split = 0.2,
  epochs = 50)

plot(h5)

class_pred5 <- model5 %>% predict_classes(my.test.2D)
cM.DL5 <- caret::confusionMatrix(data = factor(class_pred5,levels=levels(factor(test.2D.Y))), reference = factor(test.2D.Y))
cM.DL5$overall[1]

################################
################################
################################

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

h6 <- model6 %>% fit(
  my.train.2D,
  train.2D.Y.cat, 
  batch_size = 100,
  validation_split = 0.2,
  epochs = 50)

plot(h6)

class_pred6 <- model6 %>% predict_classes(my.test.2D)
cM.DL6 <- caret::confusionMatrix(data = factor(class_pred6,levels=levels(factor(test.2D.Y))), reference = factor(test.2D.Y))
cM.DL6$overall[1]
