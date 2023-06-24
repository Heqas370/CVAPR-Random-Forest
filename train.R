library(randomForest)
library(caret) 

# train one vs rest classifier 
# classes - vector of unique labels
# data - train data
# labels - train labels corresponding to train data 
OVR_train <- function(classes, data, labels){
  
  # save to a list all three binary one vs rest models 
  # C/~C  H/~H  E/~E 
  ovr_model <- list()
  
  # One-vs-Rest Approach
  for (class in classes) {
    print(class)
    target <- ifelse(train_labels == class, 1, 0)
    target <- as.factor(target)
    
    ovr_model[[class]] <- randomForest(train, target, type = "classification")
  }
  return(ovr_model)
}

# classes - vector of unique labels
# ovr_model - trained one vs rest model 
# test - test data for predictions
OVR_predict <- function(classes, ovr_model, test){
  
  ovr_predictions <- matrix(0, nrow = nrow(test), ncol = length(classes))
  colnames(ovr_predictions) <- classes
  
  for (i in 1:length(classes)) {
    class <- classes[i]
    model <- ovr_model[[class]]
    
    # Predict the probabilities for the positive class
    ovr_predictions[ ,class] <- predict(model, test, type = "prob")[, 2]
  }
  return(ovr_predictions)
}

# ovr_predictions - predictions returned from OVR_predict()
# labels - test labels which are corresponding to test data passes to OVR_predict()
OVR_accuracy <- function(ovr_predictions, labels, classes){
  # Calculate accuracy
  predicted_labels <- apply(ovr_predictions, 1, which.max)
  true_labels <- labels
  
  accuracy <- vector("numeric", length(classes))

  for (i in 1:length(classes)) {
    class <- classes[i]
    
    class_indices <- true_labels == class
    class_predicted_labels <- predicted_labels[class_indices]
    class_true_labels <- true_labels[class_indices]
    
    class_accuracy <- sum(class_predicted_labels == class_true_labels) / length(class_true_labels)
    accuracy[i] <- class_accuracy
   
    # Create confusion matrix for the class
    class_confusion_matrix <- caret::confusionMatrix(factor(class_predicted_labels, levels = c(0, 1)), factor(class_true_labels, levels = c(0, 1)))
    print(paste("Confusion matrix for class", class))
    print(class_confusion_matrix)
  }
  
  overall_accuracy <- sum(predicted_labels == true_labels) / length(true_labels)

  print("Overall accuracy:")
  print(overall_accuracy)

  
  return(list(accuracy = accuracy, overall_accuracy = overall_accuracy, ovr_predictions = predicted_labels))
}

OVO_train <- function(classes, data, labels){
  
  ovo_model <- list()
  
  # One-vs-One Approach
  for (i in 1:(length(classes) - 1)) {
    for (j in (i+1):length(classes)) {
      class1 <- classes[i]
      class2 <- classes[j]
      
      # Filter the input and labels for the two classes
      class_input <- data[labels %in% c(class1, class2), ]
      
      class_labels <- labels[labels %in% c(class1, class2)]
      
      # Create a binary target variable for the two classes
      target <- ifelse(class_labels == class1, 1, 0)
      target <- as.factor(target)
      
      # Train the random forest model for the two classes
      ovo_model[[paste(class1, class2, sep = "/")]] <- randomForest(class_input, target, type = "classification")
    }
  }
  return(ovo_model)
}

OVO_predict <- function(classes, ovo_model, test){
  ovo_predictions <- matrix(0, nrow = nrow(test), ncol = length(classes))
  colnames(ovr_predictions) <- classes
  ovo_accuracy <- list()
  # Make predictions for each class using the one-vs-one models
  for (i in 1:(length(classes) - 1)) {
    for (j in (i+1):length(classes)) {
      class1 <- classes[i]
      class2 <- classes[j]
      
      model <- ovo_model[[paste(class1, class2, sep = "/")]]
      # Predict the probabilities for the positive class
      ovo_predictions[, class1] <- ovo_predictions[, class1] + predict(model, test, type = "prob")[, 2]
      ovo_predictions[, class2] <- ovo_predictions[, class2] + predict(model, test, type = "prob")[, 1]
      
    }
  }
  return(ovo_predictions)
}

OVO_accuracy <- function(ovo_predictions, labels, classes){
  # Calculate accuracy
  predicted_labels_ovo <- apply(ovo_predictions, 1, which.max)
  true_labels <- labels
  
  accuracy <- vector("numeric", length(classes))

  for (i in 1:length(classes)) {
    class <- classes[i]
    
    class_indices <- true_labels == class
    class_predicted_labels_ovo <- predicted_labels_ovo[class_indices]
    class_true_labels <- true_labels[class_indices]
    
    class_accuracy <- sum(class_predicted_labels_ovo == class_true_labels) / length(class_true_labels)
    accuracy[i] <- class_accuracy
  }
  
  overall_accuracy <- sum(predicted_labels_ovo == true_labels) / length(true_labels)

  print("Overall accuracy:")
  print(overall_accuracy)

  
  return(list(accuracy = accuracy, overall_accuracy = overall_accuracy, ovo_predictions = predicted_labels_ovo))
}


# check if there are NA values 
# if so delete them
new_matrix <- amino_matrix_cala[!rowSums(is.na(amino_matrix_cala)),]
new_test_matrix <- amino_test_cala[!rowSums(is.na(amino_matrix_cala)),]
# define input train and test data
train <- new_matrix[, 1:ncol(new_matrix)-1]
test <- new_test_matrix[, 1:ncol(new_matrix)-1]

# define labels 
train_labels <- new_matrix[, ncol(new_matrix)]
test_labels <- new_test_matrix[, ncol(new_matrix)]

# list unique classes 
# 'C' = 1, 'H' = 2, 'E' = 3
classes <- unique(train_labels)

# ---------------------------------
# ONE VS REST

ovr_model = OVR_train(classes, train, train_labels)

ovr_predictions = OVR_predict(classes, ovr_model, test)

ovr_Q3 = OVR_accuracy(ovr_predictions, test_labels, classes)

# ---------------------------------
# ONE VS ONE

ovo_model = OVO_train(classes, train, train_labels)

ovo_predictions = OVO_predict(classes, ovo_model, test)

ovo_Q3 = OVO_accuracy(ovo_predictions, test_labels, classes)