library(randomForest)
library(caret) 

OVR_train <- function(classes, data, labels){
  # Save all three binary one-vs-rest models to a list: C/~C, H/~H, E/~E
  ovr_model <- list()
  
  # One-vs-Rest Approach
  for (class in classes) {
    print(class)
    target <- ifelse(labels == class, 1, 0)
    target <- as.factor(target)
    
    ovr_model[[class]] <- randomForest(data, target, type = "classification")
  }
  
  return(ovr_model)
}

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
  }
  
  overall_accuracy <- sum(predicted_labels == true_labels) / length(true_labels)
  
  print("Overall accuracy:")
  print(overall_accuracy)
  
  
  return(list(accuracy = accuracy, overall_accuracy = overall_accuracy, ovr_predictions = predicted_labels))
}

OVO_train <- function(class1, class2, data, labels) {
  # Filter the input and labels for the two classes
  class_input <- data[labels %in% c(class1, class2), ]
  class_labels <- labels[labels %in% c(class1, class2)]

  # Create a binary target variable for the two classes
  target <- ifelse(class_labels == class1, 1, 0)
  target <- as.factor(target)

  # Train the random forest model for the two classes
  model <- randomForest(class_input, target, type = "classification")

  return(model)
}

predictions <- function(class1, class2, class3, test_labels, ovr_predictions, ovo_predictions){
  predicted_labels <- (length(test_labels))
  for (i in 1:length(test_labels)) {
    if (ovr_predictions[i] < 0.5){
      if (ovo_predictions[i] < 0.5){
        predicted_labels[i] <- class3
      }
      else{
        predicted_labels[i] <- class2
      }
      
    }
    else {
      predicted_labels[i] <- class1
    }
  }
  return (predicted_labels)
}

# check if there are NA values 
# if so delete them
new_matrix <- amino_matrix_cala[!rowSums(is.na(amino_matrix_cala)),]
# new_test_matrix <- amino_test_cala[!rowSums(is.na(amino_matrix_cala)),]
# define input train and test data
train <- new_matrix[, 1:ncol(new_matrix)-1][1:10000,]
test <- new_matrix[, 1:ncol(new_matrix)-1][10001:15001,]

# define labels 
train_labels <- new_matrix[, ncol(new_matrix)][1:10000]
test_labels <- new_matrix[, ncol(new_matrix)][10001:15001]

# list unique classes 
# 'C' = 1, 'H' = 2, 'E' = 3
classes <- unique(train_labels)

ovr_model = OVR_train(classes, train, train_labels)
ovr_predictions = OVR_predict(classes, ovr_model, test)
ovr_Q3 = OVR_accuracy(ovr_predictions, test_labels, classes)

# C/~C H/E
ovo_model_CnotCHE <- OVO_train(2, 3, train, train_labels)
# ovr_predictions <- predict(ovr_model[[1]], test, type = "prob")[,2] # [,2] This corresponds to the predicted probabilities of the positive class (class 1) in the one-vs-rest approach.
ovo_predictions <- predict(ovo_model_CnotCHE, test, type = "prob")[,2]
predictions_CnotCHE = predictions(1, 2, 3, test_labels, ovr_predictions[,1], ovo_predictions)

#  H/~H C/E
ovo_model_HnotHCE <- OVO_train(1, 3, train, train_labels)
# ovr_predictions <- predict(ovr_model[[2]], test, type = "prob")[,2]
ovo_predictions <- predict(ovo_model_HnotHCE, test, type = "prob")[,2]
predictions_HnotHCE = predictions(2, 1, 3, test_labels, ovr_predictions[,2], ovo_predictions)

#  E/~E C/H
ovo_model_EnotECH <- OVO_train(1, 2, train, train_labels)
# ovr_predictions <- predict(ovr_model[[3]], test, type = "prob")[,2]
ovo_predictions <- predict(ovo_model_EnotECH, test, type = "prob")[,2]
predictions_EnotECH = predictions(3, 1, 2, test_labels, ovr_predictions[,3], ovo_predictions)





  
 