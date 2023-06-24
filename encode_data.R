library(randomForest)

read_data <- function(filepath){
  #'read amino data into dataframe
  file_contents <- readLines(filepath)
  
  # Split each line into three columns
  data <- data.frame(
    ID = character(),
    Sequence = character(),
    Label = character(),
    stringsAsFactors = FALSE
  )
  
  # read txt
  for (i in seq(1, length(file_contents), by = 4)) {
    line1 <- file_contents[i]
    line2 <- file_contents[i+1]
    line3 <- file_contents[i+2]
    line4 <- file_contents[i+3]
    
    entry <- data.frame(
      ID = line1,
      Sequence = line2,
      Label = line3,
      stringsAsFactors = FALSE
    )
    
    data <- rbind(data, entry)
  }
  return(data)
}

wyznaczanieOkna <- function(window, amino_acids){
  window_size <- 5
  sign_indices <- 1:21
  end_sign <- 0
  encoded_window <- matrix(0, nrow = window_size, ncol = length(amino_acids)+1)
  for (j in 1:window_size ) {
    sign <- substr(window, j, j)
    # Get the index of the amino acid in the dictionary
    sign_index <- sign_indices[amino_acids == sign]
    # Set the corresponding position in the encoded feature matrix to 1
    encoded_window[j, sign_index] <- 1
    #add end sign at the ond of each encoded window 
    encoded_window[j, 21] <- end_sign
  }
    
  return(encoded_window)
}

wyznaczanieMacierzy <- function(protein_sequence, window_size, amino_acids, encode_labels){
  my_matrix <- matrix(0, nrow = (nchar(protein_sequence)-window_size +1), ncol = (window_size*length(amino_acids))+6)
  for (i in 1:(nchar(protein_sequence)-window_size +1)){
    window <- substr(protein_sequence, i, i + window_size - 1)
    encoded_features <- c(t(wyznaczanieOkna(window, amino_acids)), encode_labels[i])
    my_matrix[i,] <- unlist(encoded_features)
  }
  # return(encoded_features)
  return(my_matrix)
}

encode_labels <- function(label, mapping) {
  encoded_lab <- sapply(strsplit(label, ''), function(x) mapping[x])
  return(as.list(encoded_lab))
}
# Create a dictionary of amino acids and their corresponding indices
amino_acids <- c("A", "C", "D", "E", "F", "G", "H", "I", "K", "L", "M", "N", "P", "Q", "R", "S", "T", "V", "W", "Y")
filepath <- "D:\\studia\\INFA\\CVAPR\\datasets\\train.txt"
data <- read_data(filepath)

protein_sequences <- data[, 2]
labels <- data[, 3]


# Define the window size
window_size <- 5

# Define the encoding mapping
mapping <- c('C' = 1, 'H' = 2, 'E' = 3)

# Initialize an empty matrix to store the encoded features
#encoded_features <- matrix(0, nrow = window_size, ncol = length(amino_acids))

# merge amino_matrix to encode all data
amino_matrix_cala <- matrix(0, nrow = (nchar(data[, 2][1])-window_size +1), ncol = (window_size*length(amino_acids))+6)


for (t in 1:length(protein_sequences)){
  
  protein_sequence <- protein_sequences[t]
  label <- labels[t]
  
  # Encode the sequence
  encode_label <- encode_labels(label, mapping)
  
  # Initialize an empty matrix to store the encoded amino and label 
  amino_matrix <- matrix(0, nrow = (nchar(protein_sequence)-window_size +1), ncol = (window_size*length(amino_acids))+6)
  
  if (t == 1){
    amino_matrix_cala <- wyznaczanieMacierzy(protein_sequence, window_size, amino_acids, encode_label)
  }
  if (t != 1){
    amino_matrix <- wyznaczanieMacierzy(protein_sequence, window_size, amino_acids, encode_label)
    amino_matrix_cala <- rbind(amino_matrix_cala, amino_matrix)
  }
}
  