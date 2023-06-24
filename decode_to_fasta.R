decode_sequence <- function(encoded_sequence) {
  mapping <- c('C' = 1, 'H' = 2, 'E' = 3)
  decoded_sequence <- sapply(encoded_sequence, function(x) names(mapping)[x])
  return(decoded_sequence)
}

# The function to save a column from the dataframe to a FASTA format
# @input - data - the dataframe to save 
# @input - column - column id
# @input - id - the sequence id 
# @input - filepath - filepath to the result file
save_as_fasta<-function(column, id, filepath){
  writeLines(paste0(">", id), filepath);
  aminos<-as.character(column);
  aminos_str<-paste(aminos, collapse="");
  write(aminos_str, filepath, append = TRUE);
}


decode_label = decode_sequence(ovr_Q3[["ovo_predictions"]])

fasta = save_as_fasta(decode_label, 'oryginal', "D:\\studia\\INFA\\CVAPR\\ovo.fasta")