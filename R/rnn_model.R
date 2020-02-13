# install packages 
install.packages(c("tokenizers", "dplyr", "tensorflow", "keras"))

# opening libraries 
library(tokenizers)
library(dplyr)
library(stringr)
library(keras)

# open our preprocessed text
text_file <- "data/cleaned_tweets.txt"
read_text <- read.table(text_file, sep = " ", stringsAsFactors = FALSE)[2] %>% setNames(c("text"))
input_text <- read_text %>% pull(text)

# we have some really short tweets so we want to filter those out & ensure that tweets are atleast 40 chars
count_chars <- nchar(input_text, type = "chars")
filtered_text <- input_text[count_chars >= 40]

# tokenize each character for every tweet
tokenized_text <- filtered_text %>% tokenize_characters(strip_non_alphanum = FALSE, lowercase = FALSE)

# let's see what the first ten tokenized tweets look like
head(tokenized_text, 10) %>% glimpse()

# count the number of total characters in dataset (excluding space chars in this case)
count_characters <- function(list){
  total_count <- sum(lengths(list))
  return(total_count)
}
total_characters <- count_characters(tokenized_text)
print(sprintf("There are %d characters in the corpus", total_characters))

# count the number of total unique characters in dataset (we need this to create a unique mapping)
#TREAT CAPITAL CHARACTERS AS UNIQUE TOKENS
unique_characters <- tokenized_text %>% unlist() %>% unique()
len_unique_chars <- unique_characters %>% length()
print(sprintf("There are %d unique characters in the corpus", len_unique_chars)) # makes sense since we preprocessed everything except alphanumeric chars

#### ---------------------- DATA PREP FOR MODEL  ---------------------- ####

# char to num mapping & num to char mapping 
char_indices <- 1:length(unique_characters)
names(char_indices) <- unique_characters

# we can start one hot encoding each tweet at a time as sequences to be used for training
# initialize some of our parameters 
max_len <- 30
step_size <- 1
sentences <- list()
next_chars <- list()
tweet_indices <- seq(1, length(filtered_text))
for(i in tweet_indices){
  text_indices <- seq(1, nchar(filtered_text[i]) - max_len, by = step_size)
  sentences[[i]] <- substr(filtered_text[i], text_indices, text_indices + max_len - 1)
  next_chars[[i]] <- substr(filtered_text[i], text_indices + max_len, text_indices + max_len)
}
print(paste(head(sentences,6), head(next_chars,6), sep = " ==> "))

# start one hot encoding 
x <- array(0L, dim = c(length(sentences), max_len, length(unique_characters)))
y <- array(0L, dim = c(length(sentences), length(unique_characters)))
sentence_indices <- seq(1, length(sentences))
for(s in sentence_indices){
  tokenized_sentence <- tokenize_characters(sentences[s])[[1]]
  for (t in 1:length(tokenized_sentence)) {
    char <- tokenized_sentence[[t]]
    x[s, t, char_indices[[char]]] <- 1
  }
  next_char <- tolower(next_chars[[s]])
  y[s, char_indices[[next_char]]] <- 1
}
#### ---------------------- CREATE MODEL  ---------------------- ####

model <- keras_model_sequential() %>% 
  layer_lstm(units = 192, input_shape = c(max_len, length(unique_characters))) %>% 
  layer_dropout(rate = 0.5) %>%
  layer_dense(units = length(unique_characters), activation = "softmax")

#### ---------------------- TRAIN MODEL  ---------------------- ####

model %>% compile(
  loss = "categorical_crossentropy", 
  optimizer = optimizer_rmsprop(lr = 0.01)
)  
training_history <- model %>% fit(x, y, epochs = 80, batch_size = 128)
plot(training_history) 

#### ---------------------- GENERATE TEXT ---------------------- ####

generate_tweets <- function(model, text, unique_chars, max_len){
  variation <- 0.3
  start_index <- sample(1:(nchar(text) - max_len), size = 1)
  sentence <- substr(text, start_index, start_index + max_len - 1)
  generated_text <- sentence
  cat(sentence)
  for (i in 1:70){
      sentence_data <- array(0, dim = c(1, max_len, length(unique_characters)))
      tokenized_generated_sentence <- tokenize_characters(generated_text)[[1]]
      for (t in 1:length(tokenized_generated_sentence)) {
        char <- tokenized_generated_sentence[[t]]
        sentence_data[1, t, char_indices[[char]]] <- 1
      }
    prediction <- predict(model, sentence_data, verbose = 0)[1,]
    next_index <- which.max(prediction)
    next_char <- unique_characters[[next_index]]
    cat(next_char)
    generated <- paste0(generated_text, next_char)
    generated_text <- substring(generated, 2)
  }
  cat('\n\n')
}
sample_next_char <- function(preds, temperature = 1.0) {
  preds <- as.numeric(preds)
  preds <- log(preds) / temperature
  exp_preds <- exp(preds)
  preds <- exp_preds / sum(exp_preds)
  which.max(t(rmultinom(1, 1, preds)))
}
for(i in 1:length(filtered_text)){
  generated_tweet <- generate_tweets(model, filtered_text[i], unique_characters, max_len)
}

