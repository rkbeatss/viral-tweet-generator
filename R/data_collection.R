# make sure to install these packages 
install.packages(c("rtweet", "dplyr","tm", "stringi", "wordcloud", "ggplot2"))

# opening libraries
library(rtweet)
library(dplyr)
library(tm)
library(stringi)
library(wordcloud)
library(ggplot2)

options(scipen = 999)

# twitter authentication
token <- get_token()

# get some popular tweets 
top_tweets <- search_tweets("(-filter:verified) (min_faves:10000 OR min_retweets:10000)", lang = "en", n = 10000, token = token)
top_tweets <- top_tweets %>% select(user_id, created_at, screen_name, text, followers_count, favorite_count, retweet_count)
glimpse(top_tweets)

# get some insight on the composition of these popular tweets to help answer some of our preliminary questions

# create a word - screen_name - cloud of the frequent viral tweeters 
top_tweeters <- as.data.frame(table(top_tweets$screen_name))
top_tweeters <- top_tweeters %>% setNames(c("screen_name", "frequency"))
sorted_tweeters <- top_tweeters[order(top_tweeters$frequency, decreasing = TRUE),]  
tweeter_wordcloud <- wordcloud(words = sorted_tweeters$screen_name, freq = sorted_tweeters$frequency, min.freq = 2, scale=c(1,.2), max.words = 100)

# visualize the follower counts of the top 10 most active users (within this list)
top_info <- merge(x = sorted_tweeters[1:10,], y = top_tweets, by = "screen_name")
top_info <- top_info %>% select(created_at, screen_name, followers_count, favorite_count)
top_followers <- distinct(top_info[,c(2,3)])
followers_barplot <- barplot(top_followers$followers_count, main = "Followers Count", xlab = "Number of Followers", ylab = "Top Users", names.arg = top_followers$screen_name, cex.names = 0.4, horiz = TRUE)

# visualize the relationship between tweet times and favourite count (ie, see if tweeting at a certain time  = higher # of faves)
top_info$created_at <- as.POSIXct(top_info$created_at)
favorites_scatterplot <- ggplot(top_info, aes(x = created_at, y = favorite_count, group = 1)) + geom_point(size = 1) + scale_x_datetime(date_breaks = "5 hours", date_labels = "%H:%M") + ggtitle("Relationship between Time and Number of Favorites") + labs(x = "Time", y = "Favorites") +  theme(plot.title = element_text(hjust = 0.5), axis.text.x = element_text(size = 5, angle=90,hjust=1)) 
plot(favorites_scatterplot)

#### ----------------------------------------- PREPROCESSING ----------------------------------------------- ####

# for now, we're only going to deal with the text column
tweet_corpus <- Corpus(VectorSource(top_tweets$text))

#open input files 
abbreviations_file <- "dictionaries/abbreviation_expansion.csv"
emojis_file <- "dictionaries/emoji_dictionary.csv"

# perform contraction expansion: wasn't --> was not
expand_contraction <- function(tweet){
  contraction_patterns = c("(won’t|won't|wont)","(can't|can’t|cant)","(ain't|ain’t|aint)","(y'all|y’all|yall)","(n't|n’t)","('re|’re)","('s|’s)","('d|’d)","('ll|’ll)","('t|’t)","('ve|’ve)","('m|’m)")
  contraction_replacements = c(" will not "," can not ", " are not "," you all "," not "," are "," is "," would "," will ", " not "," have "," am ")
  stri_replace_all_regex(tweet, contraction_patterns, contraction_replacements, vectorize_all = FALSE)
}

# perform abbreviation expansion: omw --> on my way 
expand_abbreviation <- function(tweet){
  # open lookup table
  abbreviations_list <- read.csv(abbreviations_file, header = T)
  abbreviations <- as.character(abbreviations_list$abbreviation)
  expansions <- as.character(abbreviations_list$expansion)
  # applying padding relative to text length in order to capture an exact match (as much as possible)
  abbr_patterns <- stri_pad(abbreviations, nchar(abbreviations) + 2, "both") 
  abbr_replacements <- stri_pad(expansions, nchar(expansions) + 2, "both")
  stri_replace_all_fixed(tweet, abbr_patterns, abbr_replacements, vectorize_all = FALSE)
}
# perform emoji expansion: ❤️ --> REDHEART
expand_emoji <- function(tweet){
  tweet <- iconv(tweet, "latin1", "ASCII", "byte")
  # open emoji dict 
  emoji_dict <- read.csv(emojis_file, header = T)
  stri_replace_all_fixed(tweet, emoji_dict$R_Encoding, emoji_dict$Name, vectorize_all = FALSE)
}
# get rid of urls and ASCII codes
clean_tweet <- function(tweet){
  regex_list <-c('<[[:alnum:]]+>', '(https)[^([:blank:]|\\"|<|&|#\n\r)]+')
  pattern <- paste(unlist(regex_list), collapse = "|")
  gsub(pattern, " ", tweet)
}
transform_corpus <- function(corpus){
  # convert to lowercase 
  corpus <- tm_map(corpus, content_transformer(tolower))
  # expand contractions 
  corpus <- tm_map(corpus, content_transformer(expand_contraction))
  # expand abbreviations
  corpus <- tm_map(corpus, content_transformer(expand_abbreviation))
  # remove any remaining punctuation 
  corpus <- tm_map(corpus, removePunctuation)
  # expand emojis
  corpus <- tm_map(corpus, content_transformer(expand_emoji))
  # clean text 
  corpus <- tm_map(corpus, content_transformer(clean_tweet))
  # get rid of whitespace
  corpus <- tm_map(corpus, stripWhitespace)
  return(corpus)
}

# preprocess our corpus
processed_corpus <- transform_corpus(tweet_corpus)
cleaned_tweets <- data.frame(text = sapply(processed_corpus, as.character), stringsAsFactors = FALSE)
glimpse(cleaned_tweets)

# save our cleaned text to file
write.table(cleaned_tweets, file="data/cleaned_tweets.txt", col.names = FALSE,  sep = " ")
