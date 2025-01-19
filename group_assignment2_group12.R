library(parallel)
library(textir) 
library(Matrix)
library(tidyverse)
library(slam)

library(tm)
library(tidytext)
library(dplyr)
library(stringr)
library(tokenizers)
library(stopwords)
library(proxy)

setwd("")

# Load data
load("Data/CongressionalRecords.RData")
#load("dtm_assignment2.Rdata")
#load("mnir_assignment2.Rdata")
View(crec)

##########################################################
############ DATA CLEANING AND PREPROCESSING #############
##########################################################

# In order to save some computational power when operating with bigrams and DTM 
# we will sample the documents from the dataframe and retain all bigrams used in a speech 
# to preserve the full context of the speeches and get the best results

# What is the distribution of the observations in each party?
crec %>%
  group_by(party) %>%
  summarize(sum_party = n())

# 1 Democrat        37344
# 2 Independent       444
# 3 Republican      31711

# The analysis does not include the Independent party - we will delete the observations with it
# We will consider 10% of the observations from both Republican and Democrat parties respectively

crec_updated <- crec %>%
  filter(party %in% c("Democrat", "Republican")) %>%
  group_by(party) %>%
  sample_frac(0.1) %>%
  ungroup()

# Check the result - we have limited the data from 69499 to 27622 rows, while keeping the distribution of
# both Republican and Democrat parties
crec_updated %>% count(party)

# 1 Democrat   3734
# 2 Republican 3171

# We now perform further data preparation

speeches <- crec_updated %>% pull(strText) # get the vector of speeches
crec_updated <- crec_updated %>% select(-strText) # delete the column with the speeches from the main data frame

# We are utilizing a similar code from our first group assignment
# We have created a loop that cleans every element in a vector and then appends it to the cleaned_speeches vector

cleaned_speeches <- c()

for (i in seq_along(speeches)) {
  # make the text lower and remove: numbers, punctuation, unnessecary whitespaces - on the each element of the vector
  speech_text <- speeches[i] %>%
    tolower() %>%
    removePunctuation() %>%
    removeNumbers() %>%
    str_squish()
  
  # create a tibble out of the article text to unnest the tokens and perform filtering to:
  # 1) remove the stopwords
  # 2) consider only terms with 3-20 letters
  cleaned_tokens_vector <- speech_text %>%
    as_tibble() %>%
    unnest_tokens(input = value, output = tokens) %>%
    filter(!tokens %in% stopwords()) %>%
    filter(nchar(tokens) >= 3 & nchar(tokens) <= 20) %>% # we decided to leave this filter from the first assignment as we think it to be relevant for this problem too
    pull(tokens)
  
  # append the cleaned article text to the vector with cleaned data
  cleaned_speeches <- c(cleaned_speeches, paste(cleaned_tokens_vector, collapse = " "))
  
}

# Create bigrams out of cleaned speeches
text_bigrams <- sapply(cleaned_speeches, function(text){
  tokenize_ngrams(text, n = 2, ngram_delim = "_")})

# Create a Document-Term Matrix - two possible approaches:
# 1) use a sample of bigrams used for every document 
# 2) sample the documents and save all bigrams

# Because we aim to retain a full context used in a given speech and do not want to loose any important 
# bigrams that might be relevant in assessing relationship between the speeches and political party, 
# we proceed with the second approach - documents sampling (the step was completed at the beginning of preprocessing)

bigrams_sum <- 0

for (i in seq_along(text_bigrams)) {
  no_bigrams <- length(text_bigrams[[i]])
  bigrams_sum <- bigrams_sum + no_bigrams
}
bigrams_sum

# After the preprocessing and cleaning we will create a corpus containing 901632 bigrams from 6905 documents
corpus <- Corpus(VectorSource(text_bigrams))
dtm <- DocumentTermMatrix(corpus,
                          control = list(
                            bounds = list(global = c(5,100))))

# Save the DTM for further processing
save(dtm, file = "dtm_assignment2.Rdata")

##################################################
############ ESTIMATE THE MNIR MODEL #############
##################################################

# Create a party_code column with the party names represented as numbers:
# Republican party - 0
# Democrat party - 1

crec_updated$party_code <- NA
crec_updated$party_code[crec_updated$party == "Republican"] <- 0
crec_updated$party_code[crec_updated$party == "Democrat"] <- 1

# Create the training and test data
unique(crec_updated$year)

crec_updated$period <- NA
crec_updated$period[crec_updated$year %in% c(2021, 2022)] <- "training"
crec_updated$period[crec_updated$year %in% c(2023)] <- "test"
table(crec_updated$period)

# test training 
# 1757    5148 

dim(dtm)
# 6905 15440
# We conduct our analysis based on 6905 speeches with 15440 unique bigrams

# Delete terms that are not used in the training sample
dtm_train <- dtm[, col_sums(dtm[crec_updated$period == "training", ]) != 0 ]

# Initialize parallel session and train the model
cl <- makeCluster(5)

# Fit the model
mnir <- dmr(cl, 
            covars = crec_updated[crec_updated$period == "training", "party_code"], 
            counts = dtm_train[crec_updated$period == "training", ], 
            bins=NULL, 
            gamma=10, 
            nlambda=10, 
            verb= 2)

# Close cluster
stopCluster(cl)

# Save the model for further processing
save(mnir, file = "mnir_assignment2.Rdata")

#########################################################################################
############ BIGRAMS STRONGEST CORRELATED TO REPUBICAN AND DEMOCRAT PARTIES #############
#########################################################################################

# Our coded parties:
# Republican party - 0
# Democrat party - 1

mnir.coef <- coef(mnir)
mnir.coef.df <- as.data.frame(as.matrix(mnir.coef))
mnir.coef.df <- t(mnir.coef.df)

mnir.coef.df <- as.data.frame(mnir.coef.df)
mnir.coef.df <- rownames_to_column(mnir.coef.df, var = "bigram")

mnir.coef.df <- mnir.coef.df %>%
  select(bigram, party_code) %>%
  arrange(desc(party_code))

# Since we coded democrat party as "1", the highest values of coefficients for the party_code will 
# signify the strongest relationship of bigrams with the democrat party
head(mnir.coef.df, 10)

# Since we coded republican party as "0", the lowest values of coefficients for the party_code will 
# signify the strongest relationship of bigrams with the republican party
tail(mnir.coef.df, 10)


#######################################################################
############ MAKE PREDICTION FOR POLITICAL PARTY FOR 2023 #############
#######################################################################

# Delete terms that are not used in the test sample from the full dtm
dtm_test <- dtm[, col_sums(dtm[crec_updated$period == "test", ]) != 0 ]

# Match terms for testing and training dtms
common_terms <- intersect(colnames(dtm_train), colnames(dtm))
dtm_test <- dtm[crec_updated$period == "test", common_terms, drop = FALSE]

# Predict using the model - only the terms that were included both in training and test sets can give us predictions
proj <- srproj(mnir, as.matrix(dtm_test))
head(proj)

# Convert projections to a data frame and align indices
proj_df <- data.frame(party_code = crec_updated$party_code[crec_updated$period == "test"], 
                      projection = proj[, 1])

# Merge projections with the original dataframe 
crec_test <- crec_updated %>%
  filter(period == "test") %>%
  mutate(projection = proj_df$projection)

# 10 people with the highest MNIR score/coefficient
crec_test %>%
  select(speaker, projection) %>%
  arrange(desc(projection)) %>%
  head(10)

# 10 people with the lowest MNIR score/coefficient
crec_test %>%
  select(speaker, projection) %>%
  arrange(desc(projection)) %>%
  tail(10)









