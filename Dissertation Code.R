

##########################################################################################
#               LOGISTIC REGRESSION 

# Logistic regression model IV = age, DV = value
logistic_regression <- glm(value ~ age,
                           data = context_leverage_scale,
                           family = binomial)
summary(logistic_regression)
# age Estimate = 0.274572





##########################################################################################
#               LOGISTIC REGRESSION GRAPH 

# check range ages on x-axis
range(longitudinal_aoa$age)
# 16 30

# create vector x-axis
data <- data.frame(age = seq(from = 16, to = 30, by = 1))
data <- data.frame(age = seq(from = 0, to = 50, by = 1))

# create data frame for the graph retriving values from logistic model
data$prob <- predict(logistic_regression, newdata = data, type = "response")

# graph
ggplot(data, aes(x = age, y = prob)) +
  geom_line() +
  ylim(0, 1) +
  geom_vline(xintercept = 16, linetype = "dashed") +
  geom_vline(xintercept = 30, linetype = "dashed")
# Across all words and all children, 
# what is the probability that a word is known at a given age?


# check range ages on x-axis
range(longitudinal_aoa$age)
# 16 30

# create vector x-axis
data <- data.frame(age = seq(from = 16, to = 30, by = 1))
data <- data.frame(age = seq(from = 0, to = 50, by = 1))

# create data frame for the graph retriving values from logistic model
data$prob <- predict(logistic_regression, newdata = data, type = "response")

# graph
ggplot(data, aes(x = age, y = prob)) +
  geom_line() +
  ylim(0, 1) +
  geom_vline(xintercept = 16, linetype = "dashed") +
  geom_vline(xintercept = 30, linetype = "dashed")
# Across all words and all children, 
# what is the probability that a word is known at a given age?









##########################################################################################
#               DIFFERENT MODELS

# Simple Logistic regression model --> not account for variability of CHILD or ITEM
log_reg <- glm(value ~ age,
               data = longitudinal_aoa,
               family = binomial)
summary(log_reg)


###                      RANDOM INTERCEPTS MODEL, "AGE" AS LINEAR PREDICTOR VAR  -->  VARIABILITY OF WORDS & CHILD
mixed_log_reg <- glmer(value ~ age + (1 | word) + (1 | child_id),
                       data = longitudinal_aoa,
                       family = binomial)
summary(mixed_log_reg)



###                      LOG MODEL  "AGE" AS NON LINEAR PREDICTOR VAR
log_reg_nonlinear <- glm(value ~ ns(age, df = 3),
                         data = longitudinal_aoa,
                         family = binomial)
summary(log_reg_nonlinear)



####         RANDOM INTERCEPTS MODEL, "AGE" AS NON LINEAR PREDICTOR VAR + VARIABILITY OF WORDS & CHILD
mixed_log_reg_nonlinear <- glmer(value ~ ns(age, df = 3) + (1 | word) + (1 | child_id),
                                 data = longitudinal_aoa,
                                 family = binomial)
summary(mixed_log_reg_nonlinear)





##########################################################################################
#               COMPARE DIFFERENT MODELS
# Compare two mixed-effects logistic regression models using ANOVA
# This tests whether modelling age as a NON-LINEAR predictor improves model fit.
anova(mixed_log_reg, mixed_log_reg_nonlinear)

anova(log_reg_nonlinear, mixed_log_reg_nonlinear)

anova(mixed_log_reg, mixed_log_reg_nonlinear)









########### plot log_reg_nonlinear PLOTTING REGRESSION MODEL WITH ALL THE VARS BUT WHY HERE?

# extract coefficients
coefs <- tidy(log_reg_nonlinear, effects = "fixed")
ggplot(coefs, aes(x = estimate, y = term)) +
  geom_point() +
  geom_errorbarh(aes(xmin = estimate - std.error,
                     xmax = estimate + std.error), height = 0.2) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  labs(
    x = "Effect size (log-odds)",
    y = "Predictor",
    title = "Effects of all predictors"
  ) +
  theme_minimal()






##########################################################################################


##########################################################################################


##########################################################################################



##########################################################################################




#####################################################
################### LOAD PACKAGES ###################
#####################################################
library(plyr)
library(tidyverse)
library(widyr)
library(tidytext)
library(purrr)

library(readxl)
library(stringr)

library(ggplot2)
library(ggpubr)
library(ggrepel)
library(cowplot)
library(paletteer)
library(ggcorrplot)

library(textstem)
library(textdata)
library(text2vec)
library(wordspace)



library(lemmar)


library(qdapDictionaries)

library(ppcor)
library(scam)
library(rstatix)


library(dplyr)
library(readr)
library(ggplot2)
library(splines)
library(lme4)

library(lme4)
library(splines)
library(ggeffects)
library(ggplot2)
library(broom.mixed)
library(patchwork)

#####################################################
################## LOAD MCDI DATA ###################
#####################################################

# Load age of acquisition data (calculated from MCDI data)
mcdi_aoa <- read.csv("data/mcdi_raw.csv", fileEncoding="latin1")

# longitudinal_aoa data 
mcdi_aoa <- mcdi_aoa %>%
  group_by(child_id) %>%
  filter(n_distinct(age) >= 4) %>%
  ungroup()

# Look:
head(mcdi_aoa)



#####################################################
################# LOAD ABCON DATA ###################
#####################################################

# Load concreteness data 
# These data record average "concreteness" ratings for many words
# (i.e., ratings of the degree to which each word refers to a real,
# physical object vs an abstract idea)
abcon <- read.csv("data/abcon_mcdi_english.csv", fileEncoding="latin1")

# Look:
head(abcon)

#####################################################
############## LOAD UTTERANCE DATA ##################
#####################################################

# Load utterance statistics for words, including:
# utt_length: mean length of utterances in which a word occurs
# log_freq_final: log of the frequency with which word occurs in the 
# final position in an utterance
utt_stats <- readRDS(file = "data/utt_stats_english.rds")

# Look:
head(utt_stats)

#####################################################
#################### GET CORPUS #####################
#####################################################

# Load the CHILDES corpus
# This will load a dataframe in which each row is a full CHILDES 
# transcript. Each transcript is associated with a unique id, the 
# id of the corpus, and the age of the target child.

# The corpus has already been preprocessed by: 
# (1) removing punctuation aside from apostrophes in contractions
# (2) removing utterances that did not come from a caregiver
# (3) lemmatizing (converting multiple forms of same word into one form)

childes_text <- readRDS("data/Corpora Text - Lemmatized + Stop - CHILDES.rds")

# Look:
head(childes_text)

#####################################################
################# GET COMMON NAMES ##################
#####################################################

# Load common names (e.g., of children, family members) that may be present
# in corpus
names_to_replace <- readRDS("data/CHILDES Names.rds")

# Look:
head(names_to_replace)

#####################################################
########## LOAD CHILDES & CALC FREQUENCY ############
#####################################################


# Do additional preprocessing steps on the CHILDES corpus to 
# replace contractions with their non-contraction equivalents

# Replace "'ll" with "will"
childes_text <- childes_text %>% 
  dplyr::mutate_if(is.character, str_replace_all, pattern = "'ll", replacement = ' will')

# Get common contractions and their expanded equivalents
# from qdapDictionaries
data(contractions)

contraction_key <- contractions 

# Add some additional contractions and their expanded equivalents 
# not present in qdapDictionaries
extra_contractions <- data.frame(contraction = c("i'm", "i'll", "i've", "i'd", "that'd", "where'd", 
                                                 "where're", "what're", "willn't", "there're",
                                                 "whyn't", "why'd", "haven't", "o'clock",
                                                 "who're", "hadn't", "d'you", "d'ya", "ya'll", "y'all",
                                                 "c'mon", "how're", "c'mere", "why're", "come'ere", "it'd",
                                                 "she'd", "he'd", "what've", "why've", "where've",
                                                 "s'more", "y'know", "y'want", "cann't", "come'ere",
                                                 "com'ere"),
                                 expanded = c("i am", "i will", "i have", "i would", "that would", "where did", 
                                              "where are", "what are", "will not", "there are",
                                              "why not", "why did", "have not", "oclock",
                                              "who are", "had not", "do you", "do you", "you all", "you all",
                                              "come on", "how are", "come here", "why are", "come here", "it would",
                                              "she would", "he would", "what have", "why have", "where have",
                                              "some more", "you know", "you want", "can not", "come here", "come here"))


# Combine the qdapDictionaries and extra contractions into a dataframe
contraction_key <- rbind(contraction_key, extra_contractions) %>%
  dplyr::distinct()

# Now, convert each transcript into an "unnested" long format where 
# each row is a word in the transcript
childes_unnested <- childes_text %>%
  tidytext::unnest_tokens(word, Text) 

# Add the contraction key to the unnested corpus
childes_unnested <- left_join(childes_unnested, contraction_key, by = c("word" = "contraction"))

# Replace contractions with their expanded forms
childes_unnested$word <- ifelse(is.na(childes_unnested$expanded), childes_unnested$word, childes_unnested$expanded)

# Unnest again to separate out the expanded forms of contractions
childes_unnested <- childes_unnested %>%
  tidytext::unnest_tokens(word, word) %>%
  dplyr::select(id, corpus_id, target_child_age, word)


# Next, replace people's names with a common token
childes_unnested$word <- ifelse(childes_unnested$word %in% names_to_replace, "pname", childes_unnested$word)

# Get word frequencies and log word frequencies,
# then arrange in descending order of frequency
childes_freq <- childes_unnested %>%
  # Count the number of time each word occurs and log transform
  dplyr::count(word, name = "freq") %>%
  dplyr::mutate(log_freq = log(freq)) %>%
  dplyr::arrange(desc(freq))

head(childes_freq)

# We will get only the top N most frequent words, and replace
# other words with an "UNK" token. Here, define the value of
# N as the cutoff_value
cutoff_value <- 4096

# For each word in childes_freq, identify whether it is above 
# the cutoff value or below (1 for words above, 0 for words 
# below)
childes_freq$freq_cutoff <- c(rep(1, cutoff_value), rep(0, nrow(childes_freq)-cutoff_value))

# Add frequency information to unnested corpus
childes_unnested <- left_join(childes_unnested, childes_freq)

# Replace lower freq with "UNK"
childes_unnested$word <- ifelse(childes_unnested$freq_cutoff == 1, childes_unnested$word, "UNK")


# Re-nest the words in the transcripts back together 
childes_text <- childes_unnested %>%
  group_by(id, corpus_id, target_child_age) %>%
  dplyr::summarise(Text = paste(word, collapse=" ")) %>%
  dplyr::ungroup()

# Look at our updated CHILDES corpus:
head(childes_text)

#####################################################
########## COMBINE COUNFANDING VARIABLES ############
#####################################################

# Combine all the "control" statistics that will be used 
# to predict word learning in addition to context leverage
# metrics (calculated below)
control_stats <- childes_freq %>%
  dplyr::filter(word %in% unique(mcdi_aoa$word)) %>%
  dplyr::select(c(word, log_freq)) %>%
  left_join(abcon) %>%
  left_join(utt_stats)

head(control_stats)

##############               context leverage calculation     ################################

#####################################################
################# LOAD CATEGORIES ###################
#####################################################

# The goal of this section is to identify whether each possible 
# pair of words belongs to the same or different semantic category.

# We will use a key that lists a series of semantic categories, and
# lists the words in each category.

# First, load key that maps a set of words to semantic categories 
categories_raw <- read.csv("data/categories_english.csv", fileEncoding="latin1")

# Convert names of categories to lower case
categories_raw <- mutate_all(categories_raw, .funs=tolower)

# Separate lists of words in each category into separate columns
categories_raw <- categories_raw %>% 
  separate_longer_delim(word, delim = ", ")

# Remove any redundancies
categories_raw <- categories_raw %>%
  dplyr::distinct()

# Look at our categories_raw dataframe:
head(categories_raw)

# Now, lemmatize the words in the categories so that they
# match the lemmatized words in the preprocessed corpus

# First, get English language lemmas
language_lemma <- "hash_lemma_en"
data(list = language_lemma)########################################################## what is this? #######

lemmas <- get("hash_lemma_en")

# Add a column to the categories_raw dataframe that lists 
# the lemmatized form of each word
categories_raw$lemmatized <- lemmar::lemmatize_words(categories_raw$word, dictionary = lemmas)

# Look at our updated categories_raw dataframe:
head(categories_raw)

# Create a filtered dataframe that keeps only words that appear 
# in the CHILDES corpus
categories_filter <- categories_raw %>%
  dplyr::filter(lemmatized %in% childes_freq$word)

# Now, we want to use the categories_filter dataframe to identify all possible
# pairs of words, and for each pair:
# -- identify whether they belong to the same vs different category
# -- if same category, identify the category

# First, add new column that pastes together a word with its category
categories_filter$word_category <- paste(categories_filter$lemmatized, categories_filter$category, sep = "_")

# Second, generate each pair of pasted word_category values
pairs <- data.frame(t(combn(categories_filter$word_category, 2)))

# Let's look at this new dataframe:
head(pairs)

# Third, separate pasted word_category values to generate a dataframe with
# a column for each word in the pair and the category of each word in the pair
pairs <- setNames(data.frame(str_split_fixed(pairs$X1, "_", 2), str_split_fixed(pairs$X2, "_", 2)),
                  c("word1", "category1", "word2", "category2"))

# Look:
head(pairs)

# Add a column that indicates whether words in a pair belong to the same or
# different categories
pairs$type <- ifelse(pairs$category1 == pairs$category2, "same", "diff")

# Look:
head(pairs)

# Fourth, add a column that indicates the category membership of both words in a pair
# if they belong to the same category, contains the value "diff" if they belong to 
# different categories
pairs$category <- ifelse(pairs$type == "same", pairs$category1, "diff")

# Look:
head(pairs)

# Note that at this point, each pair of words is listed only once:
# e.g., there is one row in which word1 is "dog" and word2 is "cat"
# However, we want to also have rows in which the same pair is listed
# in the inverted order: e.g., word1 is "cat" and word2 is "dog"
# So the next step is add these "inverted" rows

# Create an inverted version of the pairs dataframe
# Reverse the order of word1 and word2 columns
pairs_invert <- pairs %>%
  dplyr::select(word2, category2, word1, category1, type, category)
# Rename word1 to word2 and vice versa
names(pairs_invert) <- names(pairs)

# Add the inverted version to the main pairs dataframe
pairs <- rbind(pairs, pairs_invert)

# Next, keep only pairs of words in which we have MCDI age of
# acquisition data for both words
pairs <- pairs %>%
  dplyr::filter(word1 %in% mcdi_aoa$word & word2 %in% mcdi_aoa$word)

# Remove any duplicate rows
pairs <- dplyr::distinct(pairs)


# Look at the dataframe again:
head(pairs)

# If a word was in multiple categories (e.g., "orange" is both a food and 
# a color), it might end up having both rows in which it is listed as belonging
# to the same category as the other word, and rows in which it is listed as
# belonging to a different category. For example, when word1 is "orange" and word2
# is "apple", it might have both a row in which it is listed as belonging to the
# same category, and a row in which it is listed as belonging to a different
# category.
# To resolve this ambiguity, we next filter down the pairs dataframe so that in
# these cases, we only keep the row in which it is listed as belonging to the same
# category
pairs <- pairs %>%
  group_by(word1, word2) %>%
  # add a helper variable called "double entry" which identifies whether there are
  # two rows for the same word pair in which one of the rows has an entry in the type
  # column of "same" and the other has an entry in the type column as "diff" (i.e., 
  # two unique values in the type column)
  dplyr::mutate(double_entry = ifelse(  length(type) == 2 & length(unique(type)) == 2 , 1, 0 )) %>%
  # in cases where there is a double entry, remove the one in which the 
  # categories are listed as "diff"
  dplyr::filter(!(double_entry == 1 & type == "diff")) %>%
  # remove the helper variable
  dplyr::select(!double_entry)


# The pairs dataframe is now in a format that we will use later 
# in the script to calculate context leverage!
head(pairs)


#####################################################
#####################################################
####### MEASURE CO-OCCURRENCE ACROSS CORPORA ########        
#####################################################

# This section measures how frequenty any given pair of words
# co-occur together either adjacently, or separated by up to 10
# words. 
# Thus, co-occurrences are measured within an 11-word "window"

# Set the window size
window_size = 11

# Divide text into "ngrams" of window_size. This will be accomplished
# by going through the text word-by-word, and at each point, grabbing
# the ngram that includes the current word + the following 10 words.
ngrams_wide <- childes_text %>%
  dplyr::ungroup() %>%
  dplyr::select(id, Text) %>%
  tidytext::unnest_tokens(ngram, Text, token = "ngrams", n = window_size)

# Look:
head(ngrams_wide)

# Now, unnest the words in the ngrams to convert them to a "long"
# format in which each row is a word, and there is a column indicating
# which ngram it came from
ngrams_long <- ngrams_wide %>%
  dplyr::mutate(ngramID = row_number()) %>% # give each ngram a number starting from 1
  tidyr::unite(windowID, ngramID) %>% 
  tidytext::unnest_tokens(word, ngram) # "unnest" each ngram so that each word gets its own row

# Look:
head(ngrams_long)

# Next, count the number of times each word appears with each other word
# in the same ngram. This is our measure of co-occurrence *frequency*
#How often two words appear together in the same context (ngram/window)
# If two words often appear in the same window
# 👉 they are likely semantically related
co_counts <- ngrams_long %>%
  # we use the pairwise_count function, which counts the number of times 
  # each word occurs with each other word in the same ngram
  # we set the argument diag=FALSE to not count words co-occurring with themselves
  widyr::pairwise_count(word, windowID, diag = FALSE, sort = TRUE) 


# Next, we want to go beyond counting mere frequency of co-occurrence:
# instead, we want a measure of how *reliably* words are associated 
# with each other, controlling for the fact more frequent words will tend 
# to co-occur more frequently with other words just by chance.
# To accomplish this, we will take advantage of the *dsm* package, which includes
# functions for calculating metrics of co-occurrence reliability

# To use dsm package functions, we will first convert our co_counts
# to a special co-occurrence matrix that the dsm package uses for
# calculating metrics
direct_matrix <- dsm(target = co_counts$item1, feature = co_counts$item2, score = co_counts$n,
                     raw.freq=TRUE, sort=TRUE)

# Now we can use the dsm.score function on the co-occurrence matrix
# to calculate a meatric of word association. Here, we use PPMI as 
# our metric
direct_matrix_ppmi <- dsm.score(direct_matrix, score = "MI",
                                sparse=TRUE, # Convert negative to 0
                                normalize=FALSE
)

# In the direct_matrix_ppmi object, our PPMI scores are stored in a special
# dsm package format. So, we now need to extract them out of this
# object so that we can have a standard data.frame that lists 
# PPMI between each pair of words.
# The dsm function stored the PPMI scores in part of the direct_matrix_pmi
# object called "S". S is a kind of matrix in which each cell is the PPMI
# score for a pair of words, so the rows indicate the first word in the pair
# and the columns indicate the second word in the pair


# To extract information from S, we first convert S to a dataframe. This
# extracts the PPMI scores themselves
co <- as.data.frame(summary(direct_matrix_ppmi$S)) 

# Next, remember that each PPMI score is a measure of association between
# a pair of words. So, we need to indicate which pair of words each score
# came from. We will extract this information from the row names and column names
# of the S matrix object.
co$word1 <- rownames(direct_matrix_ppmi$S)[co$i]
co$word2 <- colnames(direct_matrix_ppmi$S)[co$j]

# As you can see, we now have a dataframe with PPMI scores for
# word pairs (though we are not finished formatting the columns). 
# Look:
head(co)

# Note that PPMI scores are in a column called "x". This
# isn't a very informative name, so we will give it an
# informative name that indicates that these are PPMI scores
# within an 11-word window.
co <- co %>%
  dplyr::rename(ppmi_11 = x)

# Let's select and organise the columns:
co <- co %>%
  dplyr::select(word1, word2, ppmi_11)

# The S matrix included the diagonal, which indicated the co-occurrence
# of words with themselves. We're not interested in these scores, so remove
# these rows
co <- co %>%
  dplyr::filter(word1 != word2)


# Filter down to only words with mcdi values
co_filter <- co %>%
  dplyr::filter(word1 %in% mcdi_aoa$word & word2 %in% mcdi_aoa$word)

# Convert the ppmi column into a long format so that we end up with one
# column indicating the type of score (ppmi_11), and another column 
# indicating the score itself
co_filter <- pivot_longer(co_filter, cols = "ppmi_11", names_to = "measure", values_to = "co_score")


# Look:
head(co_filter)


###############################################
### GET CONTEXT OVERLAP FROM CO-OCCURRENCE ####
###############################################

# This section defines a function that calculates "context overlap"
# between words. Context overlap is the degree to which any two words
# tend to co-occur with similar sets of other words. 
# The function takes one argument, cutoff, which is the size of the set of 
# co-occurring words. 
cutoff <- 100
length(unique(co_filter$word1))####################### run this first to run the function manually
calc_overlap <- function(cutoff = 100) {
  
  
  # Order the contents of co_filter so that it is first ordered by 
  # word1, and then for each word1, all the words it co-ocurs with 
  # (i.e., all the word2s) are ordered in descending order of co-occurrence.
  co_ordered <- co_filter %>%
    dplyr::arrange(word1, desc(co_score))
  
  # Create a new object called co_cutoff. In this object, for each word1,
  # we get just its top co-occurring set of words, and store that set of words
  # as a list
  co_cutoff <- co_ordered %>%
    group_by(word1) %>%
    dplyr::summarise(word2 = list(word2[1:cutoff]))
  
  # Extract the vector of top co-occurring word lists
  co_cutoff_list <- co_cutoff$word2
  
  # Identify word1 for each element of the vector
  names(co_cutoff_list) = co_cutoff$word1
  
  # Next, looking at the top word list for every possible pair of word1s -
  # e.g., the top word list when word1 is "apple" and the top word list when word1 is "melon" -
  # count the number of words that are in both top word lists.
  # This is the number of "overlaps" between the top word lists for any two word1s
  result = crossprod(table(stack(co_cutoff_list)))
  
  # Store these overlap counts in a dataframe. In this dataframe, 
  # each row is a word pair, and there is a column indicating how many
  # overlaps their are between their top word lists
  co_overlap <- data.frame(word1 = colnames(result)[col(result)],
                           word2 = rownames(result)[row(result)], 
                           overlap = c(result))
  
  # Remove rows in which both words in the pair are the same
  co_overlap <- co_overlap %>%
    dplyr::filter(word1 != word2)
  
  # Use these overlap counts to calculate "jaccard index" for each
  # word pair.
  # The jaccard index is the total number of overlapping words, 
  # divided by the union of words across the two words' lists
  co_overlap$union <- cutoff*2 - co_overlap$overlap
  
  co_overlap$jaccard <- co_overlap$overlap / co_overlap$union
  
  # Get just the jaccard index value, and arrange the dataframe
  # from highest to lowest jaccard index values
  co_overlap <- co_overlap %>%
    dplyr::select(!c(overlap, union)) %>%
    dplyr::arrange(desc(jaccard))
  
  # Finally, return the jaccard index of overlap
  return(co_overlap)
}

# Use the function just defined to calculate context overlap for the 
# default cutoff value of 100
# This returns a dataframe containing context overlap between pairs
# of words
overlap <- calc_overlap()

# Combine context overlap with the pairs dataframe generated above, 
# which indicates whether pairs of words belong to the same vs 
# different categories
overlap <- inner_join(overlap, pairs)

# Look (note that these top rows show word pairs with
# the highest context overlap - you may notice that these
# words happen to also be similar in meaning!):
head(overlap)



################################################
### PREPARING CALCULATION CONTEXT LEVERAGE  ####
################################################

# SUMMARY TABLE FROM LONGITUDINAL DATA   
summary_mcdi_aoa <- mcdi_aoa %>%
  select(word, aoa_fit) %>%
  distinct()

# Now we have calculated context overlap.
# These measures both capture the degree to which pairs of words tend
# to occur in similar contexts.
# In this section, we will use these measures to calculate context leverage:
# i.e., the degree to which a word occurs in similar contexts to words
# similar in meaning that are learned earlier. We will fist calculate
# context leverage from context overlap, then from representational similarity

# Context Leverage from Context Overlap

# Combine dataframe containing context overlap between word pairs
# with AoA (age of acquisiton) for each word in each pair
overlap_aoa <- overlap %>%
  left_join(dplyr::select(summary_mcdi_aoa, word, aoa_fit), by = c("word1" = "word")) %>%
  dplyr::rename(aoa1 = aoa_fit) %>%
  left_join(dplyr::select(mcdi_aoa, word, aoa_fit), by = c("word2" = "word")) %>%
  dplyr::rename(aoa2 = aoa_fit)

# Format the same vs diff category column as a factor
overlap_aoa$type <- factor(overlap_aoa$type, levels = c("same", "diff"))

# Organize the columns
overlap_aoa <- overlap_aoa %>%
  dplyr::select(word1, word2, aoa1, aoa2, category1, category2, type, category, jaccard)


# Look:
head(overlap_aoa)


################################################
######## CALCULATE CONTEXT LEVERAGE  ###########
################################################

# Calculate context leverage as a variable called "within_overlap" 
# This is calculated as a word's overlap with earlier-learned words that are similar 
# in meaning (i.e., same category) minus overlap with earlier-learned words that
# are different in meaning (i.e., diff category)
# In addition, also calculate a control variable called "prop_same" which is
# the proportion of earlier-learned words that are similar in meaning (same category) 
# out of all earlier-learned words
overlap_within <- overlap_aoa %>%
  dplyr::filter(!is.na(aoa1) & !is.na(aoa2)) %>%
  group_by(word1, category1, aoa1) %>%
  dplyr::summarise(within_overlap = mean(jaccard[type == "same" & aoa2 < aoa1], na.rm = T) - mean(jaccard[type == "diff" & aoa2 < aoa1], na.rm = T),
                   prop_same = log(length(type[type == "same" & aoa2 < aoa1]) / length(type[aoa2 < aoa1 ]) + .0001)  ) %>%
  dplyr::rename(word = word1,
                aoa = aoa1,
                category = category1) %>%
  left_join(control_stats)


###############################################
#### DATA FRAME WITH CONTEXT LEVERAGE ######### 
###############################################

# MERGING context leverage (overlap_within) WITH LONGITUDINAL DATA (mcdi_aoa)
context_leverage <- left_join(mcdi_aoa, overlap_within, by = "word")

context_leverage <- select(context_leverage, -word_raw, -word_homonym, -aoa)


###############################################
########   CLEANING FINAL DATA SET  ###########
###############################################

# Analyses predicting AoA from context leverage and control variables
# Analyses are conducted *separately* for context overlap


# First, standardize the predictor variables by converting them to z-scores: OVERLAP
# (using the "scale" function)
context_leverage_scale <- context_leverage
context_leverage_scale[,c("log_freq", "log_freq_final", "utt_length", "abcon", "prop_same", "within_overlap")] <- 
  scale(context_leverage_scale[,c("log_freq", "log_freq_final", "utt_length", "abcon", "prop_same", "within_overlap")])

# View final data set
head(context_leverage_scale)


# cleaning the data set by getting rid of nas 
model_vars <- c(
  "value", "age", "word", "child_id",
  "log_freq", "utt_length", "abcon",
  "prop_same", "within_overlap"
)

# include only observations with no nas
context_leverage_scale <- context_leverage_scale[, model_vars]
context_leverage_scale <- context_leverage_scale[complete.cases(context_leverage_scale), ]

# sanity check 
nrow(context_leverage_scale)


###############################################
########### STATS BEGINNING ###################
###############################################

# load data set 
context_leverage_scale <- readRDS("final dataset/context_leverage_scale.rds")

###############################################
###### JUSTIFY MIXED EFFECTS MODELS ###########
###############################################

# We need to determine whether to account for variability across children and words.
# The dataset contains repeated observations:
# - the same child appears multiple times
# - the same word appears multiple times
# This violates the independence assumption of simple logistic regression (glm).
#
# Therefore, we compare:
# 1) a simple logistic regression model (no random effects)
# 2) a mixed-effects model with random intercepts for child and word


# Simple Logistic regression model --> not account for variability of CHILD or ITEM
if (!file.exists("models/log_reg.rds")) {
  log_reg <- glm(
    value ~ age,
    data = mcdi_aoa,
    family = binomial
  )
  saveRDS(log_reg, "models/log_reg.rds")
} else {
  log_reg <- readRDS("models/log_reg.rds")
}
summary(log_reg)


# random intercepts model, "age" as linear predictor variable → account variability of words and children
if (!file.exists("models/mixed_log_reg.rds")) {
  mixed_log_reg <- glmer(
    value ~ age + (1 | word) + (1 | child_id),
    data = mcdi_aoa,
    family = binomial
  )
  saveRDS(mixed_log_reg, "models/mixed_log_reg.rds")
} else {
  mixed_log_reg <- readRDS("models/mixed_log_reg.rds")
}
summary(mixed_log_reg)


# compare the two models
AIC(log_reg, mixed_log_reg) # the lower the better
# df       AIC
# log_reg        2 174471.04
# mixed_log_reg  4  99536.73


###############################################
###### AGE AS NON-LINEAR PREDICTOR ############
###############################################

# random intercepts model, "age" as linear predictor variable
mixed_log_reg <- readRDS("models/mixed_log_reg.rds")
summary(mixed_log_reg)


# random intercepts model, "age" as non-linear predictor variable
if (!file.exists("models/mixed_log_reg_nonlinear.rds")) {
  mixed_log_reg_nonlinear <- glmer(
    value ~ ns(age, df = 3) + (1 | word) + (1 | child_id),
    data = mcdi_aoa,
    family = binomial
  )
  saveRDS(mixed_log_reg_nonlinear, "models/mixed_log_reg_nonlinear.rds")
} else {
  mixed_log_reg_nonlinear <- readRDS("models/mixed_log_reg_nonlinear.rds")
}
summary(mixed_log_reg_nonlinear)

# compare the two models 
anova(mixed_log_reg, mixed_log_reg_nonlinear, test = "Chisq")
# p < 2.2e-16 → extremely significant
# AIC drops from 99537 → 98236 → huge improvement


###############################################
###### REGRESSION MODEL INCLUDING  ############
######    COUNFOUNDING VARS        ############ 
###############################################

# Now we need to decide what variables to include in our logistic regression model

# Fit model predicting AoA from context overlap leverage and control variables
# Baseline model = log_reg_nonlinear
if (!file.exists("models/log_reg_nonlinear.rds")) {
  # Fit the logistic mixed-effects model
  log_reg_nonlinear <- glmer(
    value ~ 
      ns(age, df = 3) + 
      (1 | word) + 
      (1 | child_id) +
      log_freq + 
      utt_length + 
      abcon + 
      prop_same + 
      within_overlap, 
    data = context_leverage_scale,
    family = binomial
  )
  # Save the fitted model
  saveRDS(log_reg_nonlinear, "models/log_reg_nonlinear.rds")
} else {
  # Load the previously saved model
  log_reg_nonlinear <- readRDS("models/log_reg_nonlinear.rds")
}
summary(log_reg_nonlinear)


###############################################
###### REGRESSION MODEL MAIN EFFECTS ##########
###############################################

# We now want to know which predictors to include in the final model.
# To do this, we run models which include diff number of vars 
# and then we compare them one by one with the baseline model = log_reg_nonlinear
# (# “Does log_freq matter at all?”)


# model_no_logfreq = model without log-freq
if (!file.exists("models/model_no_logfreq.rds")) {
  model_no_logfreq <- update(log_reg_nonlinear, . ~ . - log_freq)
  saveRDS(model_no_logfreq, "models/model_no_logfreq.rds")
} else {
  model_no_logfreq <- readRDS("models/model_no_logfreq.rds")
}
summary(model_no_logfreq)

# compare models
anova(model_no_logfreq, log_reg_nonlinear, test = "Chisq")
# model_no_logfreq   AIC = 75495  
# log_reg_nonlinear  AIC = 75402
# Chisq = 94.96  
# Df = 1  
# Pr(>Chisq) < 2.2e-16
# → Full model has lower AIC and significantly better fit (p < .001); retain log_freq


# no_utt_length = model without utt_length
if (!file.exists("models/model_no_utt.rds")) {
  model_no_utt <- update(log_reg_nonlinear, . ~ . - utt_length)
  saveRDS(model_no_utt, "models/model_no_utt.rds")
} else {
  model_no_utt <- readRDS("models/model_no_utt.rds")
}
summary(model_no_utt)

# compare models
anova(model_no_utt, log_reg_nonlinear, test = "Chisq")
# model_no_utt        AIC = 75486  
# log_reg_nonlinear   AIC = 75402
# Chisq = 85.979  
# Df = 1  
# Pr(>Chisq) < 2.2e-16
# → Full model has lower AIC and significantly better fit (p < .001); retain utt_length


# no_abcon = model without abcon
if (!file.exists("models/model_no_abcon.rds")) {
  model_no_abcon <- update(log_reg_nonlinear, . ~ . - abcon)
  saveRDS(model_no_abcon, "models/model_no_abcon.rds")
} else {
  model_no_abcon <- readRDS("models/model_no_abcon.rds")
}
summary(model_no_abcon)

# compare models
anova(model_no_abcon, log_reg_nonlinear, test = "Chisq")
# model_no_abcon      AIC = 75508  
# log_reg_nonlinear   AIC = 75402
# Chisq = 108.15  
# Df = 1  
# Pr(>Chisq) < 2.2e-16
# → Full model has lower AIC and significantly better fit (p < .001); retain abcon


# no_prop_same = model without prop_same
if (!file.exists("models/model_no_prop.rds")) {
  model_no_prop <- update(log_reg_nonlinear, . ~ . - prop_same)
  saveRDS(model_no_prop, "models/model_no_prop.rds")
} else {
  model_no_prop <- readRDS("models/model_no_prop.rds")
}
summary(model_no_prop)

# compare models
anova(model_no_prop, log_reg_nonlinear, test = "Chisq")
# model_no_prop       AIC = 75411  
# log_reg_nonlinear   AIC = 75402
# Chisq = 11.029  
# Df = 1  
# Pr(>Chisq) = 0.000897
# → Full model has lower AIC and significantly better fit (p < .001); retain prop_same


# no_within_overlap = Model without within_overlap
if (!file.exists("models/model_no_overlap.rds")) {
  model_no_overlap <- update(log_reg_nonlinear, . ~ . - within_overlap)
  saveRDS(model_no_overlap, "models/model_no_overlap.rds")
} else {
  model_no_overlap <- readRDS("models/model_no_overlap.rds")
}
summary(model_no_overlap)

# compare models
anova(model_no_overlap, log_reg_nonlinear, test = "Chisq")
# model_no_overlap    AIC = 75414  
# log_reg_nonlinear   AIC = 75402
# Chisq = 14.737  
# Df = 1  
# Pr(>Chisq) = 0.0001236
# → Full model has lower AIC and significantly better fit (p < .001); retain within_overlap



###############################################
###### REGRESSION MODEL WITH INTERACTION ######
###### DECIDE WHICH INTERACTION TO INCLUDE ####
###############################################

# Test whether adding each age interaction improves model fit
# (i.e. whether the effect of each predictor varies with age)
# “Does the effect of log_freq change depending on age?”


# Base model (no interactions)
if (!file.exists("models/log_reg_nonlinear.rds")) {
  log_reg_nonlinear <- glmer(
    value ~ ns(age, df = 3) +
      log_freq + utt_length + abcon + prop_same + within_overlap +
      (1 | word) + (1 | child_id),
    data = context_leverage_scale,
    family = binomial
  )
  saveRDS(log_reg_nonlinear, "models/log_reg_nonlinear.rds")
} else {
  log_reg_nonlinear <- readRDS("models/log_reg_nonlinear.rds")
}


# age × within_overlap interaction
if (!file.exists("models/model_int_overlap.rds")) {
  model_int_overlap <- glmer(
    value ~ ns(age, df = 3) * within_overlap +
      log_freq + utt_length + abcon + prop_same +
      (1 | word) + (1 | child_id),
    data = context_leverage_scale,
    family = binomial
  )
  saveRDS(model_int_overlap, "models/model_int_overlap.rds")
} else {
  model_int_overlap <- readRDS("models/model_int_overlap.rds")
}
anova(log_reg_nonlinear, model_int_overlap, test = "Chisq")
# log_reg_nonlinear   AIC = 75402  
# model_int_overlap   AIC = 75386
# Chisq = 21.311  
# Df = 3  
# Pr(>Chisq) = 9.073e-05
# → Interaction model has lower AIC and significantly better fit (p < .001); retain age × within_overlap


# age × log_freq interaction
if (!file.exists("models/model_int_logfreq.rds")) {
  model_int_logfreq <- glmer(
    value ~ ns(age, df = 3) * log_freq +
      utt_length + abcon + prop_same + within_overlap +
      (1 | word) + (1 | child_id),
    data = context_leverage_scale,
    family = binomial
  )
  saveRDS(model_int_logfreq, "models/model_int_logfreq.rds")
} else {
  model_int_logfreq <- readRDS("models/model_int_logfreq.rds")
}
anova(log_reg_nonlinear, model_int_logfreq, test = "Chisq")
# log_reg_nonlinear   AIC = 75402  
# model_int_logfreq   AIC = 75380
# Chisq = 27.232  
# Df = 3  
# Pr(>Chisq) = 5.264e-06
# → Interaction model has lower AIC and significantly better fit (p < .001); retain age × log_freq


# age × utt_length interaction
if (!file.exists("models/model_int_utt.rds")) {
  model_int_utt <- glmer(
    value ~ ns(age, df = 3) * utt_length +
      log_freq + abcon + prop_same + within_overlap +
      (1 | word) + (1 | child_id),
    data = context_leverage_scale,
    family = binomial
  )
  saveRDS(model_int_utt, "models/model_int_utt.rds")
} else {
  model_int_utt <- readRDS("models/model_int_utt.rds")
}
anova(log_reg_nonlinear, model_int_utt, test = "Chisq")
# log_reg_nonlinear   AIC = 75402  
# model_int_utt       AIC = 75378
# Chisq = 30.099  
# Df = 3  
# Pr(>Chisq) = 1.316e-06
# → Interaction model has lower AIC and significantly better fit (p < .001); retain age × utt_length


# age × abcon interaction
if (!file.exists("models/model_int_abcon.rds")) {
  model_int_abcon <- glmer(
    value ~ ns(age, df = 3) * abcon +
      log_freq + utt_length + prop_same + within_overlap +
      (1 | word) + (1 | child_id),
    data = context_leverage_scale,
    family = binomial
  )
  saveRDS(model_int_abcon, "models/model_int_abcon.rds")
} else {
  model_int_abcon <- readRDS("models/model_int_abcon.rds")
}
anova(log_reg_nonlinear, model_int_abcon, test = "Chisq")
# log_reg_nonlinear   AIC = 75402  
# model_int_abcon     AIC = 75382
# Chisq = 25.559  
# Df = 3  
# Pr(>Chisq) = 1.179e-05
# → Interaction model has lower AIC and significantly better fit (p < .001); retain age × abcon


# age × prop_same interaction
if (!file.exists("models/model_int_prop.rds")) {
  model_int_prop <- glmer(
    value ~ ns(age, df = 3) * prop_same +
      log_freq + utt_length + abcon + within_overlap +
      (1 | word) + (1 | child_id),
    data = context_leverage_scale,
    family = binomial
  )
  saveRDS(model_int_prop, "models/model_int_prop.rds")
} else {
  model_int_prop <- readRDS("models/model_int_prop.rds")
}
anova(log_reg_nonlinear, model_int_prop, test = "Chisq")
# log_reg_nonlinear   AIC = 75402  
# model_int_prop      AIC = 75382
# Chisq = 25.998  
# Df = 3  
# Pr(>Chisq) = 9.546e-06
# → Interaction model has lower AIC and significantly better fit (p < .001); retain age × prop_same


###############################################
###### JUSTIFY COMBINATION OF INTERACTIONS ####
###############################################

# We now buil a fill interaction model and compare it with models 
# having removed one interaction at time and see which is the better fit
# It is like a gioco ad eliminazione diretta.

# Build a full interaction model = Baseline model 
if (!file.exists("models/full_interaction_model.rds")) {
  full_interaction_model <- glmer(
    value ~ ns(age, df = 3) * log_freq +
      ns(age, df = 3) * utt_length +
      ns(age, df = 3) * abcon +
      ns(age, df = 3) * prop_same +
      ns(age, df = 3) * within_overlap +
      (1 | word) + (1 | child_id),
    data = context_leverage_scale,
    family = binomial
  )
  saveRDS(full_interaction_model, "models/full_interaction_model.rds")
} else {
  full_interaction_model <- readRDS("models/full_interaction_model.rds")
}

# compare models
anova(log_reg_nonlinear, full_interaction_model, test = "Chisq")
# log_reg_nonlinear        AIC = 75402  
# full_interaction_model   AIC = 75313
# Chisq = 118.59  
# Df = 15  
# Pr(>Chisq) < 2.2e-16
# → Full interaction model has lower AIC and significantly better fit (p < .001); interactions should be considered in the model


# Excluding abcon interacion = model_no_int_abcon
if (!file.exists("models/model_no_int_abcon.rds")) {
  model_no_int_abcon <- update(full_interaction_model, . ~ . - ns(age, df = 3):abcon)
  saveRDS(model_no_int_abcon, "models/model_no_int_abcon.rds")
} else {
  model_no_int_abcon <- readRDS("models/model_no_int_abcon.rds")
}

# compare models
anova(model_no_int_abcon, full_interaction_model, test = "Chisq")
# model_no_int_abcon       AIC = 75321  
# full_interaction_model   AIC = 75313
# Chisq = 14.139  
# Df = 3  
# Pr(>Chisq) = 0.002722
# → Removing age × abcon significantly worsens model fit (p < .01); retain interaction


# Excluding log_freq interaction = model_no_int_log_freq
if (!file.exists("models/model_no_int_log_freq.rds")) {
  model_no_int_log_freq <- update(full_interaction_model, . ~ . - ns(age, df = 3):log_freq)
  saveRDS(model_no_int_log_freq, "models/model_no_int_log_freq.rds")
} else {
  model_no_int_log_freq <- readRDS("models/model_no_int_log_freq.rds")
}

# compare models
anova(model_no_int_log_freq, full_interaction_model, test = "Chisq")
# model_no_int_log_freq    AIC = 75349  
# full_interaction_model   AIC = 75313
# Chisq = 42.167  
# Df = 3  
# Pr(>Chisq) = 3.698e-09
# → Removing age × log_freq significantly worsens model fit (p < .001); retain interaction


# Excluding utt_length interaction = model_no_int_utt_length
if (!file.exists("models/model_no_int_utt_length.rds")) {
  model_no_int_utt_length <- update(full_interaction_model, . ~ . - ns(age, df = 3):utt_length)
  saveRDS(model_no_int_utt_length, "models/model_no_int_utt_length.rds")
} else {
  model_no_int_utt_length <- readRDS("models/model_no_int_utt_length.rds")
}

# compare models
anova(model_no_int_utt_length, full_interaction_model, test = "Chisq")
# model_no_int_utt_length   AIC = 75342  
# full_interaction_model    AIC = 75313
# Chisq = 35.388  
# Df = 3  
# Pr(>Chisq) = 1.008e-07
# → Removing age × utt_length significantly worsens model fit (p < .001); retain interaction


# Excluding prop_same interaction = model_no_int_prop_same
if (!file.exists("models/model_no_int_prop_same.rds")) {
  model_no_int_prop_same <- update(full_interaction_model, . ~ . - ns(age, df = 3):prop_same)
  saveRDS(model_no_int_prop_same, "models/model_no_int_prop_same.rds")
} else {
  model_no_int_prop_same <- readRDS("models/model_no_int_prop_same.rds")
}

# compare models
anova(model_no_int_prop_same, full_interaction_model, test = "Chisq")
# model_no_int_prop_same   AIC = 75324  
# full_interaction_model   AIC = 75313
# Chisq = 16.618  
# Df = 3  
# Pr(>Chisq) = 0.0008466
# → Removing age × prop_same significantly worsens model fit (p < .001); retain interaction


# Excluding within_overlap interaction = model_no_int_within_overlap
if (!file.exists("models/model_no_int_within_overlap.rds")) {
  model_no_int_within_overlap <- update(full_interaction_model, . ~ . - ns(age, df = 3):within_overlap)
  saveRDS(model_no_int_within_overlap, "models/model_no_int_within_overlap.rds")
} else {
  model_no_int_within_overlap <- readRDS("models/model_no_int_within_overlap.rds")
}

# compare models
anova(model_no_int_within_overlap, full_interaction_model, test = "Chisq")
# model_no_int_within_overlap   AIC = 75313  
# full_interaction_model        AIC = 75313
# Chisq = 6.1698  
# Df = 3  
# Pr(>Chisq) = 0.1036
# → Removing age × within_overlap does not significantly worsen model fit (p > .05); remove interaction


# The best final model 
summary(model_no_int_within_overlap)



###############################################
########### PLOTS FROM FINAL MODEL ############
###############################################

# load data set 
context_leverage_scale <- readRDS("final dataset/context_leverage_scale.rds")


# Final model used for plotting:
# model_no_int_within_overlap
# Age is modelled non-linearly using ns(age, df = 3).
# Therefore, plots should be interpreted as predicted probability curves,
# not as simple linear effects.



### 1. Effects of predictors on outcome (log-odds), all predictors are included 

# extract coefficients from final model
coefs <- tidy(model_no_int_within_overlap, effects = "fixed")

# remove intercept and age spline terms
coefs_clean <- coefs %>%
  filter(term != "(Intercept)") %>%
  filter(!grepl("ns\\(age", term))

# plot coefficients
p_all_vars <- ggplot(coefs_clean, aes(x = estimate, y = term)) +
  geom_point() +
  geom_errorbarh(
    aes(
      xmin = estimate - std.error,
      xmax = estimate + std.error
    ),
    height = 0.2
  ) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  labs(
    x = "Effect size (log-odds)",
    y = "Predictor",
    title = "Estimated effects of predictors on the outcome (log-odds)"
  ) +
  theme_minimal()

p_all_vars

ggsave(
  "figures/p_all_vars.png",
  plot = p_all_vars,
  width = 8,
  height = 6,
  dpi = 300
)

### 2. Age-only predicted probability curve
# Shows the non-linear developmental trajectory of acquisition across age.

plot_age <- ggpredict(model_no_int_within_overlap, terms = "age")

p_age <- plot(plot_age) +
  scale_y_continuous(limits = c(0, 1)) +
  labs(
    title = "Predicted probability of acquisition across age",
    x = "Age (months)",
    y = "Predicted probability"
  )

p_age

ggsave(
  "figures/p_age.png",
  plot = p_all_vars,
  width = 8,
  height = 6,
  dpi = 300
)

##########    PLOT EACH VAR WITH AGE USING THE FINAL MODEL

### 3. Age × log_freq interaction
# Shows whether the effect of word frequency changes across age.

# Get actual quantiles
log_freq_vals <- quantile(
  context_leverage_scale$log_freq,
  probs = c(0.25, 0.5, 0.75),
  na.rm = TRUE
)

# Use them correctly
plot_log_freq <- ggpredict(
  model_no_int_within_overlap,
  terms = c(
    "age",
    paste0("log_freq [", paste(round(log_freq_vals, 3), collapse = ","), "]")
  )
)

p_log_freq <- plot(plot_log_freq) +
  scale_y_continuous(
    limits = c(0, 1),
    labels = scales::number_format(accuracy = 0.01)
  ) +
  scale_colour_discrete(
    labels = c("Low", "Median", "High"),
    guide = guide_legend(reverse = TRUE)
  ) +
  labs(
    title = "Predicted probability across age by word frequency",
    x = "Age",
    y = "Predicted probability",
    colour = "Word frequency"
  )

p_log_freq

ggsave(
  "figures/p_log_freq.png",
  plot = p_all_vars,
  width = 8,
  height = 6,
  dpi = 300
)

### 4. Age × utt_length interaction
# Shows whether the effect of utterance length changes across age.

# Get actual quantiles
utt_vals <- quantile(
  context_leverage_scale$utt_length,
  probs = c(0.25, 0.5, 0.75),
  na.rm = TRUE
)

# Use them correctly
plot_utt_length <- ggpredict(
  model_no_int_within_overlap,
  terms = c(
    "age",
    paste0("utt_length [", paste(round(utt_vals, 3), collapse = ","), "]")
  )
)

p_utt_length <- plot(plot_utt_length) +
  scale_y_continuous(
    limits = c(0, 1),
    labels = scales::number_format(accuracy = 0.01)
  ) +
  scale_colour_discrete(
    labels = c("Short", "Median", "Long"),
    guide = guide_legend(reverse = TRUE)
  ) +
  labs(
    title = "Predicted probability across age by utterance length",
    x = "Age",
    y = "Predicted probability",
    colour = "Utterance length"
  )

p_utt_length

ggsave(
  "figures/p_utt_length.png",
  plot = p_all_vars,
  width = 8,
  height = 6,
  dpi = 300
)

### 5. Age × abcon interaction
# Shows whether the effect of abstraction/contextual concentration changes across age.

# Get actual quantiles
abcon_vals <- quantile(
  context_leverage_scale$abcon,
  probs = c(0.25, 0.5, 0.75),
  na.rm = TRUE
)

# Use them correctly
plot_abcon <- ggpredict(
  model_no_int_within_overlap,
  terms = c(
    "age",
    paste0("abcon [", paste(round(abcon_vals, 3), collapse = ","), "]")
  )
)

p_abcon <- plot(plot_abcon) +
  scale_y_continuous(
    limits = c(0, 1),
    labels = scales::number_format(accuracy = 0.01)
  ) +
  scale_colour_discrete(
    labels = c("Low", "Median", "High"),
    guide = guide_legend(reverse = TRUE)
  ) +
  labs(
    title = "Predicted probability across age by concreteness level",
    x = "Age",
    y = "Predicted probability",
    colour = "Concreteness level"
  )

p_abcon

ggsave(
  "figures/p_abcon.png",
  plot = p_all_vars,
  width = 8,
  height = 6,
  dpi = 300
)

### 6. Age × prop_same interaction
# Shows whether the effect of prop_same changes across age.
# This is especially useful because prop_same had no clear main effect,
# but its interaction with age was retained.

# Get actual quantiles
prop_vals <- quantile(
  context_leverage_scale$prop_same,
  probs = c(0.25, 0.5, 0.75),
  na.rm = TRUE
)

# Use them properly in ggpredict
plot_prop_same <- ggpredict(
  model_no_int_within_overlap,
  terms = c(
    "age",
    paste0("prop_same [", paste(round(prop_vals, 3), collapse = ","), "]")
  )
)

p_prop_same <- plot(plot_prop_same) +
  scale_y_continuous(
    limits = c(0, 1),
    labels = scales::number_format(accuracy = 0.01)
  ) +
  scale_colour_discrete(
    labels = c("Low", "Median", "High"),
    guide = guide_legend(reverse = TRUE)
  ) +
  labs(
    title = "Predicted probability across age by same-category proportion",
    x = "Age",
    y = "Predicted probability",
    colour = "same-category"
  )

p_prop_same

ggsave(
  "figures/p_prop_same.png",
  plot = p_all_vars,
  width = 8,
  height = 6,
  dpi = 300
)

### 7. within_overlap main effect only
# The age × within_overlap interaction was removed from the final model.
# Therefore, within_overlap should be plotted as a main effect, not as an interaction with age.

# within_overlap shown across age (low vs high only)

overlap_vals <- quantile(
  context_leverage_scale$within_overlap,
  probs = c(0.25, 0.75),
  na.rm = TRUE
)

plot_within_overlap <- ggpredict(
  model_no_int_within_overlap,
  terms = c(
    "age",
    paste0("within_overlap [", paste(round(overlap_vals, 3), collapse = ","), "]")
  )
)

p_within_overlap <- plot(plot_within_overlap) +
  scale_y_continuous(
    limits = c(0, 1),
    labels = scales::number_format(accuracy = 0.01)
  ) +
  scale_colour_manual(
    values = c("#F8766D", "#619CFF"),  # Low = red, High = blue
    labels = c("Low", "High"),
    guide = guide_legend(reverse = TRUE)  # 👈 this flips legend order
  ) +
  labs(
    title = "Predicted probability across age by context overlap",
    x = "Age",
    y = "Predicted probability",
    colour = "Context overlap"
  )

p_within_overlap

ggsave(
  "figures/p_within_overlap.png",
  plot = p_all_vars,
  width = 8,
  height = 6,
  dpi = 300
)

### Multipanel showing all the graphs with predictors that have interaction with age 

# --- Update titles (cleaner, shorter) ---
p_log_freq <- p_log_freq +
  labs(title = "Across age by word frequency")

p_utt_length <- p_utt_length +
  labs(title = "Across age by utterance length")

p_abcon <- p_abcon +
  labs(title = "Across age by concreteness")

p_prop_same <- p_prop_same +
  labs(title = "Across age by same-category proportion")

p_within_overlap <- p_within_overlap +
  labs(title = "Across age by context overlap")


# --- Remove repeated y-axis labels from right-column plots ---
p_utt_length <- p_utt_length + labs(y = NULL)
p_prop_same  <- p_prop_same + labs(y = NULL)


# --- Remove x-axis labels from top and middle rows ---
p_log_freq   <- p_log_freq + labs(x = NULL)
p_utt_length <- p_utt_length + labs(x = NULL)
p_abcon      <- p_abcon + labs(x = NULL)

# Keep x-axis only for p_prop_same and p_within_overlap
p_prop_same <- p_prop_same + labs(x = "Age")


# --- Multipanel plot ---
multi_panel_plot <- (
  p_log_freq + p_utt_length +
    p_abcon + p_prop_same +
    p_within_overlap + plot_spacer()
) +
  plot_layout(ncol = 2) +
  plot_annotation(
    title = "Predicted probability across age by key predictors",
    theme = theme(
      plot.title = element_text(face = "bold", hjust = 0.5)
    )
  )

multi_panel_plot

ggsave(
  "figures/multi_panel_plot.png",
  plot = p_all_vars,
  width = 8,
  height = 6,
  dpi = 300
)




#########################




