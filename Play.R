
# 

# load packages
library(dplyr)
library(readr)
library(ggplot2)
library(splines)
library(lme4)

# load data set AOA
aoa <- read_csv("data/mcdi_raw.csv")

# Dataset mcdi_raw variables:
#
# child_id:
#   Unique identifier for each child.
#
# age:
#   Child's age at the time of assessment. Some children may have multiple
#   entries at different ages if their parents completed the MCDI more than once,
#   while others may only have one entry.
#
# word:
#   The word included in the dataset. This has already been processed so that
#   it matches the word forms used in the lemmatised CHILDES corpus.
#
# word_raw:
#   The original word form as it appeared on the MCDI.
#
# word_homonym:
#   Disambiguation information for homonymous words in the MCDI
#   (e.g. "orange" as a food vs "orange" as a colour). For such cases,
#   this column specifies which meaning the row refers to.
#
# value:
#   Indicates whether the child knows the word:
#   1 = child knows the word
#   0 = child does not know the word
#
# aoa_fit:
#   The overall Age of Acquisition (AoA) estimate for the word, calculated
#   from the full dataset. Specifically, this is the age at which 50% of
#   children are estimated to know the word.



# longitudinal_aoa data 
longitudinal_aoa <- aoa %>%
  group_by(child_id) %>%
  filter(n_distinct(age) >= 4) %>%
  ungroup()

# Logistic regression model IV = age, DV = value
logistic_regression <- glm(value ~ age,
                       data = longitudinal_aoa,
                       family = binomial)
summary(logistic_regression)
# age Estimate = 0.274572



# log-odds of knowing a word = (−6.82 + 0.27 × age)
# key result.
# Positive (0.274) → as age increases, probability of knowing a word increases
# Very large z-value (165) and tiny p-value (< 2e-16) → extremely strong effect

# Think of deviance as a measure of how wrong the model is:
# Higher deviance = worse fit
# Lower deviance = better fit

# Null deviance (208,923): 
  # This is the baseline model with no predictors (age not included)
    # The model predicts the same probability for all observations
      # This results in a poor fit → high deviance

# Residual deviance (174,467):
  # This is the model including age as a predictor
    # The model adjusts predictions based on age
      # Better fit → lower deviance

# Change in deviance (208,923 → 174,467):
  # The large drop indicates that adding age greatly improves the model
    # This means age explains a substantial amount of variation in word knowledge

######### plot results

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







#####################################################   LOG MODEL 

# Simple Logistic regression model --> not account for variability of CHILD or ITEM
log_reg <- glm(value ~ age,
               data = longitudinal_aoa,
               family = binomial)
summary(log_reg)
# age Estimate = 0.274572

########################################################## MIXED MODEL  

# Fixed effect:
# age → estimates the overall effect of age 
# on the probability of knowing a word
# across the entire dataset 


# Random effects:
# (1 + age | word)

# (1 | word) → random intercept       -->  VARIABILITY OF WORDS             
# allows each word to have a different
# baseline probability of being known
# (some words are easier and learned 
# earlier than others)

# (age | word) → random slope         --> VARIABILITY IN THE EFFECT OF AGE ACROSS WORDS & CHILD      
# allows the effect of age to vary across words
# meaning that some words may increase in probability of 
# being known faster than others                          

##### RANDOM INTERCEPTS MODEL            -->  VARIABILITY OF WORDS & CHILD
mixed_log_reg <- glmer(value ~ age + (1 | word) + (1 | child_id),
                       data = longitudinal_aoa,
                       family = binomial)
summary(mixed_log_reg)

## FIXED

# age  Estimate = 0.587801
# age increases, the log-odds of knowing a word increase.

## RANDOM
# word variance     = 3.389
# child variance    = 4.181


##################################### LOG MODEL  "AGE" AS NON LINEAR PREDICTOR VAR
log_reg_nonlinear <- glm(value ~ ns(age, df = 3),
                         data = longitudinal_aoa,
                         family = binomial)
summary(log_reg_nonlinear)
# how does R divide the vector age with df = 3 ???????????????????????????


##### RANDOM INTERCEPTS MODEL, "AGE" AS NON LINEAR PREDICTOR VAR          -->  VARIABILITY OF WORDS & CHILD
mixed_log_reg_nonlinear <- glmer(value ~ ns(age, df = 3) + (1 | word) + (1 | child_id),
                       data = longitudinal_aoa,
                       family = binomial)
summary(mixed_log_reg_nonlinear)


#######################################  COMPARE
# Compare two mixed-effects logistic regression models using ANOVA
# This tests whether modelling age as a NON-LINEAR predictor improves model fit.
anova(mixed_log_reg, mixed_log_reg_nonlinear)

# mixed_log_reg
# Random intercepts model
# Age is included as a LINEAR predictor.

# mixed_log_reg_nonlinear
# Random intercepts model
# age is modelled as a NON-LINEAR predictor using splines.

# The ANOVA comparison tests whether the nonlinear age effect
# provides a significantly better fit to the data than the
# linear age effect.

# Higher deviance = worse fit
# Lower deviance = better fit
# look at the p-value






























#################################################################################################

# load packages
library(dplyr)
library(readr)
library(ggplot2)
library(splines)
library(lme4)

#####################################################
################## LOAD MCDI DATA ###################
#####################################################

# Load age of acquisition data (calculated from MCDI data)
mcdi_aoa <- read_csv("data/mcdi_raw.csv")

# longitudinal_aoa data 
mcdi_aoa <- mcdi_aoa %>%
  group_by(child_id) %>%
  filter(n_distinct(age) >= 4) %>%
  ungroup()














# 1 - to create the summary_table 
# load my longitudinal_data
## use it to create another tibble
### tibble very similar to the original mcdi object --> |word --> aoa_fit|


# 1 -  copy of Rscript (Layla's code)
## look where it uses the mcdi_aoa table mcdi_aoa
### instead of MCDI_AoA_english.csv (her data), I want my longitudinal_data (my data)
#### create a summary_table that has |word| aoa_fit| 
##### use summary table when calculating context leverage 
###### use the longitudinal_data to create 

# Combine dataframe containing context overlap between word pairs
# with AoA (age of acquisiton) for each word in each pair
overlap_aoa <- overlap %>%
  left_join(dplyr::select(mcdi_aoa, word, aoa_fit), by = c("word1" = "word")) %>%
  dplyr::rename(aoa1 = aoa_fit) %>%
  left_join(dplyr::select(mcdi_aoa, word, aoa_fit), by = c("word2" = "word")) %>%
  dplyr::rename(aoa2 = aoa_fit)



# 1 - copy layla's script
# 2 - create summary_table (word, aoa_fit) from my longitudinal_data --> USE DISTINCT to eliminate doubles
# 3 - start calculating CONTXT LEVERAGE using my summary_data

# Combine dataframe containing context overlap between word pairs
# with AoA (age of acquisiton) for each word in each pair
overlap_aoa <- overlap %>%
  left_join(dplyr::select(summary_table, word, aoa_fit), by = c("word1" = "word")) %>%
  dplyr::rename(aoa1 = aoa_fit) %>%
  left_join(dplyr::select(summary_table, word, aoa_fit), by = c("word2" = "word")) %>%
  dplyr::rename(aoa2 = aoa_fit)


# 4 - actual calculation of context leverage . UNCHAGED --> CONTEXT LEVERAGE

# 5 - - merge overlap_aoa (CONTEXT LEVERAGE) with the longitudnal_data --> NEW BIT










