### Load the dataset
library(tibble)
library(dplyr)

Sentiment <- read_delim(
  "~/Desktop/RL&DM/Sentiment.csv", 
  delim = ";", escape_double = FALSE, trim_ws = TRUE
)

Sentiment <- Sentiment %>%
  column_to_rownames(var = "Participants:")

## set the first column as rownames
rownames(Sentiment) <- Sentiment[,1]
head(Sentiment)

## Remove unnecessary columns and rows that are in the dataset
Sentiment <- Sentiment %>% 
  select(-`Sentiment 1`, -`Sentiment 2`, -`Sentiment 3`)

Sentiment2 <- Sentiment[-nrow(Sentiment), ]

### Perform a Crawford & Howell's modified t-test
library(singcar)

## First we clarify which rows contain control variables and case variables
# Controls
sentiment_ctrls <- data.frame(
  cat1 = c(0.10, 0.20, 0.02, 0.04, 0.06, 0.22, 0.16, 0.06, 0.04),
  cat2 = c(0.11, 0.21, 0.00, 0.04, 0.00, 0.04, 0.07, 0.29, 0.07),
  cat3 = c(0.04, 0.20, 0.00, 0.28, 0.12, 0.12, 0.12, 0.04, 0.00)
)

# Case 006
casescore006 <- list(
  cat1 = 0.06,
  cat2 = 0.04,
  cat3 = 0.04
)

# Case 007
casescore007 <- list(
  cat1 = 0.04,
  cat2 = 0.14,
  cat3 = 0.04
)

## Actual t-tests
library(singcar)

# Category 1 
TD(
  case = as.numeric(casescore006$cat1),
  controls = as.numeric(sentiment_ctrls$cat1)
)

TD(
  case = as.numeric(casescore007$cat1),
  controls = as.numeric(sentiment_ctrls$cat1)
)

# Category 2 
TD(
  case = as.numeric(casescore006$cat2),
  controls = as.numeric(sentiment_ctrls$cat2)
)

TD(
  case = as.numeric(casescore007$cat2),
  controls = as.numeric(sentiment_ctrls$cat2)
)

# Category 3 
TD(
  case = as.numeric(casescore006$cat3),
  controls = as.numeric(sentiment_ctrls$cat3)
)

TD(
  case = as.numeric(casescore007$cat3),
  controls = as.numeric(sentiment_ctrls$cat3)
)

### Effect sizes in the form of NAP
## NAP
# Install necessary packages
library(SingleCaseES)

# Use categories that exist in both cases (all of them but R wants me to clarify ig)
common_vars <- intersect(names(sentiment_ctrls), names(casescore006))  # works for both cases

# Calculate nap for 006 and 007
nap_results_006 <- sapply(common_vars, function(var) {
  NAP(sentiment_ctrls[[var]], casescore006[[var]])$Est
})

nap_results_006

nap_results_007 <- sapply(common_vars, function(var) {
  NAP(sentiment_ctrls[[var]], casescore007[[var]])$Est
})

nap_results_007

# Make a little table to make interpretation more clear
nap_table <- data.frame(
  Category = names(casescore006),
  Case_006 = nap_results_006,
  Case_007 = nap_results_007
)

nap_table