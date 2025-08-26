### Load the dataset
frequencies <- read.csv("them_analysis.csv", header = TRUE, sep = ";")
head(frequencies)

## set the first column as rownames
rownames(frequencies) <- frequencies[,1]
head(frequencies)

## Remove unnessecary accidental column
frequencies$X.1 <- NULL
frequencies <- frequencies[, -1]


### Perform a Crawford & Howell's modified t-test
install.packages("singcar")   # if not installed
library(singcar)

## First we clarify which rows contain control variables and case variables
# Controls
frequencies_ctrls <- data.frame(
  anxiety        = c(0.09, 0.20, 0.01, 0.10, 0.06, 0.15, 0.13, 0.12, 0.04),
  contextualizing = c(0.11, 0.13, 0.13, 0.06, 0.06, 0.06, 0.09, 0.16, 0),
  coping         = c(0.01, 0.08, 0.04, 0.07, 0.23, 0.12, 0.19, 0.11, 0.05),
  DepRes         = c(0, 0.03, 0.11, 0.09, 0.09, 0.16, 0.17, 0.13, 0.09),
  NegEm          = c(0.07, 0.08, 0.12, 0.16, 0.15, 0.06, 0.15, 0.05, 0.04),
  NegSelf        = c(0.06, 0.17, 0.11, 0.14, 0.06, 0.06, 0.09, 0.14, 0.02),
  Social         = c(0.09, 0.12, 0.06, 0.15, 0, 0.09, 0.06, 0.03, 0.03)
)


# Case 006
casescore006 <- list(
  anxiety        = 0.05,
  contextualizing = 0.17,
  coping         = 0.04,
  DepRes         = 0.06,
  NegEm          = 0.09,
  NegSelf        = 0.11,
  Social         = 0.06
)

# Case 007
casescore007 <- list(
  anxiety        = 0.07,
  contextualizing = 0.03,
  coping         = 0.05,
  DepRes         = 0.06,
  NegEm          = 0.04,
  NegSelf        = 0.03,
  Social         = 0.30
)

## Actual t-tests
library(singcar)

# anxiety
TD(
  case = as.numeric(casescore006$anxiety),
  controls = as.numeric(frequencies_ctrls$anxiety)
)

TD(
  case = as.numeric(casescore007$anxiety),
  controls = as.numeric(frequencies_ctrls$anxiety)
)

# contextualizing
TD(
  case = as.numeric(casescore006$contextualizing),
  controls = as.numeric(frequencies_ctrls$contextualizing)
)

TD(
  case = as.numeric(casescore007$contextualizing),
  controls = as.numeric(frequencies_ctrls$contextualizing)
)

# coping
TD(
  case = as.numeric(casescore006$coping),
  controls = as.numeric(frequencies_ctrls$coping)
)

TD(
  case = as.numeric(casescore007$coping),
  controls = as.numeric(frequencies_ctrls$coping)
)

# DepRes
TD(
  case = as.numeric(casescore006$DepRes),
  controls = as.numeric(frequencies_ctrls$DepRes)
)

TD(
  case = as.numeric(casescore007$DepRes),
  controls = as.numeric(frequencies_ctrls$DepRes)
)

# NegEm
TD(
  case = as.numeric(casescore006$NegEm),
  controls = as.numeric(frequencies_ctrls$NegEm)
)

TD(
  case = as.numeric(casescore007$NegEm),
  controls = as.numeric(frequencies_ctrls$NegEm)
)

# NegSelf
TD(
  case = as.numeric(casescore006$NegSelf),
  controls = as.numeric(frequencies_ctrls$NegSelf)
)

TD(
  case = as.numeric(casescore007$NegSelf),
  controls = as.numeric(frequencies_ctrls$NegSelf)
)

# Social
TD(
  case = as.numeric(casescore006$Social),
  controls = as.numeric(frequencies_ctrls$Social)
)

TD(
  case = as.numeric(casescore007$Social),
  controls = as.numeric(frequencies_ctrls$Social)
)

### Effect sizes in the form of NAP and Tau-U
## NAP
# Install if you haven't already
install.packages("SingleCaseES")

# Load it
library(SingleCaseES)

library(SingleCaseES)

# Use categories that exist in both cases (all of them but R wants me to clarify ig)
common_vars <- intersect(names(frequencies_ctrls), names(casescore006))  # works for both cases

# Calculate nap for 006 and 007
nap_results_006 <- sapply(common_vars, function(var) {
  NAP(frequencies_ctrls[[var]], casescore006[[var]])$Est
})

nap_results_007 <- sapply(common_vars, function(var) {
  NAP(frequencies_ctrls[[var]], casescore007[[var]])$Est
})

# Make a little table to make interpretation more clear
nap_table <- data.frame(
  Category = names(casescore006),
  Case_006 = nap_results_006,
  Case_007 = nap_results_007
)

nap_table
