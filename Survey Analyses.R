## Download packages needed for the analyses
library(readr)
library(dplyr)
library(tidyr)
library(dplyr)
library(stringr)

## Download the dataset
DTD_raw <- read_csv("~/Desktop/DTD.csv")

## Remove columns and rows that are not needed
DTD_clean <- DTD_raw %>% select(-StartDate, -EndDate, 
                                -Status, -Progress, -'Duration (in seconds)', 
                                -RecordedDate, -DistributionChannel, 
                                -UserLanguage, -Finished, -'D2_4_TEXT', 
                                -'DTD3_1_TEXT', -'DTD4_1_TEXT', -Q230, 
                                -Q231, -ALLO, -EGO, -LM, -Q219)
DTD_clean <- DTD_clean [-c(1, 2),]

## Remove row 145, because address ipv gender
DTD_clean <- DTD_clean[-145, ]

## Make categories for age
# Turn age into a categorical variable
DTD_age <- DTD_clean

DTD_age$D1 <- as.numeric(as.character(cut(as.numeric(DTD_age$D1), breaks = c(15, 39, 65, Inf), labels = c(0, 1, 2))))

DTD_age$age_category <- cut(DTD_age$D1, breaks = c(15, 39, 65, Inf), labels = c(0, 1, 2), right = TRUE, include.lowest = TRUE)

DTD_age <- DTD_age %>% select(-'age_category')

## Make categories for gender
# Make gender a categorical variable
DTD_gen <- DTD_age

DTD_gen$D2 <- as.character(DTD_age$D2)

# Turn "Other:" into "Non-Binary"
DTD_gen$D2[DTD_age$D2 == "Other:"] <- "Non-Binary"

# Turn age into three categories
DTD_gen$D2 <- recode(DTD_age$D2, "Male" = 0, "Female" = 1, "Non-Binary" = 2)

## Making the urban/rural categories
DTD_urb <- DTD_gen %>%
  mutate(D3 = case_when(
    D3 == "Urban area (characterized by higher population density, significant infrastructure, and more developed environments with taller buildings, extensive transportation networks (buses, subways), and a variety of services.)" ~ 0,
    D3 == "Rural area (regions outside cities and towns, typically with lower population density and more open spaces with smaller communities, fewer buildings, and limited public transportation.)" ~ 1,
    TRUE ~ NA_real_  # optional: handles any unexpected cases
  ))

## Making the education categories
DTD_ed <- DTD_urb %>%
  mutate(D4 = case_when(
    D4 %in% c("Not finished primary school", "Primary school", 
              "Not finished secondary school", "Secondary school") ~ 0,
    D4 == "Vocational degree" ~ 1,
    D4 %in% c("Undergraduate degree (Bachelors level)", 
              "Postgraduate degree (Masters or PhD level)") ~ 2,
    TRUE ~ NA_real_
  ))

## DTD questionnaire -> turn Yes/No into 0/1
# Turn all the missing values for DTD2 into 0 and turn yes/no into 0/1
DTD_DT <- DTD_ed %>%
  mutate(
    DTD1 = ifelse(DTD1 == "Yes", 1, 0),
    DTD2 = ifelse(DTD2 == "Yes", 1, 0),
    DTD2 = replace_na(DTD2, 0)
  )

# Turn "Yes, namely" and "No" into 1 and 0
DTD_34 <- DTD_DT %>%
  mutate(
    DTD3 = ifelse(trimws(tolower(as.character(DTD3))) == "yes, namely", 1, 0),
    DTD4 = ifelse(trimws(tolower(as.character(DTD4))) == "yes, namely", 1, 0)
  )

## Turn the sentences of the VVIQ into their respective scale
DTD_VVIQ <- DTD_34

## Make a scale (the way it is now is the correct version)
mapping <- c(
  "Perfectly clear and as vivid as normal vision" = 5,
  "Clear and reasonably vivid" = 4,
  "Moderately clear and vivid" = 3,
  "Vague and dim" = 2,
  "No image at all, you only “know” that you are thinking of an object" = 1
)

DTD_VVIQ <- DTD_VVIQ %>%
  mutate(across(10:25, ~ recode(.x, !!!mapping)))

## Pray to all the Gods let's go, remove missing data
DTD_final <- na.omit(DTD_VVIQ)

DTD_ready <- DTD_final[, -1]

### ACTUAL ANALYSES
## Make a new column for the total scores of DTD1 and DTD2
DTD_totalDTD <- DTD_ready

DTD_totalDTD$DTD_total <- DTD_totalDTD$DTD1 + DTD_totalDTD$DTD2

## Make a new column that gives a 1 for everyone with DTD and a 0 for those without
DTD_totalDTD$DTD_d <- ifelse(DTD_totalDTD$DTD_total == 2, 1, 0)

## Make a new column for the total scores of all the VVIQ columns
DTD_totalVVIQ <- DTD_totalDTD

DTD_totalVVIQ$VVIQ_total <- rowSums(DTD_totalVVIQ[, c("VVIQ-1", "VVIQ-2", "VVIQ-3", "VVIQ-4", "VVIQ-5", "VVIQ-6", 
                                              "VVIQ-7", "VVIQ-8", "VVIQ-9", "VVIQ-10", "VVIQ-11", 
                                              "VVIQ-12", "VVIQ-13", "VVIQ-14", "VVIQ-15", "VVIQ-16")], 
                                na.rm = TRUE)

## Make a new column that gives "1" for aphantasia (</- 32) and "0" for not (32+)
DTD_aphantasia <- DTD_totalVVIQ

DTD_aphantasia$aphantasia <- ifelse(DTD_aphantasia$VVIQ_total <= 32, 1, 0)

## Make a column for a total score of DTD and aphantasia 
aphantasiaDTD <- DTD_aphantasia

aphantasiaDTD$combination_DTDaph <- aphantasiaDTD$DTD_d + aphantasiaDTD$aphantasia

## Make a column that shows the people that have both aphantasia and DTD
DTDboth <- aphantasiaDTD

DTDboth <- DTDboth %>%
  mutate(Both = ifelse(combination_DTDaph == 2, 1, 0))

## Make a column that shows the people who have neither DTD nor aphantasia
DTDneither <- DTDboth

DTDneither <- DTDneither %>%
  mutate(Neither = ifelse(combination_DTDaph == 0, 1, 0))

## Create mutually exlusive columns for all four options
Finaldf <- DTDneither

Finaldf <- Finaldf %>%
  mutate(
    DTD_only = ifelse(DTD_d == 1 & aphantasia == 0, 1, 0),
    Aph_only = ifelse(DTD_d == 0 & aphantasia == 1, 1, 0),
    Both     = ifelse(DTD_d == 1 & aphantasia == 1, 1, 0),
    Neither  = ifelse(DTD_d == 0 & aphantasia == 0, 1, 0)
  )

### TIME FOR THE ANALYSES <333
## Calculate the prevalences for DTD, aphantasia, and Both
prevalenceDTD <- mean(Finaldf$DTD_only)
prevalenceDTD

prevalenceAPH <- mean(Finaldf$Aph_only)
prevalenceAPH

prevalenceBoth <- mean(Finaldf$Both)
prevalenceBoth

prevalenceNeither <- mean(Finaldf$Neither)
prevalenceNeither

## Distributions per demographic
# Convert 'group' to a factor (if not already)
Analysesdf <- Finaldf

# List your demographic variables
demographics <- c("D1", "D2", "D3", "D4")

# Function to calculate prevalence for one demographic variable
dem_prevalence <- function(var) {
  Analysesdf %>%
    group_by(.data[[var]]) %>%
    summarise(
      count = n(),
      Aph_only = mean(Aph_only) * 100,
      DTD_only = mean(DTD_only) * 100,
      Both = mean(Both) * 100,
      Neither = mean(Neither) * 100
    ) %>%
    rename(!!var := .data[[var]])
}


# Apply it to all demographics and store results in a list
prevalence_tables <- lapply(demographics, dem_prevalence)

# Optionally name the list
names(prevalence_tables) <- demographics

# To view the result for e.g. gender:
prevalence_tables$D1  # For demographic D1
prevalence_tables$D2  # For D2
prevalence_tables$D3  # For D3
prevalence_tables$D4  # For D4

## Fischer's table's <3
conditions <- c("DTD_only", "Aph_only", "Both", "Neither")

for (d in demographics) {
  for (c in conditions) {
    cat("Fisher's Exact Test for", d, "vs", c, ":\n")
    tbl <- table(Analysesdf[[d]], Analysesdf[[c]])
    print(tbl)
    print(fisher.test(tbl))
    cat("\n----------------------\n")
  }
}

## Table of the demographics
# New packages for plots
library(ggplot2)
library(forcats)
library(scales)

# Make groups for tables
Analysesdf <- Analysesdf %>%
  mutate(group = factor(group, levels = c("Neither", "DTD only", "Aphantasia only", "Both")))

Analysesdf <- Analysesdf %>%
  mutate(
    group = case_when(
      DTD_only == 1 & Aph_only == 0 ~ "DTD only",
      DTD_only == 0 & Aph_only == 1 ~ "Aphantasia only",
      DTD_only == 1 & Aph_only == 1 ~ "Both",
      TRUE ~ "Neither"
    )
  )

# Make long format for table
DTDgraph_long <- Analysesdf %>%
  mutate(demo = "Age", level = as.character(D1)) %>%
  bind_rows(Analysesdf %>% mutate(demo = "Gender", level = as.character(D2))) %>%
  bind_rows(Analysesdf %>% mutate(demo = "Living area", level = as.character(D3))) %>%
  bind_rows(Analysesdf %>% mutate(demo = "Education", level = as.character(D4)))

# Make graph
DTDgraph_long %>%
  group_by(demo, level, group) %>%
  summarise(n = n(), .groups = "drop") %>%
  group_by(demo, level) %>%
  mutate(prop = n / sum(n)) %>%
  ggplot(aes(x = fct_inorder(level), y = prop, fill = group)) +
  geom_col() +
  facet_wrap(~demo, scales = "free_x") +
  scale_y_continuous(labels = percent_format(accuracy = 1)) +
  scale_fill_manual(values = c(
    "Neither" = "#FFE1FF",
    "DTD only" = "#FFC0CB",
    "Aphantasia only" = "darkseagreen1",
    "Both" = "lightcyan1"
  )) +
  labs(
    title = "Prevalence of DTD / Aphantasia Groups by Demographics",
    x = "Demographic Level",
    y = "Proportion",
    fill = "Group"
  ) +
  theme_minimal()

## alternative graph
DTDgraph_long %>%
  group_by(demo, level, group) %>%
  summarise(n = n(), .groups = "drop") %>%
  group_by(demo, level) %>%
  mutate(prop = n / sum(n)) %>%
  ggplot(aes(x = fct_inorder(level), y = prop, fill = group)) +
  geom_col(position = position_dodge()) +
  geom_text(aes(label = n), position = position_dodge(width = 0.9), vjust = -0.5) +
  facet_wrap(~demo, scales = "free_x") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  scale_fill_manual(values = c(
    "Neither" = "#FFE1FF",
    "DTD only" = "#FFC0CB",
    "Aphantasia only" = "darkseagreen1",
    "Both" = "lightcyan1"
  )) +
  labs(
    title = "Prevalence of DTD / Aphantasia Groups by Demographics",
    x = "Demographic Level",
    y = "Proportion",
    fill = "Group"
  ) +
  theme_minimal()





















