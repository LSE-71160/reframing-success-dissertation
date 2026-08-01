# ================================================
# DISSERTATION SURVEY ANALYSIS - FINAL VERSION
# Reframing Success: Public Perceptions of Life After Incarceration
#Candidate Number: 71160
# ================================================

# ---- SETUP ----
# Install packages once only - comment out after first run
# install.packages("tidyverse")
# install.packages("gridExtra")
# install.packages("broom")

# Load packages every session - required for all functions used below
library(tidyverse)  # data manipulation and visualisation
library(gridExtra)  # arranging multiple plots side by side
library(broom)      # extracting regression coefficients for visualisation

# ================================================
# SECTION 1: DATA LOADING AND CLEANING
# ================================================

# ---- LOAD DATA ----
# read.csv() loads the Qualtrics CSV export into R
# file.choose() opens a file browser to select the file manually
# skip = 1 removes the first Qualtrics metadata header row
data <- read.csv(file.choose(), skip = 1)

# ---- INITIAL DATA CHECK ----
# Diagnostic checks only - do not change the data
# head() previews first 6 rows to confirm data loaded correctly
# nrow() counts total rows - expected ~308
# names() lists all column names
head(data)
nrow(data)
names(data)

# ---- REMOVE JUNK ROW ----
# Qualtrics exports one remaining metadata row even with skip=1
# data[-1, ] means keep everything EXCEPT row 1
# The comma means for all columns - rows and columns separated by comma in R
data <- data[-1, ]

# ---- CHECK CONDITION SPLIT ----
# table() counts how many participants landed in each condition
# $ selects a specific column from a dataset
# Expecting roughly equal numbers in A, B and C (~100 each)
# Unequal groups would suggest the randomizer did not work correctly
table(data$Condition)

# ---- RENAME COLUMNS ----
# Qualtrics exports full question text as column names - too long to use
# Renaming using column position numbers (22, 23 etc.) instead
# Format is always: new_name = old_name or position number
# Good column names should be short, lowercase, and descriptive
data <- data %>%
  rename(
    perceptions_progress = 22,      # Jordan has made concrete progress
    perceptions_attention = 23,     # Attention check item
    perceptions_emotional = 24,     # Jordan seems to be doing well emotionally
    perceptions_ability = 25,       # Jordan has the ability to rebuild his life
    policy_investment = 26,         # Government should invest more in reentry programs
    policy_recidivism_measure = 27, # Returning to prison is a good measure of success
    policy_support = 28,            # People from prison should not expect govt support
    deservingness_society = 29,     # People like Jordan deserve support from society
    deservingness_govt = 30,        # People released from prison deserve govt support
    deservingness_responsibility = 31, # Rebuilding life is own responsibility
    social_neighbor = 32,           # Comfortable having Jordan as a neighbor
    social_coworker = 33,           # Comfortable working alongside Jordan
    social_family = 34,             # Comfortable introducing Jordan to family
    success_not_returning = 35,     # Not returning to prison - importance rating
    success_housing = 36,           # Stable housing - importance rating
    success_employment = 37,        # Employment - importance rating
    success_financial = 38,         # Financial independence - importance rating
    success_family = 39,            # Family relationships - importance rating
    success_health = 40,            # Physical/mental health - importance rating
    success_community = 41,         # Community involvement - importance rating
    success_education = 42,         # Education - importance rating
    age = 43,                       # Participant age
    gender = 44,                    # Participant gender
    gender_text = 45,               # Gender self description text
    race = 46,                      # Participant race/ethnicity
    race_text = 47,                 # Race self description text
    education = 48,                 # Participant education level
    politics = 49,                  # Political orientation (1=very liberal, 5=very conservative)
    prior_contact = 50,             # Prior contact with incarceration (1=yes, 2=no)
    contact_shaped_views = 51,      # Did contact shape views (1=yes, 2=no)
    contact_how_much = 52,          # How much did contact shape views
    cj_interest = 53                # CJ interest (1=extremely interested, 3=not at all)
  )

# ---- ATTENTION CHECK VERIFICATION ----
# Before filtering, check how many people passed and failed
# Attention check asked participants to select Somewhat agree = value 2
# Anyone who did not select 2 failed the attention check
# table() shows distribution of responses - vast majority should be 2
table(data$perceptions_attention)

# ---- FILTER OUT ATTENTION CHECK FAILURES ----
# Keep only participants who correctly selected 2 (Somewhat agree)
# == means is equal to - different from = which means assign
# Saving into data_clean preserves the original data object
# nrow() confirms filter worked - expected 304 (307 minus 3 failures)
data_clean <- data %>%
  filter(perceptions_attention == 2)

nrow(data_clean)

# ---- CONVERT CONDITION TO FACTOR ----
# condition contains categories A, B, C not numbers
# as.factor() tells R these are distinct experimental groups
# Required for ANOVA and regression to handle condition correctly
# Note: using data_clean$Condition with capital C as rename did not change this column
data_clean$condition <- as.factor(data_clean$Condition)

# ---- POST FILTERING BALANCE CHECK ----
# After removing attention check failures verify conditions still balanced
# Note: using lowercase condition after converting to factor
# Expected roughly equal groups of ~100 per condition
table(data_clean$condition)

# ---- CONVERT COLUMNS TO NUMERIC ----
# Qualtrics exports all values as text/character format by default
# R cannot perform mathematical operations on text
# mutate(across()) applies the same transformation to multiple columns at once
# as.numeric converts each column from text to number format
# c() creates a list of all columns to convert
data_clean <- data_clean %>%
  mutate(across(c(perceptions_progress, perceptions_emotional, perceptions_ability,
                  policy_investment, policy_recidivism_measure, policy_support,
                  deservingness_society, deservingness_govt, deservingness_responsibility,
                  social_neighbor, social_coworker, social_family,
                  success_not_returning, success_housing, success_employment,
                  success_financial, success_family, success_health,
                  success_community, success_education,
                  age, gender, race, education, politics,
                  prior_contact, cj_interest), as.numeric))

# ---- REVERSE CODING ----
# Three items were negatively worded - agreeing = LESS supportive attitudes
# Must be flipped so higher scores consistently mean MORE supportive
# Formula: 6 - score (scale maximum is 5, so 5+1=6)
# _r suffix flags these as reverse coded - originals preserved
# Items reversed:
# deservingness_responsibility: rebuilding life is own responsibility
# policy_recidivism_measure: not returning to prison is a good measure of success
# policy_support: people from prison should not expect same govt support

# ---- CREATE COMPOSITE SCORES ----
# rowMeans() averages across columns for each participant row
# cbind() temporarily groups columns together for averaging
# na.rm = TRUE means ignore missing values when calculating mean
# Reverse coded versions (_r) used where applicable
# Four composite scores created - one per outcome subscale:
# score_perceptions: how positively they view Jordan (3 items)
# score_policy: how supportive of reintegration policies (3 items)
# score_deservingness: how much formerly incarcerated people deserve support (3 items)
# score_social: how comfortable around someone like Jordan (3 items)
data_clean <- data_clean %>%
  mutate(
    # Reverse coding (6 - score flips 1-5 scale direction)
    deservingness_responsibility_r = 6 - deservingness_responsibility,
    policy_recidivism_measure_r = 6 - policy_recidivism_measure,
    policy_support_r = 6 - policy_support,
    
    # Composite scores (average of relevant items per subscale)
    score_perceptions = rowMeans(cbind(perceptions_progress,
                                       perceptions_emotional,
                                       perceptions_ability), na.rm = TRUE),
    
    score_policy = rowMeans(cbind(policy_investment,
                                  policy_recidivism_measure_r,
                                  policy_support_r), na.rm = TRUE),
    
    score_deservingness = rowMeans(cbind(deservingness_society,
                                         deservingness_govt,
                                         deservingness_responsibility_r), na.rm = TRUE),
    
    score_social = rowMeans(cbind(social_neighbor,
                                  social_coworker,
                                  social_family), na.rm = TRUE)
  )

# ---- SANITY CHECK COMPOSITE SCORES ----
# summary() gives min, max, mean and quartiles for each composite score
# [, c(...)] selects only these four columns rather than all 67
# All values should fall between 1 and 5 (our scale range)
summary(data_clean[, c("score_perceptions", "score_policy",
                       "score_deservingness", "score_social")])

# ---- SAVE CLEANED DATASET ----
# Save data_clean as CSV so cleaning steps dont need re-running each session
# row.names = FALSE prevents R adding an extra row number column
# Next session just run: data_clean <- read.csv("data_clean.csv")
write.csv(data_clean, "data_clean.csv", row.names = FALSE)

# ================================================
# SECTION 2: DESCRIPTIVE STATISTICS
# ================================================

# ---- MEAN SCORES BY CONDITION ----
# group_by() splits data into groups A, B, C for separate calculations
# summarise() calculates summary statistics for each group
# mean() calculates average score within each condition
# Lower scores = more positive attitudes (1 = strongly agree)
# Key finding: B and C both lower than A on perceptions and social comfort
data_clean %>%
  group_by(condition) %>%
  summarise(
    mean_perceptions = mean(score_perceptions),
    mean_policy = mean(score_policy),
    mean_deservingness = mean(score_deservingness),
    mean_social = mean(score_social)
  )

# ---- SUCCESS IMPORTANCE RATINGS - OVERALL ----
# How did participants rate the importance of each success factor?
# Scale: 1 = Extremely important, 4 = Not at all important
# Lower scores = more important
data_clean %>%
  summarise(
    mean_not_returning = mean(success_not_returning, na.rm = TRUE),
    mean_housing = mean(success_housing, na.rm = TRUE),
    mean_employment = mean(success_employment, na.rm = TRUE),
    mean_financial = mean(success_financial, na.rm = TRUE),
    mean_family = mean(success_family, na.rm = TRUE),
    mean_health = mean(success_health, na.rm = TRUE),
    mean_community = mean(success_community, na.rm = TRUE),
    mean_education = mean(success_education, na.rm = TRUE)
  )

# ---- SUCCESS IMPORTANCE RATINGS BY CONDITION ----
# Does vignette framing influence how people define success?
# group_by condition then calculate means for each success factor
data_clean %>%
  group_by(condition) %>%
  summarise(
    mean_not_returning = mean(success_not_returning, na.rm = TRUE),
    mean_housing = mean(success_housing, na.rm = TRUE),
    mean_employment = mean(success_employment, na.rm = TRUE),
    mean_financial = mean(success_financial, na.rm = TRUE),
    mean_family = mean(success_family, na.rm = TRUE),
    mean_health = mean(success_health, na.rm = TRUE),
    mean_community = mean(success_community, na.rm = TRUE),
    mean_education = mean(success_education, na.rm = TRUE)
  )

# ---- PRIOR CONTACT WITH INCARCERATION ----
# Q15: Have you or someone close to you ever been to prison?
# 1 = Yes, 2 = No, 6 = Prefer not to say
# table() shows raw counts, prop.table() converts to percentages
table(data_clean$prior_contact)
prop.table(table(data_clean$prior_contact)) * 100

# Q19: Has this shaped your views? (only shown to those who said Yes to Q15)
# 1 = Yes, 2 = No
table(data_clean$contact_shaped_views)
prop.table(table(data_clean$contact_shaped_views)) * 100

# ================================================
# SECTION 3: ANOVA
# ================================================

# ---- ONE WAY ANOVA ----
# aov() runs Analysis of Variance
# ~ means predicted by - consistent formula syntax across all R statistical models
# Four separate ANOVAs - one per outcome variable
# summary() displays F statistic and p value
# Significant result (p < .05) means at least one condition differs from others
# Tukey post hoc needed to identify WHICH conditions differ
anova_perceptions <- aov(score_perceptions ~ condition, data = data_clean)
anova_policy <- aov(score_policy ~ condition, data = data_clean)
anova_deservingness <- aov(score_deservingness ~ condition, data = data_clean)
anova_social <- aov(score_social ~ condition, data = data_clean)

summary(anova_perceptions)   # F = 20.76, p < .001 *** SIGNIFICANT
summary(anova_policy)        # F = 0.871, p = .42 not significant
summary(anova_deservingness) # F = 0.104, p = .90 not significant
summary(anova_social)        # F = 4.355, p = .014 * SIGNIFICANT

# ---- ANOVA FOR SUCCESS IMPORTANCE RATINGS ----
# Testing whether condition significantly predicts importance ratings
# Expecting non-significant results given tiny differences across conditions
anova_not_returning <- aov(success_not_returning ~ condition, data = data_clean)
anova_housing <- aov(success_housing ~ condition, data = data_clean)
anova_employment <- aov(success_employment ~ condition, data = data_clean)
anova_financial <- aov(success_financial ~ condition, data = data_clean)
anova_family <- aov(success_family ~ condition, data = data_clean)
anova_health <- aov(success_health ~ condition, data = data_clean)
anova_community <- aov(success_community ~ condition, data = data_clean)
anova_education <- aov(success_education ~ condition, data = data_clean)

summary(anova_not_returning) # p = .448 not significant
summary(anova_housing)       # p = .449 not significant
summary(anova_employment)    # p = .584 not significant
summary(anova_financial)     # p = .610 not significant
summary(anova_family)        # p = .371 not significant
summary(anova_health)        # p = .465 not significant
summary(anova_community)     # p = .671 not significant
summary(anova_education)     # p = .145 not significant

# ---- TUKEY POST HOC TESTS ----
# TukeyHSD() identifies WHICH specific condition pairs differ significantly
# Only run after a significant ANOVA result
# Tests all three pairs: B vs A, C vs A, C vs B
# Controls for multiple comparisons to avoid false positives
# Key finding: B and C both significantly differ from A
# B and C do not significantly differ from each other (p = .99)
TukeyHSD(anova_perceptions)
TukeyHSD(anova_social)

# ================================================
# SECTION 4: REGRESSION
# ================================================

# ---- SET REFERENCE CATEGORY ----
# relevel() sets which condition is the baseline in regression
# ref = "A" sets recidivism-only condition as reference category
# All regression coefficients for B and C interpreted as compared to A
# Theoretically correct - A represents the status quo recidivism framing
data_clean$condition <- relevel(data_clean$condition, ref = "A")

# ---- BASIC REGRESSION MODELS ----
# lm() runs linear regression
# ~ means predicted by - same formula syntax as ANOVA
# With condition as factor R automatically creates dummy variables:
# conditionB = 1 if in B, 0 otherwise
# conditionC = 1 if in C, 0 otherwise
# Condition A is reference category shown as intercept
# Intercept = mean score for Condition A
# conditionB coefficient = difference between B and A
# conditionC coefficient = difference between C and A
reg_perceptions <- lm(score_perceptions ~ condition, data = data_clean)
reg_policy <- lm(score_policy ~ condition, data = data_clean)
reg_deservingness <- lm(score_deservingness ~ condition, data = data_clean)
reg_social <- lm(score_social ~ condition, data = data_clean)

summary(reg_perceptions)   # B: -0.47***, C: -0.48*** R2 = 0.12
summary(reg_policy)        # B: ns, C: ns
summary(reg_deservingness) # B: ns, C: ns
summary(reg_social)        # B: -0.34*, C: -0.37** R2 = 0.03

# ---- FULL REGRESSION MODELS WITH DEMOGRAPHIC COVARIATES ----
# Adding demographics after + to control for participant characteristics
# Only running for perceptions and social - the two significant outcomes
# Two key questions:
# 1. Do condition coefficients stay significant? = tests robustness of framing effect
# 2. Which demographics significantly predict the outcome?
# If condition coefficients barely change, framing effect is robust
# R-squared should increase as demographics explain additional variance
reg_perceptions_full <- lm(score_perceptions ~ condition + politics + age +
                             education + gender + race + prior_contact +
                             cj_interest, data = data_clean)

reg_social_full <- lm(score_social ~ condition + politics + age +
                        education + gender + race + prior_contact +
                        cj_interest, data = data_clean)

summary(reg_perceptions_full)
# conditionB: -0.48***, conditionC: -0.50*** - condition effect robust
# No demographic variables significant (all p > .05)
# R2 increased from 0.12 to 0.14

summary(reg_social_full)
# conditionB: -0.36**, conditionC: -0.31* - condition effect robust
# politics: 0.23*** (more conservative = less comfortable)
# cj_interest: 0.23** (less interested = less comfortable)
# R2 increased from 0.03 to 0.16

# ---- INTERACTION/MODERATION MODELS ----
# * includes both main effects AND their interaction in one formula
# Tests whether framing effect differs depending on political orientation
# Does holistic framing work differently for liberals vs conservatives?
# conditionB:politics and conditionC:politics are the interaction terms
# Non-significant interactions = framing effect consistent across politics
# Key finding: framing is not just preaching to the converted
reg_perceptions_int <- lm(score_perceptions ~ condition * politics,
                          data = data_clean)

reg_social_int <- lm(score_social ~ condition * politics,
                     data = data_clean)

summary(reg_perceptions_int)
# conditionB:politics p = .808, conditionC:politics p = .759 - not significant
# Framing effect on perceptions consistent regardless of political orientation

summary(reg_social_int)
# conditionB:politics p = .763, conditionC:politics p = .540 - not significant
# Framing effect on social comfort consistent regardless of political orientation

# ================================================
# SECTION 5: VISUALISATIONS
# ================================================

# ---- VISUALISATION 1: MEAN SCORES BY CONDITION ----
# Grouped bar chart showing all four outcomes by condition
# Most comprehensive single visual for dissertation
# Shows what framing changed (perceptions, social) and what it did not (policy, deservingness)

# Create summary dataframe for all four outcomes
plot_data_full <- data_clean %>%
  group_by(condition) %>%
  summarise(
    `Perceptions of Jordan ***` = mean(score_perceptions),
    `Policy attitudes` = mean(score_policy),
    `Deservingness` = mean(score_deservingness),
    `Social comfort *` = mean(score_social)
  ) %>%
  pivot_longer(cols = -condition,
               names_to = "outcome",
               values_to = "mean_score")

# Set order of outcomes for display
plot_data_full$outcome <- factor(plot_data_full$outcome,
                                 levels = c("Perceptions of Jordan ***",
                                            "Policy attitudes",
                                            "Deservingness",
                                            "Social comfort *"))

# Plot grouped bar chart
ggplot(plot_data_full, aes(x = outcome, y = mean_score, fill = condition)) +
  geom_bar(stat = "identity", position = "dodge", width = 0.65) +
  scale_fill_manual(values = c("A" = "#3266ad",
                               "B" = "#1D9E75",
                               "C" = "#D4537E"),
                    labels = c("A — recidivism only",
                               "B — holistic",
                               "C — combined")) +
  scale_y_continuous(limits = c(1, 3.5), oob = scales::squish) +
  labs(title = "Mean Scores by Vignette Condition",
       subtitle = "Lower scores = more positive attitudes (1 = strongly agree, 5 = strongly disagree)",
       x = "",
       y = "Mean score (lower = more positive)",
       fill = "") +
  theme_minimal() +
  theme(
    legend.position = "top",
    legend.text = element_text(size = 11),
    plot.title = element_text(size = 14, face = "bold"),
    plot.subtitle = element_text(size = 10, colour = "grey40"),
    axis.text.x = element_text(size = 11),
    axis.text.y = element_text(size = 10),
    panel.grid.major.x = element_blank()
  ) +
  labs(caption = "Note: lower scores = more positive attitudes. * p < .05, *** p < .001")

# Save plot 1
ggsave("plot1_condition_means_improved.png", width = 10, height = 7)

# ---- VISUALISATION 2: SUCCESS IMPORTANCE RANKING ----
# Horizontal bar chart showing mean importance ratings for each success factor
# Lower score = more important (1 = Extremely important, 4 = Not at all important)
# Key finding: employment and not returning to prison rated equally most important
# Community involvement rated least important - contrasts with interview findings

success_data <- data.frame(
  factor = c("Employment", "Not returning to prison", "Stable housing",
             "Physical/mental health", "Financial independence",
             "Family relationships", "Education", "Community involvement"),
  mean_score = c(1.16, 1.16, 1.24, 1.26, 1.40, 1.52, 1.82, 2.00)
)

# Reorder bars from most to least important
success_data$factor <- reorder(success_data$factor, success_data$mean_score)

# Plot horizontal bar chart with colour gradient
ggplot(success_data, aes(x = factor, y = mean_score, fill = mean_score)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  scale_fill_gradient(low = "#2ABFBF", high = "#E8735A") +
  labs(title = "Public Ratings of Success Factors After Prison",
       subtitle = "Lower scores = more important (1 = Extremely important, 4 = Not at all important)",
       x = "",
       y = "Mean Importance Score") +
  theme_minimal() +
  theme(legend.position = "none")

# Save plot 2
ggsave("plot2_success_importance.png", width = 8, height = 6)

# ---- VISUALISATION 3: REGRESSION COEFFICIENTS FOREST PLOT ----
# Shows effect of holistic and combined framing vs recidivism only
# with 95% confidence intervals
# Key features to note:
# All four dots are to the left of zero = all effects are negative = more positive attitudes
# None of the confidence intervals cross zero = all effects statistically significant
# Perceptions confidence intervals narrower than social comfort = more precisely estimated

# Extract coefficients from full regression models
perceptions_coef <- tidy(reg_perceptions_full, conf.int = TRUE) %>%
  filter(term %in% c("conditionB", "conditionC")) %>%
  mutate(outcome = "Perceptions of Jordan")

social_coef <- tidy(reg_social_full, conf.int = TRUE) %>%
  filter(term %in% c("conditionB", "conditionC")) %>%
  mutate(outcome = "Social Comfort")

# Combine both models into one dataframe
coef_data <- rbind(perceptions_coef, social_coef) %>%
  mutate(term = recode(term,
                       "conditionB" = "Holistic vs Recidivism (B vs A)",
                       "conditionC" = "Combined vs Recidivism (C vs A)"))

# Plot forest plot with facets for each outcome
ggplot(coef_data, aes(x = term, y = estimate, colour = outcome, shape = outcome)) +
  geom_point(size = 5) +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.15, size = 1) +
  geom_hline(yintercept = 0, linetype = "dashed", colour = "grey40", size = 0.8) +
  coord_flip() +
  facet_wrap(~ outcome, ncol = 1) +
  scale_colour_manual(values = c("Perceptions of Jordan" = "#E8735A",
                                 "Social Comfort" = "#2ABFBF")) +
  labs(title = "Effect of Holistic Framing Compared to Recidivism-Only Framing",
       subtitle = "Negative coefficients = more positive attitudes than Condition A\nDashed line = no effect",
       x = "",
       y = "Regression coefficient (vs Condition A)") +
  theme_minimal() +
  theme(legend.position = "none",
        strip.text = element_text(size = 12, face = "bold"),
        plot.title = element_text(size = 13),
        plot.subtitle = element_text(size = 10, colour = "grey40"),
        axis.text = element_text(size = 11))

# Save plot 3
ggsave("plot3_regression_coefficients.png", width = 9, height = 7)

# ---- VISUALISATION 4: PRIOR CONTACT DISTRIBUTION ----
# Combined chart showing prior contact with incarceration
# and whether it shaped views (among those with prior contact)
# Key finding: 32.7% had prior contact, of those 71.4% said it shaped their views
# Despite this, prior contact was not significant in regression models

# Create prior contact summary dataframe
contact_data <- data_clean %>%
  mutate(prior_contact_label = recode(prior_contact,
                                      `1` = "Yes",
                                      `2` = "No",
                                      `6` = "Prefer not to say")) %>%
  filter(!is.na(prior_contact_label)) %>%
  count(prior_contact_label) %>%
  mutate(percentage = n / sum(n) * 100)

# Create shaped views summary dataframe (among those with prior contact only)
shaped_data <- data_clean %>%
  filter(prior_contact == 1) %>%
  mutate(shaped_label = recode(contact_shaped_views,
                               `1` = "Yes, shaped my views",
                               `2` = "No, did not shape views")) %>%
  filter(!is.na(shaped_label)) %>%
  count(shaped_label) %>%
  mutate(percentage = n / sum(n) * 100,
         group = "Of those with prior contact")

# Plot 4a - prior contact distribution
plot4a <- ggplot(contact_data, aes(x = reorder(prior_contact_label, -percentage),
                                   y = percentage, fill = prior_contact_label)) +
  geom_bar(stat = "identity", width = 0.5) +
  geom_text(aes(label = paste0(round(percentage, 1), "%")), vjust = -0.5, size = 4) +
  scale_fill_manual(values = c("Yes" = "#E8735A", "No" = "#2ABFBF",
                               "Prefer not to say" = "#B0B0B0")) +
  labs(title = "Prior contact with incarceration",
       x = "", y = "Percentage (%)") +
  theme_minimal() +
  theme(legend.position = "none")

# Plot 4b - shaped views among those with prior contact
plot4b <- ggplot(shaped_data, aes(x = reorder(shaped_label, -percentage),
                                  y = percentage, fill = shaped_label)) +
  geom_bar(stat = "identity", width = 0.5) +
  geom_text(aes(label = paste0(round(percentage, 1), "%")), vjust = -0.5, size = 4) +
  scale_fill_manual(values = c("Yes, shaped my views" = "#E8735A",
                               "No, did not shape views" = "#2ABFBF")) +
  labs(title = "Did it shape your views?",
       subtitle = "(Among those with prior contact)",
       x = "", y = "Percentage (%)") +
  theme_minimal() +
  theme(legend.position = "none")

# Arrange both plots side by side
grid.arrange(plot4a, plot4b, ncol = 2)

# Save plot 4
ggsave("plot4_prior_contact.png", width = 10, height = 6)

install.packages("rmarkdown")
install.packages("knitr")

