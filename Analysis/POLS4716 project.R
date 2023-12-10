# Spardha Sharma

# importing necessary libraries
library(readr)
library(ggplot2)
library(dplyr) # make sure tidyverse is detached because the 2 packages interact with each other
library(corrplot)
library(plotly)
library(ggsci)


# reading all files
#combined_df <- read_csv("/Users/Spardha/Downloads/combined_df.csv")
voter_df <- read_csv("/Users/Spardha/Downloads/voter_turnout.csv")
anes_df <- read_csv("/Users/Spardha/Downloads/anes_timeseries_cdf_csv_20220916.csv")
abortion_df <- read_csv("/Users/Spardha/Downloads/pregnancy.csv")

# changing variable names
anes_df$when_abort  = anes_df$VCF0837
anes_df$abort_law   = anes_df$VCF0838
anes_df$anti_abort_therm  = anes_df$VCF0230
anes_df$YEAR = anes_df$VCF0004
anes_df$STATE_ABV = anes_df$VCF0901b
anes_df$age = anes_df$VCF0101
anes_df$gender = anes_df$VCF0104
anes_df$religion = anes_df$VCF0128
anes_df$parent = anes_df$VCF0139
anes_df$race= anes_df$VCF0105b

abortion_df$STATE_ABV= abortion_df$state
abortion_df$YEAR= abortion_df$year

# keeping only total abortion data per state per year
abortion_df <- abortion_df%>%
  select(YEAR, STATE_ABV, abortionratetotal, pregnancyratetotal)

# data wrangling
combined_df <- anes_df%>%
  select(anti_abort_therm, YEAR, STATE_ABV)

combined_df <- inner_join(combined_df, voter_df, by = c("STATE_ABV", "YEAR"))

combined_df <- left_join(combined_df, abortion_df, by=c("STATE_ABV", "YEAR"))

#PART 1: RELATION BETWEEN A RESPONDENT'S ANTI ABORTION METER & VOTER TURNOUT RATE
# State 1: IOWA
iowa_df <- combined_df%>%
  filter(STATE_ABV=='IA')

# converting voter turnout rate to a numeric data type
iowa_df$VEP_TURNOUT_RATE <- as.numeric(sub("%", "", iowa_df$VEP_TURNOUT_RATE))

# Subsetting into category 1: Pro-abortionists
subset1_iowa_df <- iowa_df %>% filter(anti_abort_therm < 25)
# making a correlation matrix
cor_matrix <- cor(subset1_iowa_df[, c("anti_abort_therm", "VEP_TURNOUT_RATE")], use = "complete.obs")
# plotting the correlation matrix
colnames(cor_matrix) <- c("Voter Turnout Rate", "Anti Abortion Level")
rownames(cor_matrix) <-  c("Voter Turnout Rate", "Anti Abortion Level")
corrplot(cor_matrix, method = "color", addCoef.col = "black", tl.col = "black", tl.srt = 0, 
         main = list(
           "Correlation Between Voter Turnout and Anti-Abortion Level Among Iowa's Abortionists"
         ), mar=c(1,1,1,1))

# Subsetting into category 2: Moderate abortionists
subset2_iowa_df <- iowa_df %>% filter(anti_abort_therm %in% c(25:50))
cor_matrix <- cor(subset2_iowa_df[, c("anti_abort_therm", "VEP_TURNOUT_RATE")], use = "complete.obs")
colnames(cor_matrix) <- c("Voter Turnout Rate", "Anti Abortion Level")
rownames(cor_matrix) <-  c("Voter Turnout Rate", "Anti Abortion Level")
corrplot(cor_matrix, method = "color", addCoef.col = "black", tl.col = "black", tl.srt = 0, 
         main = list(
           "Correlation Between Voter Turnout and Anti-Abortion Level Among Iowa's Moderate Abortionists"
         ), mar=c(1,1,1,1))

# Subsetting into category 3: Moderate anti-abortionists
subset3_iowa_df <- iowa_df %>% filter(anti_abort_therm %in% (50:97))
cor_matrix <- cor(subset3_iowa_df[, c("anti_abort_therm", "VEP_TURNOUT_RATE")], use = "complete.obs")
colnames(cor_matrix) <- c("Voter Turnout Rate", "Anti Abortion Level")
rownames(cor_matrix) <-  c("Voter Turnout Rate", "Anti Abortion Level")
corrplot(cor_matrix, method = "color", addCoef.col = "black", tl.col = "black", tl.srt = 0, 
         main = list(
           "Correlation Between Voter Turnout and Anti-Abortion Level Among Iowa's Moderate Anti-Abortionists"
         ), mar=c(1,1,1,1))

# Subsetting into category 4: Anti-abortionists
subset4_iowa_df <- iowa_df %>% filter(anti_abort_therm >= 97)
cor_matrix <- cor(subset4_iowa_df[, c("anti_abort_therm", "VEP_TURNOUT_RATE")], use = "complete.obs")
colnames(cor_matrix) <- c("Voter Turnout Rate", "Anti Abortion Level")
rownames(cor_matrix) <-  c("Voter Turnout Rate", "Anti Abortion Level")
corrplot(cor_matrix, method = "color", addCoef.col = "black", tl.col = "black", tl.srt = 0, 
         main = list(
           "Correlation Between Voter Turnout and Anti-Abortion Level Among Iowa's Anti-Abortionists"
         ), mar=c(1,1,1,1))


# State 2: INDIANA
indiana_df <- combined_df%>%
  filter(STATE_ABV=='IN')

# converting voter turnout rate to a numeric data type
indiana_df$VEP_TURNOUT_RATE <- as.numeric(sub("%", "", indiana_df$VEP_TURNOUT_RATE))

# Subsetting into category 1: Pro-abortionists
subset1_indiana_df <- indiana_df %>% filter(anti_abort_therm < 25)
# making a correlation matrix
cor_matrix <- cor(subset1_indiana_df[, c("anti_abort_therm", "VEP_TURNOUT_RATE")], use = "complete.obs")
# plotting the correlation matrix
colnames(cor_matrix) <- c("Voter Turnout Rate", "Anti Abortion Level")
rownames(cor_matrix) <-  c("Voter Turnout Rate", "Anti Abortion Level")
corrplot(cor_matrix, method = "color", addCoef.col = "black", tl.col = "black", tl.srt = 0, 
         main = list(
           "Correlation Between Voter Turnout and Anti-Abortion Level Among Indiana's Abortionists"
         ), mar=c(1,1,1,1))

# Subsetting into category 2: Moderate abortionists
subset2_indiana_df <- indiana_df %>% filter(anti_abort_therm %in% c(25:50))
cor_matrix <- cor(subset2_indiana_df[, c("anti_abort_therm", "VEP_TURNOUT_RATE")], use = "complete.obs")
colnames(cor_matrix) <- c("Voter Turnout Rate", "Anti Abortion Level")
rownames(cor_matrix) <-  c("Voter Turnout Rate", "Anti Abortion Level")
corrplot(cor_matrix, method = "color", addCoef.col = "black", tl.col = "black", tl.srt = 0, 
         main = list(
           "Correlation Between Voter Turnout and Anti-Abortion Level Among Indiana's Moderate Abortionists"
         ), mar=c(1,1,1,1))

# Subsetting into category 3: Moderate anti-abortionists
subset3_indiana_df <- indiana_df %>% filter(anti_abort_therm %in% (50:97))
cor_matrix <- cor(subset3_indiana_df[, c("anti_abort_therm", "VEP_TURNOUT_RATE")], use = "complete.obs")
colnames(cor_matrix) <- c("Voter Turnout Rate", "Anti Abortion Level")
rownames(cor_matrix) <-  c("Voter Turnout Rate", "Anti Abortion Level")
corrplot(cor_matrix, method = "color", addCoef.col = "black", tl.col = "black", tl.srt = 0, 
         main = list(
           "Correlation Between Voter Turnout and Anti-Abortion Level Among Indiana's Moderate Anti-Abortionists"
         ), mar=c(1,1,1,1))

# Subsetting into category 4: Anti-abortionists
subset4_indiana_df <- indiana_df %>% filter(anti_abort_therm >= 97)
cor_matrix <- cor(subset4_indiana_df[, c("anti_abort_therm", "VEP_TURNOUT_RATE")], use = "complete.obs")
colnames(cor_matrix) <- c("Voter Turnout Rate", "Anti Abortion Level")
rownames(cor_matrix) <-  c("Voter Turnout Rate", "Anti Abortion Level")
corrplot(cor_matrix, method = "color", addCoef.col = "black", tl.col = "black", tl.srt = 0, 
         main = list(
           "Correlation Between Voter Turnout and Anti-Abortion Level Among Indiana's Anti-Abortionists"
         ), mar=c(1,1,1,1))


# State 3: NEW YORK
ny_df <- combined_df%>%
  filter(STATE_ABV=='NY')

# converting voter turnout rate to a numeric data type
ny_df$VEP_TURNOUT_RATE <- as.numeric(sub("%", "", ny_df$VEP_TURNOUT_RATE))

# Subsetting into category 1: Pro-abortionists
subset1_ny_df <- ny_df %>% filter(anti_abort_therm < 25)
# making a correlation matrix
cor_matrix <- cor(subset1_ny_df[, c("anti_abort_therm", "VEP_TURNOUT_RATE")], use = "complete.obs")
# plotting the correlation matrix
colnames(cor_matrix) <- c("Voter Turnout Rate", "Anti Abortion Level")
rownames(cor_matrix) <-  c("Voter Turnout Rate", "Anti Abortion Level")
corrplot(cor_matrix, method = "color", addCoef.col = "black", tl.col = "black", tl.srt = 0, 
         main = list(
           "Correlation Between Voter Turnout and Anti-Abortion Level Among New York's Abortionists"
         ), mar=c(1,1,1,1))

# Subsetting into category 2: Moderate abortionists
subset2_ny_df <- ny_df %>% filter(anti_abort_therm %in% c(25:50))
cor_matrix <- cor(subset2_ny_df[, c("anti_abort_therm", "VEP_TURNOUT_RATE")], use = "complete.obs")
colnames(cor_matrix) <- c("Voter Turnout Rate", "Anti Abortion Level")
rownames(cor_matrix) <-  c("Voter Turnout Rate", "Anti Abortion Level")
corrplot(cor_matrix, method = "color", addCoef.col = "black", tl.col = "black", tl.srt = 0, 
         main = list(
           "Correlation Between Voter Turnout and Anti-Abortion Level Among New York's Moderate Abortionists"
         ), mar=c(1,1,1,1))

# Subsetting into category 3: Moderate anti-abortionists
subset3_ny_df <- ny_df %>% filter(anti_abort_therm %in% (50:97))
cor_matrix <- cor(subset3_ny_df[, c("anti_abort_therm", "VEP_TURNOUT_RATE")], use = "complete.obs")
colnames(cor_matrix) <- c("Voter Turnout Rate", "Anti Abortion Level")
rownames(cor_matrix) <-  c("Voter Turnout Rate", "Anti Abortion Level")
corrplot(cor_matrix, method = "color", addCoef.col = "black", tl.col = "black", tl.srt = 0, 
         main = list(
           "Correlation Between Voter Turnout and Anti-Abortion Level Among New York's Moderate Anti-Abortionists"
         ), mar=c(1,1,1,1))

# Subsetting into category 4: Anti-abortionists
subset4_ny_df <- ny_df %>% filter(anti_abort_therm >= 97)
cor_matrix <- cor(subset4_ny_df[, c("anti_abort_therm", "VEP_TURNOUT_RATE")], use = "complete.obs")
colnames(cor_matrix) <- c("Voter Turnout Rate", "Anti Abortion Level")
rownames(cor_matrix) <-  c("Voter Turnout Rate", "Anti Abortion Level")
corrplot(cor_matrix, method = "color", addCoef.col = "black", tl.col = "black", tl.srt = 0, 
         main = list(
           "Correlation Between Voter Turnout and Anti-Abortion Level Among New York's Anti-Abortionists"
         ), mar=c(1,1,1,1))


# Note that all other states of interest do not have sufficient data available

# State 4: FLORIDA
florida_df <- combined_df%>%
  filter(STATE_ABV=='FL')

# converting voter turnout rate to a numeric data type
florida_df$VEP_TURNOUT_RATE <- as.numeric(sub("%", "", florida_df$VEP_TURNOUT_RATE))

# We are examining the relation between VEP (Voting Eligible Population) turnout and pregnancy rate.
# This choice is motivated by the fact that abortion regulations often reflect the state's legislative 
# decisions, and individuals may not have a direct influence on these policies. 
# On the other hand, pregnancy rates are influenced by various factors, and experiencing 
# an unwanted pregnancy could potentially impact an individual's decision to participate in voting. 
# For instance, a woman in a state where abortion is illegal might be more inclined to vote for 
# change after facing an unwanted pregnancy. This nuance is crucial as it allows us to 
# explore connections between civic engagement and personal experiences that may be more 
# directly related to voting behaviors.

# limitation of this approach: we only have pregnancy rate data till 2017

# linear regression model
summary(lm(VEP_TURNOUT_RATE ~ pregnancyratetotal, data = florida_df))

ggplot(florida_df, aes(x = pregnancyratetotal, y = VEP_TURNOUT_RATE)) +
  geom_point() +
  geom_abline(intercept = coef(lm(VEP_TURNOUT_RATE ~ pregnancyratetotal, data = florida_df))[1],
              slope = coef(lm(VEP_TURNOUT_RATE ~ pregnancyratetotal, data = florida_df))[2],
              color = "blue") +
  labs(
    title = "Effect of Pregnancy Rate on VEP Turnout Rate in Florida",
    x = "Pregnancy Rate (% Among 1000 women)",
    y = "VEP Turnout Rate (%)"
  )

# State 5: GEORGIA
georgia_df <- combined_df%>%
  filter(STATE_ABV=='GA')

# converting voter turnout rate to a numeric data type
georgia_df$VEP_TURNOUT_RATE <- as.numeric(sub("%", "", georgia_df$VEP_TURNOUT_RATE))

# limitation of this approach: we only have pregnancy rate data till 2017

# linear regression model
summary(lm(VEP_TURNOUT_RATE ~ pregnancyratetotal, data = georgia_df))

ggplot(georgia_df, aes(x = pregnancyratetotal, y = VEP_TURNOUT_RATE)) +
  geom_point() +
  geom_abline(intercept = coef(lm(VEP_TURNOUT_RATE ~ pregnancyratetotal, data = florida_df))[1],
              slope = coef(lm(VEP_TURNOUT_RATE ~ pregnancyratetotal, data = florida_df))[2],
              color = "blue") +
  labs(
    title = "Effect of Pregnancy Rate on VEP Turnout Rate in Florida",
    x = "Pregnancy Rate (% Among 1000 women)",
    y = "VEP Turnout Rate (%)"
  )



# PART 2: EFFECT OF AGE, RELIGION, GENDER, RACE ON VOTER TURNOUT RATE
# data wrangling
combined_df <- anes_df%>%
  select(anti_abort_therm, abort_law, age,religion, gender,race, YEAR, STATE_ABV)

combined_df <- inner_join(combined_df, voter_df, by = c("STATE_ABV", "YEAR"))

combined_df <- left_join(combined_df, abortion_df, by=c("STATE_ABV", "YEAR"))

# Religion labels
religion_mapping <- c("NA", "Christian", "Catholic", "Jewish", "Other")
df <- combined_df %>%
  mutate(religion_category = factor(religion, labels = religion_mapping))

# Age categories
df$age_category <- ifelse(is.na(df$age) | df$age < 18, "NA",
                          ifelse(df$age < 25, "Under 25",
                                 ifelse(df$age < 30, "25-29",
                                        ifelse(df$age < 50, "30-49",
                                               ifelse(df$age < 65, "50-64", "65 and above")))))

# Gender
df$gender <- ifelse(is.na(df$gender), "NA",
                    ifelse(df$gender == 1, "Male",
                           ifelse(df$gender == 2, "Female",
                                  ifelse(df$gender == 3, "Other", "Unknown"))))

# Race
df$race_category <- ifelse(is.na(df$race) | df$race==9 | df$race==0, "NA",
                          ifelse(df$race ==1, "White non-hispanic",
                                 ifelse(df$race == 2, "Black non-hispanic",
                                        ifelse(df$race == 3, "Hispanic",
                                               ifelse(df$race == 4, "Other", "Uncategorized")))))

# converting turnout rate to numeric
df$VEP_TURNOUT_RATE <- as.numeric(sub("%", "", df$VEP_TURNOUT_RATE))

# IMPACT OF RELIGION ON VOTER TURNOUT RATE BY YEAR (IOWA)
religion_df<- df %>%
  filter((STATE_ABV=="IA") , (religion_category %in% c("Christian", "Catholic", "Other")))%>%
  group_by(YEAR, religion_category) %>%
  summarise(n = n()) %>%
  mutate(prop = n / sum(n))

ggplot(religion_df, aes(x = religion_category, y = prop, fill=religion_category)) +
  geom_bar(stat = "identity") +
  facet_wrap(YEAR ~ ., ncol = 4, scales = "free_y") +
  labs(
    title = "VEP Turnout Rate by Religion and Year in Iowa",
    x = "Religion",
    y = "VEP Turnout Rate"
  ) +
  ylab("VEP Turnout Rate")+
  theme_minimal()

# IMPACT OF AGE ON VOTER TURNOUT RATE BY YEAR (IOWA)
age_df <- df %>%
  filter((STATE_ABV == "IA"), (age_category %in% c("Under 25", "25-29", "30-49","50-64", "65 and above"))) %>%
  group_by(YEAR, age_category) %>%
  summarise(n = n()) %>%
  mutate(prop = n / sum(n))

age_df <- age_df %>%
  mutate(age_category = factor(age_category, levels = c("Under 25", "25-29", "30-49", "50-64", "65 and above")))

ggplot(age_df, aes(x = age_category, y = prop, fill=age_category)) +
  geom_bar(stat = "identity") +
  facet_wrap(YEAR ~ ., ncol = 4, scales = "free_y") +
  labs(
    title = "VEP Turnout Rate by Age and Year in Iowa",
    x = "Religion",
    y = "VEP Turnout Rate"
  ) +
  ylab("VEP Turnout Rate")+
  theme_minimal()

# IMPACT OF GENDER ON VOTER TURNOUT RATE BY YEAR (IOWA)
gender_df <- df %>%
  filter((STATE_ABV == "IA"), (gender %in% c("Male", "Female", "Other"))) %>%
  group_by(YEAR, gender) %>%
  summarise(n = n()) %>%
  mutate(prop = n / sum(n))

ggplot(gender_df, aes(x = gender, y = prop, fill=gender)) +
  geom_bar(stat = "identity") +
  facet_wrap(YEAR ~ ., ncol = 4, scales = "free_y") +
  labs(
    title = "VEP Turnout Rate by Gender and Year in Iowa",
    x = "Religion",
    y = "VEP Turnout Rate"
  ) +
  ylab("VEP Turnout Rate")+
  theme_minimal()

# IMPACT OF RACE ON VOTER TURNOUT RATE BY YEAR (IOWA)
race_df <- df %>%
  filter((STATE_ABV == "IA"), (race_category %in% c("White non-hispanic", "Black non-hispanic", "Hispanic", "Other"))) %>%
  group_by(YEAR, race_category) %>%
  summarise(n = n()) %>%
  mutate(prop = n / sum(n))

race_df <- race_df %>%
  mutate(race_category = factor(race_category, levels = c("White non-hispanic", "Black non-hispanic", "Hispanic", "Other", "NA", "Uncategorized")))

ggplot(race_df, aes(x = race_category, y = prop, fill=race_category)) +
  geom_bar(stat = "identity") +
  facet_wrap(YEAR ~ ., ncol = 4, scales = "free_y") +
  labs(
    title = "VEP Turnout Rate by Race and Year in Iowa",
    x = "Religion",
    y = "VEP Turnout Rate"
  ) +
  ylab("VEP Turnout Rate")+
  theme_minimal()+theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))


# IMPACT OF RELIGION ON VOTER TURNOUT RATE BY YEAR (INDIANA)
religion_df<- df %>%
  filter((STATE_ABV=="IN"),(religion_category %in% c("Christian","Catholic", "Jweish", "Other")))%>%
  group_by(YEAR, religion_category) %>%
  summarise(n = n()) %>%
  mutate(prop = n / sum(n))

ggplot(religion_df, aes(x = religion_category, y = prop, fill=religion_category)) +
  geom_bar(stat = "identity") +
  facet_wrap(YEAR ~ ., ncol = 4, scales = "free_y") +
  labs(
    title = "VEP Turnout Rate by Religion and Year in Indiana",
    x = "Religion",
    y = "VEP Turnout Rate"
  ) +
  ylab("VEP Turnout Rate")+
  theme_minimal()

# IMPACT OF AGE ON VOTER TURNOUT RATE BY YEAR (INDIANA)
age_df <- df %>%
  filter((STATE_ABV == "IN"), (age_category %in% c("Under 25", "25-29", "30-49","50-64", "65 and above"))) %>%
  group_by(YEAR, age_category) %>%
  summarise(n = n()) %>%
  mutate(prop = n / sum(n))

age_df <- age_df %>%
  mutate(age_category = factor(age_category, levels = c("Under 25", "25-29", "30-49", "50-64", "65 and above")))

ggplot(age_df, aes(x = age_category, y = prop, fill=age_category)) +
  geom_bar(stat = "identity") +
  facet_wrap(YEAR ~ ., ncol = 4, scales = "free_y") +
  labs(
    title = "VEP Turnout Rate by Age and Year in Indiana",
    x = "Religion",
    y = "VEP Turnout Rate"
  ) +
  ylab("VEP Turnout Rate")+
  theme_minimal()

# IMPACT OF GENDER ON VOTER TURNOUT RATE BY YEAR (INDIANA)
gender_df <- df %>%
  filter((STATE_ABV == "IN"), (gender %in% c("Male", "Female", "Other"))) %>%
  group_by(YEAR, gender) %>%
  summarise(n = n()) %>%
  mutate(prop = n / sum(n))

ggplot(gender_df, aes(x = gender, y = prop, fill=gender)) +
  geom_bar(stat = "identity") +
  facet_wrap(YEAR ~ ., ncol = 4, scales = "free_y") +
  labs(
    title = "VEP Turnout Rate by Gender and Year in Indiana",
    x = "Religion",
    y = "VEP Turnout Rate"
  ) +
  ylab("VEP Turnout Rate")+
  theme_minimal()

# IMPACT OF RACE ON VOTER TURNOUT RATE BY YEAR (INDIANA)
race_df <- df %>%
  filter((STATE_ABV == "IN"), (race_category %in% c("White non-hispanic", "Black non-hispanic", "Hispanic", "Other"))) %>%
  group_by(YEAR, race_category) %>%
  summarise(n = n()) %>%
  mutate(prop = n / sum(n))

race_df <- race_df %>%
  mutate(race_category = factor(race_category, levels = c("White non-hispanic", "Black non-hispanic", "Hispanic", "Other", "NA", "Uncategorized")))

ggplot(race_df, aes(x = race_category, y = prop, fill=race_category)) +
  geom_bar(stat = "identity") +
  facet_wrap(YEAR ~ ., ncol = 4, scales = "free_y") +
  labs(
    title = "VEP Turnout Rate by Race and Year in Indiana",
    x = "Religion",
    y = "VEP Turnout Rate"
  ) +
  ylab("VEP Turnout Rate")+
  theme_minimal()+theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))


# IMPACT OF RELIGION ON VOTER TURNOUT RATE BY YEAR (NEW YORK)
religion_df<- df %>%
  filter((STATE_ABV=="NY"), (religion_category %in% c("Christian", "Catholic", "Jewish", "Other")))%>%
  group_by(YEAR, religion_category) %>%
  summarise(n = n()) %>%
  mutate(prop = n / sum(n))

ggplot(religion_df, aes(x = religion_category, y = prop, fill=religion_category)) +
  geom_bar(stat = "identity") +
  facet_wrap(YEAR ~ ., ncol = 4, scales = "free_y") +
  labs(
    title = "VEP Turnout Rate by Religion and Year in New York",
    x = "Religion",
    y = "VEP Turnout Rate"
  ) +
  ylab("VEP Turnout Rate")+
  theme_minimal()

# IMPACT OF AGE ON VOTER TURNOUT RATE BY YEAR (NEW YORK)
age_df <- df %>%
  filter((STATE_ABV == "NY"), (age_category %in% c("Under 25", "25-29", "30-49","50-64", "65 and above"))) %>%
  group_by(YEAR, age_category) %>%
  summarise(n = n()) %>%
  mutate(prop = n / sum(n))

age_df <- age_df %>%
  mutate(age_category = factor(age_category, levels = c("Under 25", "25-29", "30-49", "50-64", "65 and above")))

ggplot(age_df, aes(x = age_category, y = prop, fill=age_category)) +
  geom_bar(stat = "identity") +
  facet_wrap(YEAR ~ ., ncol = 4, scales = "free_y") +
  labs(
    title = "VEP Turnout Rate by Age and Year in New York",
    x = "Religion",
    y = "VEP Turnout Rate"
  ) +
  ylab("VEP Turnout Rate")+
  theme_minimal()

# IMPACT OF GENDER ON VOTER TURNOUT RATE BY YEAR (NEW YORK)
gender_df <- df %>%
  filter((STATE_ABV == "NY"), (gender %in% c("Male", "Female", "Other"))) %>%
  group_by(YEAR, gender) %>%
  summarise(n = n()) %>%
  mutate(prop = n / sum(n))

ggplot(gender_df, aes(x = gender, y = prop, fill=gender)) +
  geom_bar(stat = "identity") +
  facet_wrap(YEAR ~ ., ncol = 4, scales = "free_y") +
  labs(
    title = "VEP Turnout Rate by Gender and Year in New York",
    x = "Religion",
    y = "VEP Turnout Rate"
  ) +
  ylab("VEP Turnout Rate")+
  theme_minimal()

# IMPACT OF RACE ON VOTER TURNOUT RATE BY YEAR (NEW YORK)
race_df <- df %>%
  filter((STATE_ABV == "NY"), (race_category %in% c("White non-hispanic", "Black non-hispanic", "Hispanic", "Other"))) %>%
  group_by(YEAR, race_category) %>%
  summarise(n = n()) %>%
  mutate(prop = n / sum(n))

race_df <- race_df %>%
  mutate(race_category = factor(race_category, levels = c("White non-hispanic", "Black non-hispanic", "Hispanic", "Other", "NA", "Uncategorized")))

ggplot(race_df, aes(x = race_category, y = prop, fill=race_category)) +
  geom_bar(stat = "identity") +
  facet_wrap(YEAR ~ ., ncol = 4, scales = "free_y") +
  labs(
    title = "VEP Turnout Rate by Race and Year in New york",
    x = "Religion",
    y = "VEP Turnout Rate"
  ) +
  ylab("VEP Turnout Rate")+
  theme_minimal()+theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))


# IMPACT OF RELIGION ON VOTER TURNOUT RATE BY YEAR (FLORIDA)
religion_df<- df %>%
  filter((STATE_ABV=="FL"), (religion_category %in% c("Christian", "Jewish", "Catholic", "Other")))%>%
  group_by(YEAR, religion_category) %>%
  summarise(n = n()) %>%
  mutate(prop = n / sum(n))

ggplot(religion_df, aes(x = religion_category, y = prop, fill=religion_category)) +
  geom_bar(stat = "identity") +
  facet_wrap(YEAR ~ ., ncol = 4, scales = "free_y") +
  labs(
    title = "VEP Turnout Rate by Religion and Year in Florida",
    x = "Religion",
    y = "VEP Turnout Rate"
  ) +
  ylab("VEP Turnout Rate")+
  theme_minimal()

# IMPACT OF AGE ON VOTER TURNOUT RATE BY YEAR (FLORIDA)
age_df <- df %>%
  filter((STATE_ABV == "FL"), (age_category %in% c("Under 25", "25-29", "30-49","50-64", "65 and above"))) %>%
  group_by(YEAR, age_category) %>%
  summarise(n = n()) %>%
  mutate(prop = n / sum(n))

age_df <- age_df %>%
  mutate(age_category = factor(age_category, levels = c("Under 25", "25-29", "30-49", "50-64", "65 and above")))

ggplot(age_df, aes(x = age_category, y = prop, fill=age_category)) +
  geom_bar(stat = "identity") +
  facet_wrap(YEAR ~ ., ncol = 4, scales = "free_y") +
  labs(
    title = "VEP Turnout Rate by Age and Year in Florida",
    x = "Religion",
    y = "VEP Turnout Rate"
  ) +
  ylab("VEP Turnout Rate")+
  theme_minimal()

# IMPACT OF GENDER ON VOTER TURNOUT RATE BY YEAR (FLORIDA)
gender_df <- df %>%
  filter((STATE_ABV == "FL"), (gender %in% c("Male", "Female", "Other"))) %>%
  group_by(YEAR, gender) %>%
  summarise(n = n()) %>%
  mutate(prop = n / sum(n))

ggplot(gender_df, aes(x = gender, y = prop, fill=gender)) +
  geom_bar(stat = "identity") +
  facet_wrap(YEAR ~ ., ncol = 4, scales = "free_y") +
  labs(
    title = "VEP Turnout Rate by Gender and Year in Florida",
    x = "Religion",
    y = "VEP Turnout Rate"
  ) +
  ylab("VEP Turnout Rate")+
  theme_minimal()

# IMPACT OF RACE ON VOTER TURNOUT RATE BY YEAR (FLORIDA)
race_df <- df %>%
  filter((STATE_ABV == "FL"), (race_category %in% c("White non-hispanic", "Black non-hispanic", "Hispanic", "Other"))) %>%
  group_by(YEAR, race_category) %>%
  summarise(n = n()) %>%
  mutate(prop = n / sum(n))

race_df <- race_df %>%
  mutate(race_category = factor(race_category, levels = c("White non-hispanic", "Black non-hispanic", "Hispanic", "Other", "NA", "Uncategorized")))

ggplot(race_df, aes(x = race_category, y = prop, fill=race_category)) +
  geom_bar(stat = "identity") +
  facet_wrap(YEAR ~ ., ncol = 4, scales = "free_y") +
  labs(
    title = "VEP Turnout Rate by Race and Year in Florida",
    x = "Religion",
    y = "VEP Turnout Rate"
  ) +
  ylab("VEP Turnout Rate")+
  theme_minimal()+theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))


# IMPACT OF RELIGION ON VOTER TURNOUT RATE BY YEAR (GEORGIA)
religion_df<- df %>%
  filter((STATE_ABV=="GA"), (religion_catgory %in% c("Christian", "Jewish" ,"Catholic", "Other")))%>%
  group_by(YEAR, religion_category) %>%
  summarise(n = n()) %>%
  mutate(prop = n / sum(n))

ggplot(religion_df, aes(x = religion_category, y = prop, fill=religion_category)) +
  geom_bar(stat = "identity") +
  facet_wrap(YEAR ~ ., ncol = 4, scales = "free_y") +
  labs(
    title = "VEP Turnout Rate by Religion and Year in Georgia",
    x = "Religion",
    y = "VEP Turnout Rate"
  ) +
  ylab("VEP Turnout Rate")+
  theme_minimal()

# IMPACT OF AGE ON VOTER TURNOUT RATE BY YEAR (GEORGIA)
age_df <- df %>%
  filter((STATE_ABV == "GA"), (age_category %in% c("Under 25", "25-29", "30-49","50-64", "65 and above"))) %>%
  group_by(YEAR, age_category) %>%
  summarise(n = n()) %>%
  mutate(prop = n / sum(n))

age_df <- age_df %>%
  mutate(age_category = factor(age_category, levels = c("Under 25", "25-29", "30-49", "50-64", "65 and above")))

ggplot(age_df, aes(x = age_category, y = prop, fill=age_category)) +
  geom_bar(stat = "identity") +
  facet_wrap(YEAR ~ ., ncol = 4, scales = "free_y") +
  labs(
    title = "VEP Turnout Rate by Age and Year in Georgia",
    x = "Religion",
    y = "VEP Turnout Rate"
  ) +
  ylab("VEP Turnout Rate")+
  theme_minimal()

# IMPACT OF GENDER ON VOTER TURNOUT RATE BY YEAR (GEORGIA)
gender_df <- df %>%
  filter((STATE_ABV == "GA"), (gender %in% c("Male", "Female", "Other"))) %>%
  group_by(YEAR, gender) %>%
  summarise(n = n()) %>%
  mutate(prop = n / sum(n))

ggplot(gender_df, aes(x = gender, y = prop, fill=gender)) +
  geom_bar(stat = "identity") +
  facet_wrap(YEAR ~ ., ncol = 4, scales = "free_y") +
  labs(
    title = "VEP Turnout Rate by Gender and Year in Georgia",
    x = "Religion",
    y = "VEP Turnout Rate"
  ) +
  ylab("VEP Turnout Rate")+
  theme_minimal()

# IMPACT OF RACE ON VOTER TURNOUT RATE BY YEAR (Georgia)
race_df <- df %>%
  filter((STATE_ABV == "GA"), (race_category %in% c("White non-hispanic", "Black non-hispanic", "Hispanic", "Other"))) %>%
  group_by(YEAR, race_category) %>%
  summarise(n = n()) %>%
  mutate(prop = n / sum(n))

race_df <- race_df %>%
  mutate(race_category = factor(race_category, levels = c("White non-hispanic", "Black non-hispanic", "Hispanic", "Other", "NA", "Uncategorized")))

ggplot(race_df, aes(x = race_category, y = prop, fill=race_category)) +
  geom_bar(stat = "identity") +
  facet_wrap(YEAR ~ ., ncol = 4, scales = "free_y") +
  labs(
    title = "VEP Turnout Rate by Race and Year in Georgia",
    x = "Religion",
    y = "VEP Turnout Rate"
  ) +
  ylab("VEP Turnout Rate")+
  theme_minimal()+theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))



# PART 3: OVERVIEW OF IMPACT OF RELIGION/ RACE/ AGE/GENDER ON VEP TURNOUT RATE ON THESE 5 STATES (NY, FL, GA, IA, IN)
# Religion
# Violin plot (Interactive)
violin_plot_religion_df <-df%>%
  filter((religion_category %in% c("Christian", "Catholic", "Jewish", "Other")),
          (STATE_ABV %in% c("GA", "NY", "IN", "IA", "FL")))
  
violin_plot <- plot_ly(violin_plot_religion_df, x = ~religion_category, y = ~VEP_TURNOUT_RATE, type = "violin",
                       color = ~religion_category, side = "both", box = list(visible = FALSE),
                       name = "Violin Plot")
# Mean points
mean_points <- plot_ly(violin_plot_religion_df, x = ~religion_category, y = ~VEP_TURNOUT_RATE, type = "scatter",
                       mode = "markers", marker = list(color = "white", size = 6),
                       transforms = list(list(type = "aggregate", groups = ~religion_category, 
                                              aggregations = list(list(target = "y", func = "mean")))),
                       name = "Mean Points")
# Error bars
error_bars <- plot_ly(violin_plot_religion_df, x = ~religion_category, y = ~VEP_TURNOUT_RATE, type = "error",
                      color = ~religion_category, opacity = 0.6,
                      error_y = list(type = "data", array = ~sd(VEP_TURNOUT_RATE) / sqrt(length(VEP_TURNOUT_RATE))),
                      name = "Error Bars")
# Combine traces
combined_plot <- add_trace(violin_plot) %>%
  add_trace(mean_points) %>%
  add_trace(error_bars) %>%
  layout(
    title = "Effect of Religion on VEP Turnout Rate",
    xaxis = list(title = "Religion Category"),
    yaxis = list(title = "VEP Turnout Rate (%)"),
    violinmode = "overlay",
    showlegend = FALSE
  )
combined_plot


# Race
# Violin plot (Interactive)
violin_plot_race_df <-df%>%
  filter((race_category %in% c("White non-hispanic", "Black non-hispanic", "Hispanic", "Other")),
         (STATE_ABV %in% c("GA", "NY", "IN", "IA", "FL")))

violin_plot <- plot_ly(violin_plot_race_df, x = ~race_category, y = ~VEP_TURNOUT_RATE, type = "violin",
                       color = ~race_category, side = "both", box = list(visible = FALSE),
                       name = "Violin Plot")
# Mean points
mean_points <- plot_ly(violin_plot_race_df, x = ~race_category, y = ~VEP_TURNOUT_RATE, type = "scatter",
                       mode = "markers", marker = list(color = "white", size = 6),
                       transforms = list(list(type = "aggregate", groups = ~race_category, 
                                              aggregations = list(list(target = "y", func = "mean")))),
                       name = "Mean Points")
# Error bars
error_bars <- plot_ly(violin_plot_race_df, x = ~race_category, y = ~VEP_TURNOUT_RATE, type = "error",
                      color = ~race_category, opacity = 0.6,
                      error_y = list(type = "data", array = ~sd(VEP_TURNOUT_RATE) / sqrt(length(VEP_TURNOUT_RATE))),
                      name = "Error Bars")
# Combine traces
combined_plot <- add_trace(violin_plot) %>%
  add_trace(mean_points) %>%
  add_trace(error_bars) %>%
  layout(
    title = "Effect of Race on VEP Turnout Rate",
    xaxis = list(title = "Race"),
    yaxis = list(title = "VEP Turnout Rate (%)"),
    violinmode = "overlay",
    showlegend = FALSE
  )
combined_plot


# Age
# Violin plot (Interactive)
violin_plot_age_df <-df%>%
  filter((age_category %in% c("Under 25", "25-29", "30-49", "50-64", "65 and above")),
         (STATE_ABV %in% c("GA", "NY", "IN", "IA", "FL")))

violin_plot <- plot_ly(violin_plot_age_df, x = ~age_category, y = ~VEP_TURNOUT_RATE, type = "violin",
                       color = ~age_category, side = "both", box = list(visible = FALSE),
                       name = "Violin Plot")
# Mean points
mean_points <- plot_ly(violin_plot_age_df, x = ~age_category, y = ~VEP_TURNOUT_RATE, type = "scatter",
                       mode = "markers", marker = list(color = "white", size = 6),
                       transforms = list(list(type = "aggregate", groups = ~age_category, 
                                              aggregations = list(list(target = "y", func = "mean")))),
                       name = "Mean Points")
# Error bars
error_bars <- plot_ly(violin_plot_age_df, x = ~age_category, y = ~VEP_TURNOUT_RATE, type = "error",
                      color = ~age_category, opacity = 0.6,
                      error_y = list(type = "data", array = ~sd(VEP_TURNOUT_RATE) / sqrt(length(VEP_TURNOUT_RATE))),
                      name = "Error Bars")
# Combine traces
combined_plot <- add_trace(violin_plot) %>%
  add_trace(mean_points) %>%
  add_trace(error_bars) %>%
  layout(
    title = "Effect of Age on VEP Turnout Rate",
    xaxis = list(title = "Age"),
    yaxis = list(title = "VEP Turnout Rate (%)"),
    violinmode = "overlay",
    showlegend = FALSE
  )
combined_plot


# Gender
# Violin plot (Interactive)
violin_plot_gender_df <-df%>%
  filter((gender %in% c("Male", "Female", "Other")),
         (STATE_ABV %in% c("GA", "NY", "IN", "IA", "FL")))

violin_plot <- plot_ly(violin_plot_gender_df, x = ~gender, y = ~VEP_TURNOUT_RATE, type = "violin",
                       color = ~gender, side = "both", box = list(visible = FALSE),
                       name = "Violin Plot")
# Mean points
mean_points <- plot_ly(violin_plot_gender_df, x = ~gender, y = ~VEP_TURNOUT_RATE, type = "scatter",
                       mode = "markers", marker = list(color = "white", size = 6),
                       transforms = list(list(type = "aggregate", groups = ~gender, 
                                              aggregations = list(list(target = "y", func = "mean")))),
                       name = "Mean Points")
# Error bars
error_bars <- plot_ly(violin_plot_gender_df, x = ~gender, y = ~VEP_TURNOUT_RATE, type = "error",
                      color = ~gender, opacity = 0.6,
                      error_y = list(type = "data", array = ~sd(VEP_TURNOUT_RATE) / sqrt(length(VEP_TURNOUT_RATE))),
                      name = "Error Bars")
# Combine traces
combined_plot <- add_trace(violin_plot) %>%
  add_trace(mean_points) %>%
  add_trace(error_bars) %>%
  layout(
    title = "Effect of Gender on VEP Turnout Rate",
    xaxis = list(title = "Gender"),
    yaxis = list(title = "VEP Turnout Rate (%)"),
    violinmode = "overlay",
    showlegend = FALSE
  )
combined_plot

# PART 4: REGRESSION MODEL OF VEP TURNOUT VS RESPONDENTS' OPINIONS ON ABORTION
# Iowa
df1<-df%>%
  filter(STATE_ABV=='IA')
abort_mapping <- c("NA", "Banned in all cases", "Permitted in Extreme Cases", "Permitted with Clear Need", "Always Permitted", "Other")
df1 <- df1 %>%
  mutate(abort_category = factor(abort_law, labels = abort_mapping))

df1<- df1%>%
  filter(abort_category %in% c("Banned in all cases", "Permitted in Extreme Cases", "Permitted with Clear Need", "Always Permitted"))

# regression model summary
summary(lm(df1$VEP_TURNOUT_RATE ~ df1$abort_category), na.rm=TRUE)

# plot
ggplot(df1, aes(x = abort_category, y = VEP_TURNOUT_RATE, group = 1)) +
  geom_point()  +
  geom_smooth(method = "lm") +
  labs(title = "Effect of People's Opinions on Abortion on Voter Turnout Rate in Iowa",
       x= "Abortion Opinion Categories", y="VEP Turnout Rate (%)")+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


# Indiana
df1<-df%>%
  filter(STATE_ABV=='IN')
abort_mapping <- c("NA", "Banned in all cases", "Permitted in Extreme Cases", "Permitted with Clear Need", "Always Permitted", "Other")
df1 <- df1 %>%
  mutate(abort_category = factor(abort_law, labels = abort_mapping))

df1<- df1%>%
  filter(abort_category %in% c("Banned in all cases", "Permitted in Extreme Cases", "Permitted with Clear Need", "Always Permitted"))

# regression model summary
summary(lm(df1$VEP_TURNOUT_RATE ~ df1$abort_category), na.rm=TRUE)

# plot
ggplot(df1, aes(x = abort_category, y = VEP_TURNOUT_RATE, group = 1)) +
  geom_point()  +
  geom_smooth(method = "lm") +
  labs(title = "Effect of People's Opinions on Abortion on Voter Turnout Rate in Indiana",
       x= "Abortion Opinion Categories", y="VEP Turnout Rate (%)")+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


# New York
df1<-df%>%
  filter(STATE_ABV=='NY')
abort_mapping <- c("NA", "Banned in all cases", "Permitted in Extreme Cases", "Permitted with Clear Need", "Always Permitted", "Other")
df1 <- df1 %>%
  mutate(abort_category = factor(abort_law, labels = abort_mapping))

df1<- df1%>%
  filter(abort_category %in% c("Banned in all cases", "Permitted in Extreme Cases", "Permitted with Clear Need", "Always Permitted"))

# regression model summary
summary(lm(df1$VEP_TURNOUT_RATE ~ df1$abort_category), na.rm=TRUE)

# plot
ggplot(df1, aes(x = abort_category, y = VEP_TURNOUT_RATE, group = 1)) +
  geom_point()  +
  geom_smooth(method = "lm") +
  labs(title = "Effect of People's Opinions on Abortion on Voter Turnout Rate in New York",
       x= "Abortion Opinion Categories", y="VEP Turnout Rate (%)")+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Florida
df1<-df%>%
  filter(STATE_ABV=='FL')
abort_mapping <- c("NA", "Banned in all cases", "Permitted in Extreme Cases", "Permitted with Clear Need", "Always Permitted", "Other")
df1 <- df1 %>%
  mutate(abort_category = factor(abort_law, labels = abort_mapping))

df1<- df1%>%
  filter(abort_category %in% c("Banned in all cases", "Permitted in Extreme Cases", "Permitted with Clear Need", "Always Permitted"))

# regression model summary
summary(lm(df1$VEP_TURNOUT_RATE ~ df1$abort_category), na.rm=TRUE)

# plot
ggplot(df1, aes(x = abort_category, y = VEP_TURNOUT_RATE, group = 1)) +
  geom_point()  +
  geom_smooth(method = "lm") +
  labs(title = "Effect of People's Opinions on Abortion on Voter Turnout Rate in Florida",
       x= "Abortion Opinion Categories", y="VEP Turnout Rate (%)")+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


# Georgia
df1<-df%>%
  filter(STATE_ABV=='GA')
abort_mapping <- c("NA", "Banned in all cases", "Permitted in Extreme Cases", "Permitted with Clear Need", "Always Permitted", "Other")
df1 <- df1 %>%
  mutate(abort_category = factor(abort_law, labels = abort_mapping))

df1<- df1%>%
  filter(abort_category %in% c("Banned in all cases", "Permitted in Extreme Cases", "Permitted with Clear Need", "Always Permitted"))

# regression model summary
summary(lm(df1$VEP_TURNOUT_RATE ~ df1$abort_category), na.rm=TRUE)

# plot
ggplot(df1, aes(x = abort_category, y = VEP_TURNOUT_RATE, group = 1)) +
  geom_point()  +
  geom_smooth(method = "lm") +
  labs(title = "Effect of People's Opinions on Abortion on Voter Turnout Rate in Georgia",
       x= "Abortion Opinion Categories", y="VEP Turnout Rate (%)")+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
