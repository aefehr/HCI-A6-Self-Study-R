# Question 1

# hiVote mean and SD
hiVotes <- read.csv("hiVotes.csv") 
print(mean_hiVotes <- mean(hiVotes$hiVote))
print(sd_hiVotes <- sd(hiVotes$hiVote))

# scoreVote mean and SD
scoreVotes <- read.csv("scoreVotes.csv")
print(mean_scoreVotes <- mean(scoreVotes$scoreVote))
print(sd_scoreVotes <- sd(scoreVotes$scoreVote))

# scoreVote for questions about "wellbeing"
scoreMeta <- read.csv("scoreMetadata.csv")

wellbeing_qs <- scoreMeta$questionId[ scoreMeta$name == "Wellbeing" ]
wellbeing_data <- scoreVotes[ scoreVotes$questionId %in% wellbeing_qs, ]
print(mean_wellbeing <- mean(wellbeing_data$scoreVote))
print(sd_wellbeing <- sd(wellbeing_data$scoreVote))

head(scoreMeta)

# scoreVote for scale question
stress_q <- scoreMeta$questionId[
  scoreMeta$question == 
    "On a scale from 1 to 10, how would you rate the work-related stress?"
]
stress_data <- scoreVotes[ scoreVotes$questionId == stress_q, ]
print(mean_stress <- mean(stress_data$scoreVote))
print(sd_stress <- sd(stress_data$scoreVote))

# Question 2
library(dplyr)
library(ggplot2)
library(stringr)

companyMeta <- read.csv("companyMetadata.csv")

top_industries <- companyMeta %>%
  filter(!is.na(industry) & industry != "") %>%  
  count(industry, name = "n") %>%
  arrange(desc(n)) %>%
  slice_head(n = 10)

ggplot(top_industries, aes(x = reorder(industry, n), y = n)) +
  geom_col(fill = "steelblue") +
  coord_flip(clip = "off") +  
  labs(
    title = "Top 10 Industries by Number of Companies",
    x = "Industry",
    y = "Number of Companies"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    axis.title = element_text(face = "bold"),
    axis.text  = element_text(color = "black"),
    plot.title = element_text(size = 16, face = "bold")
  )

# Question 3
joined <- scoreVotes %>%
  inner_join(scoreMeta, by = c("scoreId", "questionId")) %>%
  filter(!is.na(name) & name != "")

ggplot(joined, aes(x = name, y = scoreVote)) +
  geom_boxplot(fill = "lightblue", outlier.color = "red", outlier.shape = 1) +
  labs(
    title = "Distribution of Score Votes by Score Category",
    x = "Score Category",
    y = "Score Vote (1–10)"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.title = element_text(face = "bold")
  )

# Question 4
joined <- scoreVotes %>%
  inner_join(scoreMeta, by = c("scoreId", "questionId"))

wellbeing_scores <- joined %>%
  filter(name == "Wellbeing")

company_avg <- wellbeing_scores %>%
  group_by(companyId) %>%
  summarise(avg_wellbeing = mean(scoreVote, na.rm = TRUE)) %>%
  arrange(desc(avg_wellbeing))

highest_avg_company <- company_avg %>%
  left_join(companyMeta, by = "companyId") %>%
  slice(1)   

print(highest_avg_company)

# Question 5
merged <- hiVotes %>%
  inner_join(companyMeta, by = "companyId")

filtered <- merged %>%
  filter(industry %in% c("ARTS_ENTERTAINMENT_RECREATION", "FINANCIAL_SERVICES_INSURANCE"))

arts <- filtered %>% filter(industry == "ARTS_ENTERTAINMENT_RECREATION") %>% pull(hiVote)
finance <- filtered %>% filter(industry == "FINANCIAL_SERVICES_INSURANCE") %>% pull(hiVote)

mean_arts <- mean(arts, na.rm = TRUE)
mean_finance <- mean(finance, na.rm = TRUE)

t_test_result <- t.test(arts, finance, alternative = "greater")

print(paste("Arts mean hiVote:", round(mean_arts, 2)))
print(paste("Finance mean hiVote:", round(mean_finance, 2)))
print(paste("p-value:", signif(t_test_result$p.value, 4)))

# Question 6
happiest_company <- hiVotes %>%
  group_by(companyId) %>%
  summarise(avg_hiVote = mean(hiVote, na.rm = TRUE), count = n()) %>%
  arrange(desc(avg_hiVote)) %>%
  left_join(companyMeta, by = "companyId") %>%
  slice(1)

print(happiest_company)

# Question 7 
happpiest_company_by_median <- hiVotes %>%
  group_by(companyId) %>%
  summarise(median_hiVote = median(hiVote, na.rm = TRUE)) %>%
  arrange(desc(median_hiVote)) %>%
  left_join(companyMeta, by = "companyId") %>%
  slice(1)

print(happpiest_company_by_median)

# Question 8 
merged <- hiVotes %>%
  inner_join(companyMeta, by = "companyId") %>%
  filter(!is.na(industry) & industry != "") 

anova_result <- aov(hiVote ~ industry, data = merged)

summary(anova_result)

# Question 9
joined <- scoreVotes %>%
  inner_join(companyMeta, by = "companyId") %>%
  filter(!is.na(timezone) & timezone != "")

lm_fit <- lm(scoreVote ~ timezone, data = joined)

coefs <- summary(lm_fit)$coefficients
timezone_coefs <- as.data.frame(coefs[-1, , drop=FALSE])
timezone_coefs$timezone <- rownames(timezone_coefs)
colnames(timezone_coefs) <- c("Estimate","StdError","tValue","pValue","timezone")

strongest_effect <- timezone_coefs %>% arrange(desc(abs(Estimate))) %>% slice(1)
most_significant <- timezone_coefs %>% arrange(pValue) %>% slice(1)

print(strongest_effect)
print(most_significant)

# Part 2 Question 4a 
overall_mean <- mean(hiVotes$hiVote, na.rm=TRUE)
median_hi <- median(hiVotes$hiVote, na.rm=TRUE)

print(paste("Overall mean HI:", round(overall_mean,2)))

# Part 2 Question 4b
joined <- scoreVotes %>%
  inner_join(scoreMeta, by = c("scoreId","questionId"))

themes <- c("Feedback",
            "Intrinsic Motivation",
            "Relationships",
            "Wellbeing")

theme_stats <- joined %>%
  filter(name %in% themes) %>%
  group_by(name) %>%
  summarise(
    median_score = median(scoreVote, na.rm = TRUE),
  )

print(theme_stats)

# Part 2 Question 5
hi_by_industry <- hiVotes %>%
  inner_join(companyMeta, by = "companyId") %>%
  filter(!is.na(industry) & industry != "")

variability <- hi_by_industry %>%
  group_by(industry) %>%
  summarise(
    sd_hi    = sd(hiVote, na.rm = TRUE),
    n_votes  = n()
  ) %>%
  arrange(desc(sd_hi))

print(variability[1, ])




