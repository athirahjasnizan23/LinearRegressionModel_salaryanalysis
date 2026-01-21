# =====================================================
# 1. LIBRARIES
# =====================================================
library(dplyr)

library(ggplot2)

# =====================================================
# 2. LOAD DATA
# =====================================================
df <- read.csv("Salary_Data.csv")
str(df)

# =====================================================
# 3a. DATA CLEANING & VARIABLE SELECTION
# =====================================================
clean_df <- df %>%
   select(
     Salary,
     Years.of.Experience,
     Job.Title,
     Education.Level
   ) %>%
   filter(
     Salary > 0,
     Years.of.Experience >= 0,
     !is.na(Salary),
     !is.na(Years.of.Experience),
     !is.na(Job.Title),
     !is.na(Education.Level)
   )

# =====================================================
# 3b. STANDARDISE EDUCATION LEVEL LABELS (DUPLICATE)
# =====================================================
clean_df <- clean_df %>%
   mutate(
     Education.Level = case_when(
       Education.Level %in% c("High School", "high school") ~ "High School",
       Education.Level %in% c("Bachelor's", "Bachelor's Degree") ~ "Bachelor's",
       Education.Level %in% c("Master's", "Master's Degree") ~ "Master's",
       Education.Level %in% c("PhD", "phD", "PHD") ~ "PhD",
       TRUE ~ Education.Level
     )
   )
summary(clean_df) 

# =====================================================
# 4a. EXPERIENCE GROUPING (EDA - Descriptive Analytics)
# =====================================================
clean_df <- clean_df %>%
   mutate(
     Experience_Group = cut(
       Years.of.Experience,
       breaks = c(0, 2, 5, 10, 20, 40),
       labels = c("0–2", "3–5", "6–10", "11–20", "20+"),
       include.lowest = TRUE
     )
   )

# =====================================================
# 4b. Salary Summary by Experience
# =====================================================
experience_summary <- clean_df %>%
   group_by(Experience_Group) %>%
   summarise(
     Avg_Salary = mean(Salary),
     Count = n()
   )
print(experience_summary)

# =====================================================
# 4c. Visualization
# =====================================================
ggplot(experience_summary, aes(x = Experience_Group, y = Avg_Salary)) +
   geom_col(fill = "steelblue") +
   labs(
     title = "Average Salary by Years of Experience",
     x = "Experience Group (Years)",
     y = "Average Salary"
   )

# =====================================================
# 5. TRAIN–TEST SPLIT
# =====================================================
set.seed(123)
train_index <- sample(seq_len(nrow(clean_df)), size = 0.8 * nrow(clean_df))
train_data <- clean_df[train_index, ]
test_data  <- clean_df[-train_index, ]

# =====================================================
# 6. FAIRNESS RULE:
# KEEP ONLY JOB TITLES WITH ENOUGH OBSERVATIONS
# =====================================================
min_n <- 30
 
job_counts <- train_data %>%
   count(Job.Title) %>%
   arrange(desc(n))
 
print(job_counts)
 
valid_jobs <- job_counts %>%
   filter(n >= min_n) %>%
   pull(Job.Title)
 
train_data <- train_data %>%
   filter(Job.Title %in% valid_jobs)

test_data <- test_data %>%
   filter(Job.Title %in% valid_jobs)

# =====================================================
# 7. FACTOR CONVERSION (for lm)
# =====================================================
train_data$Job.Title <- factor(train_data$Job.Title)
test_data$Job.Title  <- factor(
   test_data$Job.Title,
   levels = levels(train_data$Job.Title)
 )
 
train_data$Education.Level <- factor(train_data$Education.Level)
test_data$Education.Level  <- factor(
   test_data$Education.Level,
   levels = levels(train_data$Education.Level)
 )

# =====================================================
# 8. BUILD MULTIPLE LINEAR REGRESSION MODEL
# =====================================================
final_model <- lm(
   Salary ~ Years.of.Experience + Job.Title + Education.Level,
   data = train_data
 )
 
summary(final_model)

# =====================================================
# 9. PREDICTION & MODEL ACCURACY
# =====================================================
test_data$Predicted_Salary <- predict(final_model, newdata = test_data)
 
rmse <- sqrt(mean((test_data$Salary - test_data$Predicted_Salary)^2))
rmse

# =====================================================
# 10. FAIRNESS CLASSIFICATION
# =====================================================
test_data <- test_data %>%
   mutate(
     Salary_Difference = Salary - Predicted_Salary,
     Fairness_Status = case_when(
       Salary_Difference < -rmse ~ "Underpaid",
       Salary_Difference >  rmse ~ "Overpaid",
       TRUE                      ~ "Fairly Paid"
     )
   )
 
# View examples
head(test_data %>% filter(Fairness_Status == "Underpaid"))
head(test_data %>% filter(Fairness_Status == "Fairly Paid"))
head(test_data %>% filter(Fairness_Status == "Overpaid"))
 
table(test_data$Fairness_Status)

# =====================================================
# 11a. FAIRNESS VISUALIZATIONS (PAY GAP BY EXPERIENCE)
# =====================================================
ggplot(test_data, aes(x = Experience_Group, y = Salary_Difference)) +
  geom_boxplot(fill = "lightblue") +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_hline(yintercept = c(-rmse, rmse),
             linetype = "dotted", color = "red") +
  labs(
    title = "Pay Gaps Among Employees with Similar Experience",
    x = "Experience Group",
    y = "Actual − Predicted Salary"
  )

# =====================================================
# 11b. PAY GAP BY EDUCATION LEVEL
# =====================================================
ggplot(test_data, aes(x = Education.Level, y = Salary_Difference)) +
  geom_boxplot(fill = "lightblue") +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_hline(yintercept = c(-rmse, rmse),
             linetype = "dotted", color = "red") +
  labs(
    title = "Pay Gaps Among Employees with Similar Education Level",
    x = "Education Level",
    y = "Actual − Predicted Salary"
  )

# =====================================================
# 11c. SALARY GAP DISTRIBUTION AMONG EMPLOYEES
# =====================================================
ggplot(test_data, aes(x = Salary_Difference, fill = Fairness_Status)) +
   geom_histogram(bins = 40, alpha = 0.7) +
   geom_vline(xintercept = c(-rmse, rmse),
              linetype = "dashed",
              color = "red") +
   labs(
     title = "Salary Fairness Distribution",
     x = "Actual − Predicted Salary",
     y = "Number of Employees"
   )

# =====================================================
# 12. JOB TITLE SAMPLE SIZE (APPENDIX)
# =====================================================
train_data %>%
   count(Job.Title) %>%
   arrange(n)





























