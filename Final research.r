rm(list =ls())  #REMOVED ALL VARIABLES STROED PREVIOUSLY
#LOADING REQUIRED PACKAGES
library(dplyr)
library(ggplot2)
library(tidyverse)
library(infer)
library(stringr)
library(Hmisc)
#Importing the dataset
setwd("C:/Users/Jagaran Chakma/OneDrive/Documents/RESEARCH/R projects")
data<- read.csv("Water scarcity dataset recoded.csv", header = TRUE, sep = ",")
View(data)
head(data, 30)       # FIRST 20 ROWS
tail(data, 30)       # LAST 20 ROWS
data[50:60, ]        # FOR SPECIFIC RANGE


library(dplyr)

# Remove several columns by name
data <- data %>%
  select(-c(name,age, reason_of_water_scarcity, water_affects_work))



#Inspecting the raw data
describe(data)
glimpse(data)
summary(data)
str(data)


#Checking for missing values
sum(is.na(data)) 
colSums(is.na(data))  #NAs per column
duplicated(data)  #check duplicates
data<- distinct(data) #removing duplicates



#ORDINAL FACTORINGS
#WORKDING EDUCATION LEVEL VARIABLE
data$edu_level <- factor(data$edu_level,
                         levels = c("No Education", "Primary Education", 
                                    "Secondary Education", "Higher Education"),
                         ordered = TRUE) #FOR ORDERED FACTORS

str(data$edu_level)
table(data$edu_level, useNA = "ifany")  #INCLUDING MISSING VALUES
sum(is.na(data$edu_level))
class(data$edu_level)


#WORKDING WITH INCOME STATUS
data$income_status<- factor(data$income_status,
                            levels = c("Lower Class", "Middle Class", 
                                       "Higher Class"),
                            ordered = TRUE) #FOR ORDERED FACTORS

table(data$income_status, useNA = "ifany")  #INCLUDING MISSING VALUES
sum(is.na(data$income_status))
class(data$edu_level)



#MENTAL STRESS LEVEL
data$mental_stress_level <- factor(data$mental_stress_level, 
                                   levels = c("Low", "Moderate", "Strong"), 
                                   ordered = TRUE)
class(data$mental_stress_level)


#WORKING WITH HUSBAND OCCUPATION
data$husband_occu <- trimws(data$husband_occu) #REMOVING EXTRA SPACES
describe(data$husband_occu)
table(data$husband_occu)
sum(is.na(data$husband_occu))


#REMOVING EXTRA SPACES USING dplyr and stringr
library(dplyr)
library(stringr)
data <- data %>%
  mutate(across(c(water_source, distance_of_source, edu_level, 
                  drink_from_unreliable_sources, recent_health_problems,
                  time_of_feeling_anxiety
  ),  # list of columns
  ~ str_squish(.)                              # remove all extra spaces
  ))

#FACTORING VARIABLES USING dplyr PACKAGE
library(dplyr)
data <- data %>%   
  mutate(marital_status = as.factor(marital_status)) 
data$distance_of_source<- as.factor(data$distance_of_source)

#FOR NOMINAL MULTIPLE VARIABLES
data <- data %>%
  mutate(across(c("husband_occu","unmarr_face_challenges","sources_of_water",
                  "water_availability","seasonal_water_shortage","purification_of_water",
                  "water_for_handwashing",
                  "drink_from_unreliable_sources", "menstrual_hygiene",
                  "exp_health_issue","medical_evidence",
                  "time_of_feeling_anxiety","govt_programs",
                  "recent_health_problems",
                  "husband_occu"), as.factor))


str(data)



library(tidyverse)
library(tidyr)
class(data$recent_health_problems)
data$ID <- 1:nrow(data)
# Clean up extra spaces, double commas
data$recent_health_problems <- data$recent_health_problems %>%
  gsub(",,", ",", .) %>%                     # Remove double commas
  gsub("\\s*,\\s*", ",", .) %>%              # Remove space around commas
  trimws()                            # Trim leading/trailing white space

# Handle "None of them" as blank
data$recent_health_problems[data$recent_health_problems == "None of them"] <- NA

# Separate into long format (one label per row)
data_long <- data %>%
  separate_rows(recent_health_problems, sep = ",") %>%
  filter(!is.na(recent_health_problems)) %>%
  mutate(label = 1)


# Wide format: one binary column per health problem
data_wide <- pivot_wider(data_long, 
                         id_cols = ID, 
                         names_from = recent_health_problems, 
                         values_from = label,
                         values_fill = 0)

# Fill in zeros for rows with "None of them"
full_data <- full_join(data, data_wide, by = "ID") %>%
  mutate(across(where(is.numeric), ~replace_na(., 0)))

# View final dataset
print(full_data$recent_health_problems)






#MULTILABEL CLASSIFICATION
names(full_data)


library(dplyr)
full_data <- full_data %>%
  select(-c(menstrual_hygiene,unmarr_face_challenges, husband_occu,
            mental_stress_level, medical_evidence,exp_health_issue,ID,
            time_of_feeling_anxiety))


#RENAMING THE DATASET
library(dplyr)
full_data <- full_data %>%
  rename(
    diarrhea = "Diarrhea",
    skin_disease = "Skin Disease",
    uti ="Urinary Tract Infections",
    rhi= "Reproductive Health Issues",
    dehydration ="Dehydration",
    others= "Others",
    malnutrition ="Malnutrition"
  )







library(mlr3verse)
library(mlr3learners)
library(dplyr)
library(randomForest)
library(mlr)
library(mlr3)

label_columns<- c("diarrhea", "skin_disease", 
                  "malnutrition", "dehydration","others",
                  "uti", "rhi" )

# Features are all other columns
feature_columns <- setdiff(names(full_data), label_columns)
# Make sure dependent vars are in df
colnames(full_data)

#MOST IMPORTANT ONE
#CLASSIFIER CHAINS WITH RANDOM FOREST + 5 CROSS VALIDATION
multilabel_df <- full_data[, c(feature_columns, label_columns)]

# Convert character columns to factors (required by mlr3)
multilabel_df <- multilabel_df %>%
  mutate(across(where(is.character), as.factor))

# Manually fix factor levels for water_source
multilabel_df$water_source <- factor(multilabel_df$water_source, 
                                     levels = unique(multilabel_df$water_source))


# Convert labels to logical (if not already)
multilabel_df[label_columns] <- lapply(multilabel_df[label_columns], as.logical)


# Remove problematic columns if any
multilabel_df <- multilabel_df[, !names(multilabel_df) %in% c("recent_health_problems")]

#CONVERTING TO A MULTI-LABEL TASK
task <- makeMultilabelTask(data = multilabel_df, target = label_columns)

# Define base learner: Random Forest
base_learner <- makeLearner(
  "classif.randomForest",
  predict.type = "prob",
  par.vals = list(mtry = 2, ntree = 223,
  importance= TRUE)
)


#WRAP the base learner for multi-label classification
chain_learner <- makeMultilabelClassifierChainsWrapper(base_learner)

# Define 5-fold cross-validation
rdesc <- makeResampleDesc("CV", iters = 5)

#RUN RESAMPLING WITH WRAPPED LEARNER
res_cc <- resample(
  learner = chain_learner,
  task = task,
  resampling = rdesc,
  measures = list(multilabel.hamloss, multilabel.subset01, multilabel.acc, multilabel.f1)
)

print(res_cc$aggr)









#EVALUATING WITH BINARY RELEVANCE WITH RANDOM FOREST
br_learner <- makeMultilabelBinaryRelevanceWrapper(
  makeLearner("classif.randomForest", predict.type = "prob")
)

res_br <- resample(
  learner = br_learner,
  task = task,
  resampling = rdesc,
  measures = list(multilabel.hamloss, multilabel.subset01, multilabel.acc, multilabel.f1)
)

print(res_br$aggr)



#FREQUENCY OF THE HEALTH PROBLEMS
# label_columns already defined
sapply(multilabel_df[label_columns], function(x) {
  tab <- table(x)
  pct_pos <- round(100 * sum(x == TRUE) / length(x), 2)
  c(n_pos = sum(x==TRUE), n_neg = sum(x==FALSE), pct_pos = pct_pos)
})






# VARIABLE IMPORTANCE FOR CLASSIFIER CHAINS
# FIXED VARIABLE IMPORTANCE ANALYSIS

# First, let's fix the feature_columns issue
cat("VARIABLE IMPORTANCE ANALYSIS\n")
cat("============================================================\n")

# Check what columns we have
cat("Available columns in multilabel_df:\n")
print(names(multilabel_df))

# Re-define feature_columns correctly (exclude label columns)
label_columns <- c("diarrhea", "skin_disease", "malnutrition", "dehydration", "others", "uti", "rhi")
feature_columns <- setdiff(names(multilabel_df), label_columns)

cat("Feature columns:", feature_columns, "\n")
cat("Label columns:", label_columns, "\n")

# METHOD 1: Simple Random Forest for one label (GUARANTEED TO WORK)
cat("\n1. SIMPLE RANDOM FOREST IMPORTANCE:\n")

# Convert features to dataframe and handle factors
x_data <- multilabel_df[feature_columns]

# Convert all character columns to factors
x_data <- as.data.frame(lapply(x_data, function(col) {
  if(is.character(col)) as.factor(col) else col
}))

y_data <- as.factor(multilabel_df[[label_columns[1]]])

# Train simple Random Forest
simple_rf <- randomForest(
  x = x_data,
  y = y_data,
  importance = TRUE,
  ntree = 200  # Reduced for speed
)

# Extract importance
if (!is.null(simple_rf$importance)) {
  imp_data <- data.frame(
    feature = rownames(simple_rf$importance),
    importance = as.numeric(simple_rf$importance[, "MeanDecreaseGini"])
  )
  
  # Sort by importance (using base R to avoid dplyr issues)
  imp_data <- imp_data[order(-imp_data$importance), ]
  
  cat("Top 10 features for", label_columns[1], ":\n")
  print(head(imp_data, 10))
  
  # Simple plot
  library(ggplot2)
  p_simple <- ggplot(head(imp_data, 10), 
                     aes(x = importance, y = reorder(feature, importance))) +
    geom_col(fill = "darkred", alpha = 0.8) +
    labs(title = paste("Feature Importance for", label_columns[1]),
         x = "Importance", y = "Features") +
    theme_minimal()
  
  print(p_simple)
  ggsave("simple_importance_plot.png", p_simple, width = 10, height = 8, dpi = 300)
  cat("Plot saved as 'simple_importance_plot.png'\n")
}


# METHOD 3: Manual importance extraction for all labels
cat("\n3. MANUAL IMPORTANCE FOR ALL LABELS:\n")

all_importance <- data.frame()

for (label in label_columns) {
  cat("Processing:", label, "\n")
  
  # Prepare data for this label
  x_data <- multilabel_df[feature_columns]
  x_data <- as.data.frame(lapply(x_data, function(col) {
    if(is.character(col)) as.factor(col) else col
  }))
  y_data <- as.factor(multilabel_df[[label]])
  
  # Remove rows with NA in the target
  complete_cases <- complete.cases(y_data)
  x_clean <- x_data[complete_cases, ]
  y_clean <- y_data[complete_cases]
  
  if (length(unique(y_clean)) > 1) {  # Need at least 2 classes
    # Train RF for this label
    rf_model <- randomForest(
      x = x_clean,
      y = y_clean,
      importance = TRUE,
      ntree = 200
    )
    
    if (!is.null(rf_model$importance)) {
      imp_df <- data.frame(
        feature = rownames(rf_model$importance),
        importance = as.numeric(rf_model$importance[, "MeanDecreaseGini"]),
        label = label
      )
      all_importance <- rbind(all_importance, imp_df)
      cat("  ✓ Importance extracted\n")
    }
  } else {
    cat("  ✗ Skipped - not enough variation in target\n")
  }
}

# Calculate average importance across all labels (using base R)
if (nrow(all_importance) > 0) {
  cat("\n4. AVERAGE IMPORTANCE ACROSS ALL LABELS:\n")
  
  # Calculate average using aggregate (base R)
  avg_imp <- aggregate(importance ~ feature, data = all_importance, FUN = mean)
  avg_imp <- avg_imp[order(-avg_imp$importance), ]
  
  cat("Top 10 features overall:\n")
  print(head(avg_imp, 10))
  
  # Create final plot
  p_final <- ggplot(head(avg_imp, 10), 
                    aes(x = importance, y = reorder(feature, importance))) +
    geom_col(fill = "steelblue", alpha = 0.8) +
    labs(title = "Top 10 Most Important Features Across All Health Problems",
         x = "Average Importance", y = "Features") +
    theme_minimal()
  
  print(p_final)
  ggsave("final_importance_plot.png", p_final, width = 12, height = 8, dpi = 300)
  cat("Final plot saved as 'final_importance_plot.png'\n")
}

cat("\n VARIABLE IMPORTANCE ANALYSIS COMPLETED SUCCESSFULLY!\n")







# BINARY RELEVANCE - VARIABLE IMPORTANCE WITH GRAPH
cat("BINARY RELEVANCE - VARIABLE IMPORTANCE ANALYSIS\n")
cat("============================================================\n")

# Fix feature_columns
label_columns <- c("diarrhea", "skin_disease", "malnutrition", "dehydration", "others", "uti", "rhi")
feature_columns <- setdiff(names(multilabel_df), label_columns)

cat("Feature columns:", length(feature_columns), "columns\n")
cat("Label columns:", length(label_columns), "columns\n")

# Prepare feature data
x_data <- multilabel_df[feature_columns]
x_data <- as.data.frame(lapply(x_data, function(col) {
  if(is.character(col)) as.factor(col) else col
}))

# BINARY RELEVANCE IMPORTANCE EXTRACTION
cat("\n EXTRACTING VARIABLE IMPORTANCE...\n")

br_importance_all <- data.frame()

for (label in label_columns) {
  cat("Processing:", label, "\n")
  
  y_data <- as.factor(multilabel_df[[label]])
  
  # Remove rows with NA
  complete_cases <- complete.cases(y_data) & complete.cases(x_data)
  x_clean <- x_data[complete_cases, ]
  y_clean <- y_data[complete_cases]
  
  if (length(unique(y_clean)) > 1) {
    # Train RF for this label
    rf_model <- randomForest(
      x = x_clean,
      y = y_clean,
      importance = TRUE,
      ntree = 200
    )
    
    if (!is.null(rf_model$importance)) {
      imp_df <- data.frame(
        feature = rownames(rf_model$importance),
        importance = as.numeric(rf_model$importance[, "MeanDecreaseGini"]),
        label = label
      )
      br_importance_all <- rbind(br_importance_all, imp_df)
      cat("  ✓ Success\n")
    }
  } else {
    cat("  ✗ Skipped - not enough variation\n")
  }
}

# CALCULATE AVERAGE IMPORTANCE
cat("\nCALCULATING AVERAGE IMPORTANCE...\n")

if (nrow(br_importance_all) > 0) {
  # Calculate average importance using aggregate
  avg_importance <- aggregate(importance ~ feature, data = br_importance_all, FUN = mean)
  avg_importance <- avg_importance[order(-avg_importance$importance), ]
  
  cat("Top 10 Most Important Features:\n")
  for (i in 1:min(10, nrow(avg_importance))) {
    cat(i, ". ", avg_importance$feature[i], " (", 
        round(avg_importance$importance[i], 2), ")\n", sep = "")
  }
  
  # CREATE GRAPH
  library(ggplot2)
  
  # Take top 15 features for clear visualization
  plot_data <- head(avg_importance, 10)
  
  p <- ggplot(plot_data, aes(x = importance, y = reorder(feature, importance))) +
    geom_col(fill = "steelblue", alpha = 0.8, width = 0.7) +
    labs(
      title = "Variable Importance - Binary Relevance Method",
      subtitle = "Top 10 Most Important Predictors of Health Problems",
      x = "Average Importance Score (Mean Decrease Gini)",
      y = "Features"
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
      plot.subtitle = element_text(size = 12, hjust = 0.5),
      axis.title = element_text(size = 12),
      axis.text.y = element_text(size = 10),
      panel.grid.major.y = element_blank()
    ) +
    scale_x_continuous(expand = expansion(mult = c(0, 0.1)))
  
  # Display plot
  print(p)
  
  # Save plot
  ggsave("binary_relevance_importance.png", p, width = 12, height = 8, dpi = 300, bg = "white")
  cat("\n✅ Graph saved as 'binary_relevance_importance.png'\n")
  
  # Save results to CSV
  write.csv(avg_importance, "binary_relevance_importance.csv", row.names = FALSE)
  cat("Results saved as 'binary_relevance_importance.csv'\n")
  
  # Performance info
  cat("\n BINARY RELEVANCE PERFORMANCE:\n")
  cat("Hamming Loss:", round(res_br$aggr["multilabel.hamloss"], 4), "\n")
  cat("F1 Score:", round(res_br$aggr["multilabel.f1"], 4), "\n")
  
} else {
  cat("No importance data extracted. Check your data.\n")
}

cat("\n BINARY RELEVANCE ANALYSIS COMPLETED!\n")

















