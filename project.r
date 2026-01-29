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
data<- read.csv("Research final dataset.csv", header = TRUE, sep = ",")
View(data)
head(data, 30)       # FIRST 20 ROWS
tail(data, 30)       # LAST 20 ROWS
data[50:60, ]        # FOR SPECIFIC RANGE


library(dplyr)

# Remove several columns by name
data <- data %>%
  select(-c(name, age, reason_of_water_scarcity, water_affects_work))




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


#REMOVING EXTRA SPACES USING dplyr and stringr
library(dplyr)
library(stringr)
data <- data %>%
  mutate(across(c(water_source, distance_of_source, marital_status, edu_level, 
                  drink_from_unreliable_sources, recent_health_problems,
                  time_of_feeling_anxiety 
  ),  # list of columns
  ~ str_squish(.)                              # remove all extra spaces
  ))

#ORDINAL FACTORINGS
#WORKDING EDUCATION LEVEL VARIABLE
data$husband_occu <- trimws(data$husband_occu) #REMOVING EXTRA SPACES

data$edu_level <- factor(data$edu_level,
                         levels = c("No Education", "Primary Education", 
                                    "Secondary Education", "Higher Education"),
                         ordered = TRUE) #FOR ORDERED FACTORS

str(data$edu_level)
table(data$edu_level, useNA = "ifany")  #INCLUDING MISSING VALUES
sum(is.na(data))
class(data$edu_level)


#WORKDING WITH INCOME STATUS
data$income_status <- trimws(data$income_status) #REMOVING EXTRA SPACES

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



#FACTORING VARIABLES USING dplyr PACKAGE
library(dplyr)
data <- data %>%   
  mutate(marital_status = as.factor(marital_status)) 
data$distance_of_source<- as.factor(data$distance_of_source)

#FOR NOMINAL MULTIPLE VARIABLES
data <- data %>%
  mutate(across(c("husband_occu","unmarr_face_challenges","water_source",
                  "water_availability","seasonal_water_shortage","purification_method",
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
data$recent_health_problems[data$recent_health_problems == "None of them"] <- "others"

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
names(full_data)

library(dplyr)
full_data <- full_data %>%
  select(-c(menstrual_hygiene,unmarr_face_challenges, husband_occu,
            mental_stress_level, medical_evidence, exp_health_issue,ID,
            time_of_feeling_anxiety, recent_health_problems))

names(full_data)




#RENAMING THE DATASET
library(dplyr)
full_data <- full_data %>%
  rename(
    diarrhea = "Diarrhea",
    skin_disease = "Skin Disease",
    malnutrition ="Malnutrition",
    dehydration ="Dehydration",
    uti ="Urinary Tract Infections",
    rhi= "Reproductive Health Issues",
    others= "Others"
    
  )

tail(data, 30)


#MULTILABEL CLASSIFICATION
library(mlr3)
library(mlr)
#library(VIM)  # For KNN imputation
#library(ROSE)  # For oversampling (alternative to SMOTE)
library(parallel)
library(parallelMap)
set.seed(123)


#labels
label_columns<- c("diarrhea", "skin_disease", 
                  "malnutrition", "dehydration",
                  "others","uti", "rhi" )


# Features are all other columns
feature_columns <- setdiff(names(full_data), label_columns)
# Make sure dependent vars are in df
colnames(full_data)


#write.csv(full_data, file = "processed_data.csv", row.names = FALSE)
# Calculate correlation matrix between labels
label_corr <- cor(full_data[, label_columns], use = "pairwise.complete.obs")
print(label_corr)
# Visualize the correlation matrix
library(corrplot)
library(ggplot2)
library(reshape2)
corrplot(label_corr, method = "color", type = "upper", 
         tl.col = "black", tl.srt = 45)

# Convert correlation matrix to long format
corr_df <- melt(label_corr)

# To see the exact correlation values from your data:
cor_matrix <- cor(full_data[, label_columns], use = "complete.obs")
print(round(cor_matrix, 4))

ggplot(corr_df, aes(x = Var1, y = Var2, fill = value)) +
  geom_tile(color = "white") +
  geom_text(aes(label = round(value, 2)), size = 4) +
  scale_fill_gradient2(
    low = "blue",
    mid = "white",
    high = "red",
    midpoint = 0,
    limits = c(-1, 1),
    name = "Correlation"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    axis.title = element_blank(),
    panel.grid = element_blank()
  ) +
  labs(title = "Correlation Between Disease Labels")



#CLASSIFIER CHAINS WITH RANDOM FOREST + 10 FOLD CROSS VALIDATION
multilabel_df <- full_data[, c(feature_columns, label_columns)]

# Convert character columns to factors (required by mlr3)
multilabel_df <- multilabel_df %>%
  mutate(across(where(is.character), as.factor))

# Manually fix factor levels for water_source
multilabel_df$water_source <- factor(multilabel_df$water_source, 
                                     levels = unique(multilabel_df$water_source))


#Convert labels to logical (if not already)
multilabel_df[label_columns] <- lapply(multilabel_df[label_columns], as.logical)


#Remove problematic columns if any
multilabel_df <- multilabel_df[, !names(multilabel_df) %in% c("recent_health_problems")]

colnames(multilabel_df) <- make.names(colnames(multilabel_df), unique = TRUE)

#CONVERTING TO A MULTI-LABEL TASK
task <- makeMultilabelTask(data = multilabel_df, target = label_columns)


#Define base learner: Random Forest
base_learner <- makeLearner(
  "classif.randomForest",
  predict.type = "prob",
  par.vals = list(
    ntree = 500,  # Reduced for speed
    mtry = 3,
    nodesize = 5,
    classwt = c("FALSE" = 1, "TRUE" = 5), # Higher weight for rare labels
    importance = TRUE
  )
)

#WRAP the base learner for multi-label classification
chain_learner <- makeMultilabelClassifierChainsWrapper(base_learner)

#Hyper-parameter Tuning
ps <- makeParamSet(
  makeIntegerParam("ntree", lower = 100, upper = 500),
  makeIntegerParam("mtry", lower = 2, upper = 5),
  makeIntegerParam("nodesize", lower = 1, upper = 6)
)
rdesc <- makeResampleDesc("CV", iters = 10)  # 10-fold CV 
ctrl <- makeTuneControlRandom(maxit = 20)   # Minimal random search

#TUNING PARAMETERS
tune <- tuneParams(
  learner = chain_learner,
  task = task,
  resampling = rdesc,
  measures = list(multilabel.hamloss),
  par.set = ps,
  control = ctrl,
  show.info = TRUE
)

#Train Optimized Model
optimized_learner <- setHyperPars(chain_learner, par.vals = tune$x)
model <- train(optimized_learner, task)
listMeasures("multilabel")


# Cross-validated performance
res_cc <- resample(
  learner = optimized_learner,
  task = task,
  resampling = rdesc,
  measures = list(multilabel.acc,
                  multilabel.ppv, #Positive Predictive Value = precision
                  multilabel.tpr,  #True Positive Rate= Recall = Sensitivity,
                  multilabel.f1,
                  multilabel.hamloss,
                  multilabel.subset01)
)
print(res_cc$aggr)

#PER LABEL BINARY PERFORAMANCE(CLASSIFIER CHAINS)
pred_full <- predict(model, task)
# Per-label binary AUCs (one per label_column)
label_aucs <- getMultilabelBinaryPerformances(pred_full, measures = auc)
print("Per-label AUCs:")
print(label_aucs)  # Vector: names = your labels (diarrhea, skindisease, etc.)

# Macro AUC (simple average across labels)
macro_auc <- mean(label_aucs, na.rm = TRUE)
print(paste("Macro AUC:", round(macro_auc, 4)))

# Full binary table (acc, auc, etc. per label)
binary_table <- getMultilabelBinaryPerformances(pred_full, 
                                           measures = list(acc, auc, f1,ppv))
print(binary_table)
# ROC curves (unchanged - works after generateThreshVsPerfData)



#EVALUATING BASED ON LABEL POWERSET
#Per-Label Performance
pred <- predict(model, task = task)
perf <- performance(pred, measures = 
                      list(multilabel.f1, 
                           multilabel.ppv,
                           multilabel.tpr,
                           multilabel.acc), 
                    task = task)

perf

#EVALUATING WITH BINARY RELEVANCE WITH OPTIMIZED RANDOM FOREST
optimized_br <- setHyperPars(
  makeMultilabelBinaryRelevanceWrapper(base_learner),
  par.vals = tune$x
)

# Cross-validated performance
res_br <- resample(
  learner = optimized_br,
  task = task,
  resampling = rdesc,
  measures = list(multilabel.acc,
                  multilabel.ppv, #Positive Predictive Value = precision
                  multilabel.tpr,  #True Positive Rate= Recall = Sensitivity,
                  multilabel.f1,
                  multilabel.hamloss,
                  multilabel.subset01)
)

print(res_br$aggr)



#FREQUENCY OF THE HEALTH PROBLEMS
# label_columns already defined
sapply(multilabel_df[label_columns], function(x) {
  tab <- table(x)
  pct_pos <- round(100 * sum(x == TRUE) / length(x), 2)
  c(n_pos = sum(x==TRUE), n_neg = sum(x==FALSE), pct_pos = pct_pos)
})




# VARIABLE IMPORTANCE WITH RANDOM FOREST
print(names(multilabel_df))

# Re-define feature_columns correctly (exclude label columns)
label_columns <- c("diarrhea", "skin_disease", "malnutrition", "dehydration", "others", "uti", "rhi")
feature_columns <- setdiff(names(multilabel_df), label_columns)



# METHOD 1: Simple Random Forest for one label (GUARANTEED TO WORK)
cat("\n1. SIMPLE RANDOM FOREST IMPORTANCE:\n")

# Convert features to dataframe and handle factors
x_data <- multilabel_df[feature_columns]

# Convert all character columns to factors
x_data <- as.data.frame(lapply(x_data, function(col) {
  if(is.character(col)) as.factor(col) else col
}))

y_data <- as.factor(multilabel_df[[label_columns[1]]])

library(randomForest)
# Train simple Random Forest
simple_rf <- randomForest(
  x = x_data,
  y = y_data,
  importance = TRUE,
  ntree = 300  # Reduced for speed
)

# Extract importance
if (!is.null(simple_rf$importance)) {
  imp_data <- data.frame(
    feature = rownames(simple_rf$importance),
    importance = as.numeric(simple_rf$importance[, "MeanDecreaseGini"])
  )
  
  # Sort by importance (using base R to avoid dplyr issues)
  imp_data <- imp_data[order(-imp_data$importance), ]

# IMPROVED CLEAR PLOT - Fixed margin error
library(ggplot2)
top_imp <- head(imp_data, 10)

p_clear <- ggplot(top_imp, aes(x = reorder(feature, importance), y = importance)) +
  geom_col(fill = "steelblue", width = 0.75, alpha = 1) +
  coord_flip() +
  labs(
    title = paste("Top 10 Feature Importance for", label_columns[3]),
    x = "Features",
    y = "Importance"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5, 
                              margin = ggplot2::margin(b = 15)),  # Fixed: ggplot2::margin()
    axis.text.y = element_text(size = 12, color = "black", hjust = 0.5),
    axis.text.x = element_text(size = 12, color = "black"),
    axis.title = element_text(size = 13, face = "bold"),
    panel.grid.major.x = element_line(color = "grey85", linewidth = 0.4),
    panel.grid.minor = element_blank(),
    plot.margin = ggplot2::margin(t = 20, r = 25, b = 20, l = 60),  # Fixed here too
    axis.ticks = element_line(color = "grey80")
  ) +
  scale_y_continuous(expand = ggplot2::expansion(mult = c(0, 0.08)))  # Fixed expansion

print(p_clear)}

# Save high-quality version
ggsave("clear_feature_importance_SKIN.png", plot = p_clear, 
       width = 9.5, height = 7, dpi = 320, bg = "white")






# METHOD 3: Manual importance extraction for all labels (your existing code up to avg_imp)

if (nrow(all_importance) > 0) {
  cat("\n4. AVERAGE IMPORTANCE ACROSS ALL LABELS:\n")
  
  # Calculate average using aggregate (base R)
  avg_imp <- aggregate(importance ~ feature, data = all_importance, FUN = mean)
  avg_imp <- avg_imp[order(-avg_imp$importance), ]
  
  cat("Top 10 features overall:\n")
  print(head(avg_imp, 10))
  
  # IMPROVED CLEAR PLOT - Same styling as your diarrhea plot
  library(ggplot2)
  top_avg <- head(avg_imp, 10)
  
  p_final_clear <- ggplot(top_avg, aes(x = reorder(feature, importance), y = importance)) +
    geom_col(fill = "steelblue", width = 0.75, alpha = 1) +
    coord_flip() +
    labs(
      title = "Top 10 Most Important Features Across All Health Problems",
      subtitle = "Average MeanDecreaseGini - Multi-Label Random Forest",
      x = "Features",
      y = "Average Importance"
    ) +
    theme_minimal(base_size = 13) +
    theme(
      plot.title = element_text(size = 16, face = "bold", hjust = 0.5, 
                                margin = ggplot2::margin(b = 15)),
      plot.subtitle = element_text(size = 13, hjust = 0.5),
      axis.text.y = element_text(size = 12, color = "black", hjust = 0.5),
      axis.text.x = element_text(size = 12, color = "black"),
      axis.title = element_text(size = 13, face = "bold"),
      panel.grid.major.x = element_line(color = "grey85", linewidth = 0.4),
      panel.grid.minor = element_blank(),
      plot.margin = ggplot2::margin(t = 25, r = 25, b = 20, l = 60),
      axis.ticks = element_line(color = "grey80")
    ) +
    scale_y_continuous(expand = ggplot2::expansion(mult = c(0, 0.08)))
  
  print(p_final_clear)
  
  # Save high-quality version
  ggsave("clear_avg_feature_importance_multilabel.png", plot = p_final_clear, 
         width = 10, height = 7.5, dpi = 320, bg = "white")
}






# BINARY RELEVANCE - VARIABLE IMPORTANCE WITH GRAPH
cat("BINARY RELEVANCE - VARIABLE IMPORTANCE ANALYSIS\n")

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


# [Keep all your existing code until the CREATE GRAPH section unchanged]

# CREATE GRAPH - IMPROVED CLEAR VISUALIZATION (replace your plotting section)
library(ggplot2)

# Take top 10 features for clear visualization
plot_data <- head(avg_importance, 10)

p_clear_br <- ggplot(plot_data, aes(x = reorder(feature, importance), y = importance)) +
  geom_col(fill = "steelblue", width = 0.75, alpha = 1) +
  coord_flip() +
  labs(
    title = "Variable Importance - Binary Relevance Method",
    subtitle = "Top 10 Most Important Predictors of Health Problems\nAverage MeanDecreaseGini Across All Labels",
    x = "Features",
    y = "Average Importance Score (Mean Decrease Gini)"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5, 
                              margin = ggplot2::margin(b = 15)),
    plot.subtitle = element_text(size = 13, hjust = 0.5, lineheight = 1.2),
    axis.text.y = element_text(size = 12, color = "black", hjust = 0.5),
    axis.text.x = element_text(size = 12, color = "black"),
    axis.title = element_text(size = 13, face = "bold"),
    panel.grid.major.x = element_line(color = "grey85", linewidth = 0.4),
    panel.grid.minor = element_blank(),
    plot.margin = ggplot2::margin(t = 25, r = 25, b = 20, l = 65),
    axis.ticks = element_line(color = "grey80")
  ) +
  scale_y_continuous(expand = ggplot2::expansion(mult = c(0, 0.08)))

# Display plot
print(p_clear_br)

# Save high-quality publication version
ggsave("binary_relevance_clear_importance.png", plot = p_clear_br, 
       width = 10, height = 7.5, dpi = 320, bg = "white")
cat("Clear visualization saved as 'binary_relevance_clear_importance.png'\n")

# [Keep your existing CSV save and performance output unchanged]
write.csv(avg_importance, "binary_relevance_importance.csv", row.names = FALSE)
cat("Results saved as 'binary_relevance_importance.csv'\n")

cat("\n BINARY RELEVANCE PERFORMANCE:\n")
cat("Hamming Loss:", round(res_br$aggr["multilabel.hamloss"], 4), "\n")
cat("F1 Score:", round(res_br$aggr["multilabel.f1"], 4), "\n")












library(ggplot2)
library(dplyr)

# Your data with CORRECT values
correct_data <- data.frame(
  Metric = rep(c("Accuracy", "F1 Score", "Subset Accuracy", "Hamming Loss"), 2),
  Model = rep(c("Binary Relevance (RF)", "Classifier Chains (RF)"), each = 4),
  Value = c(0.221, 0.281, 0.842, 0.233, 0.237, 0.280, 0.875, 0.235)
)

# Create the final research-ready plot
ggplot(correct_data, aes(x = Metric, y = Value, fill = Model)) +
  geom_bar(stat = "identity", position = position_dodge(0.8), width = 0.7, alpha = 0.9) +
  geom_text(aes(label = sprintf("%.3f", Value)), 
            position = position_dodge(0.8), vjust = -0.5, size = 3.5, fontface = "bold") +
  scale_fill_manual(values = c("Binary Relevance (RF)" = "#E41A1C", 
                               "Classifier Chains (RF)" = "#377EB8")) +
  labs(title = "Multi-label Classification Performance Comparison",
       subtitle = "Random Forest Base Classifier | 5-Fold Cross-Validation",
       y = "Metric Value",
       x = "",
       caption = "Note: Higher values are better for Accuracy, F1-Score, and Subset Accuracy\nLower values are better for Hamming Loss") +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
    plot.subtitle = element_text(hjust = 0.5, color = "gray40", size = 11),
    axis.text.x = element_text(angle = 45, hjust = 1, size = 10),
    legend.position = "top",
    legend.title = element_text(face = "bold"),
    panel.grid.major = element_line(color = "gray90"),
    panel.grid.minor = element_blank(),
    plot.caption = element_text(color = "gray50", size = 9, hjust = 0)
  ) +
  ylim(0, 1.0)

# Create professional research figure
ggplot(correct_data, aes(x = Metric, y = Value, fill = Model)) +
  geom_col(position = position_dodge(0.8), width = 0.7) +
  geom_text(aes(label = sprintf("%.3f", Value)), 
            position = position_dodge(0.8), vjust = -0.5, size = 4) +
  scale_fill_manual(values = c("Binary Relevance (RF)" = "#E41A1C", 
                               "Classifier Chains (RF)" = "#377EB8")) +
  labs(title = "Multi-label Classification Performance Comparison",
       subtitle = "Random Forest Base Classifier | 5-Fold Cross-Validation",
       y = "Metric Value",
       x = "") +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 16),
    plot.subtitle = element_text(hjust = 0.5, size = 12),
    axis.text.x = element_text(angle = 45, hjust = 1, size = 11),
    legend.position = "top"
  ) +
  ylim(0, 1.0)




#frequencies for research paper
#DEALING WITH MARITAL STATUS COLUMN
library(dplyr)
library(ggplot2)

ggplot(full_data, aes(x = marital_status)) +
  geom_bar(
    color = "black",
    fill = "grey70",
    width = 0.6
  ) +
  geom_text(
    stat = "count",
    aes(label = after_stat(count)),
    vjust = -0.4,
    size = 3.8
  ) +
  labs(
    x = "Marital Status",
    y = "Frequency"
  ) +
  theme_classic(base_size = 12) +
  theme(
    axis.title = element_text(face = "bold"),
    axis.text = element_text(color = "black"),
    plot.title = element_text(hjust = 0.5)
  )

# Prepare data
marital_df <- full_data %>%
  count(marital_status) %>%
  mutate(
    percent = n / sum(n) * 100,
    label = paste0(round(percent, 1), "%")
  )


#Education level variable
# Vertical bar chart
library(dplyr)
library(ggplot2)

ggplot(full_data, aes(x = water_source)) +
  geom_bar(
    color = "black",
    fill = "grey70",
    width = 0.6
  ) +
  geom_text(
    stat = "count",
    aes(label = after_stat(count)),
    vjust = -0.4,
    size = 3.8
  ) +
  labs(
    x = "Sources of Water",
    y = "Frequency"
  ) +
  theme_classic(base_size = 12) +
  theme(
    axis.title = element_text(face = "bold"),
    axis.text = element_text(color = "black")
  )



#Water source
# Prepare data: frequency by water source
library(dplyr)
library(ggplot2)

water_data <- full_data %>%
  count(water_source) %>%
  mutate(
    percentage = round(n / sum(n) * 100, 1),
    label = paste0(percentage, "%")
  )

ggplot(water_data, aes(x = "", y = n, fill = water_source)) +
  geom_bar(stat = "identity", width = 1, color = "black", size = 0.6) +
  coord_polar(theta = "y") +
  geom_text(
    aes(label = label),
    position = position_stack(vjust = 0.5),
    size = 5,          # bigger labels
    color = "black"
  ) +
  labs(
    title = "Sources of Household Water",
    fill  = "Sources of Water"
  ) +
  theme_void(base_size = 14) +
  theme(
    plot.title   = element_text(hjust = 0.5, face = "bold"),
    legend.title = element_text(face = "bold")
  )




#DISTANCE OF WATER SOURCES
library(dplyr)
library(ggplot2)

ggplot(full_data, aes(x = distance_of_source)) +
  geom_bar(
    color = "black",
    fill  = "grey80",
    width = 0.6
  ) +
  geom_text(
    stat  = "count",
    aes(label = after_stat(count)),
    vjust = -0.3,
    size  = 4
  ) +
  labs(
    x = "Distance of Water Sources",
    y = "Frequency"
  ) +
  theme_classic(base_size = 13) +
  theme(
    axis.title = element_text(face = "bold"),
    axis.text  = element_text(color = "black")
  )


