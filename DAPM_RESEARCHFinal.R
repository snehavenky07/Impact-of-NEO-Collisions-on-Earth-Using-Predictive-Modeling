install.packages("readxl")
install.packages("dplyr")
install.packages("stringr")

# Load the packages
library(readxl)
library(leaps)
library(Metrics)
library(vip)
library(ROSE)
library(dplyr)
library(ggplot2)
library(stringr)
library(tidyr)

# Read the data
setwd("C:/")
data <- read.csv("dataset.csv")

head(data, 5)

tail(data, 5)

# Number of rows
num_rows <- nrow(data)
cat("Number of Rows:", num_rows, "\n")

# Number of columns
num_cols <- ncol(data)
cat("Number of Columns:", num_cols, "\n")

# column names
colnames(data)

str(data)

column_types <- sapply(data, class)
# Printing the result
print(column_types)

# EDA - name
# Strip and lower-case the 'name' column
data$name <- tolower(str_trim(data$name))
name_counts <- table(data$name)
# Sort the table in descending order of counts
sorted_name_counts <- sort(name_counts, decreasing = TRUE)
# Take the top 10 names
top_10_names <- sorted_name_counts[1:10]
# Create a bar plot
barplot(top_10_names, main = "Top 10 Names", xlab = "Name", ylab = "Frequency", col = "skyblue", las = 2)

# EDA - neo
neo_counts <- table(data$neo)
neo_percentages <- neo_counts / sum(neo_counts) * 100
pie(neo_counts, labels = paste(names(neo_counts), "\n", 
                               sprintf("%.1f%%", neo_percentages)), 
    main = "Percent of asteroids are near earth objects")

# EDA - pha
pha_counts <- table(data$pha)
pha_percentages <- pha_counts / sum(pha_counts) * 100
pie(pha_counts, labels = paste(names(pha_counts), "\n", 
                               sprintf("%.1f%%", pha_percentages)), 
    main = "Percent of potentially hazardous asteroids")

# EDA - class
class_counts <- table(data$class)
# Sort the table in descending order of counts
sorted_class_counts <- sort(class_counts, decreasing = TRUE)
# Create a bar plot
barplot(sorted_class_counts, main = "Distribution of the orbit classification", 
        xlab = "Class", ylab = "Frequency", col = "skyblue", las = 2)

# EDA - Hazardous and Non-Hazardous neo
tmp_df <- data %>%
  group_by(neo, pha) %>%
  summarise(counts = n()) %>%
  filter(pha %in% c("N", "Y"))
# Plotting
ggplot(tmp_df, aes(x = neo, y = counts, fill = pha)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(x = "neo", y = "count", title = "Number of Hazardous and Non-Hazardous NEOs") +
  scale_fill_manual(values = c("N" = "blue", "Y" = "red"), name = "Category") +
  theme_minimal()

# EDA - orbit_id
orbit_id_counts <- table(data$orbit_id)
# Sort the table in descending order of counts
sorted_orbit_id_counts <- sort(orbit_id_counts, decreasing = TRUE)
# Take the top 10 names
top_10_orbit_ids <- sorted_orbit_id_counts[1:10]
# Create a bar plot
barplot(top_10_orbit_ids, main = "Distribution of the top orbit ids", xlab = "Name", ylab = "Frequency", col = "skyblue", las = 2)

# EDA - diameter
boxplot(diameter ~ class, data = data, main = "Boxplot of Diameter by Class", xlab = "Class", ylab = "Diameter")

# EDA - diameter by pha
ggplot(data, aes(x = diameter, fill = pha)) +
  geom_density(alpha = 0.5) +
  labs(title = "KDE Plot of Diameter by pha", x = "Diameter", fill = "pha")

# EDA - diameter by pha
ggplot(data, aes(x = per, fill = pha)) +
  geom_density(alpha = 0.5) +
  labs(title = "KDE Plot of per by pha", x = "per", fill = "pha")

# count of nans for each column
nan_counts <- colSums(is.na(data))
print(nan_counts)

# drop name and prefix
data <- data[, !(names(data) %in% c("name", "prefix"))]

# omit nan values
data <- na.omit(data)

# Number of rows after removing nan
num_rows <- nrow(data)
cat("Number of Rows:", num_rows, "\n")

# Number of columns after removing nan
num_cols <- ncol(data)
cat("Number of Columns:", num_cols, "\n")

# encode pha and neo
data$pha <- ifelse(data$pha == "N", 0, 1)
data$neo <- ifelse(data$neo == "N", 0, 1)

#freq_table <- table(data$orbit_id)
#least_freq_unique <- names(freq_table)[which(freq_table == min(freq_table))]

#removing upper bound outliers for 'a'
# Calculate the first and third quartiles (Q1 and Q3)
Q1 <- quantile(data$a, 0.25)
Q3 <- quantile(data$a, 0.75)
# Calculate the Interquartile Range (IQR)
IQR_value <- IQR(data$a)
# Set the upper bound as Q3 + 1.5 * IQR
upper_bound <- Q3 + 1.5 * IQR_value
# Remove values above the dynamically determined upper bound
data_no_outliers_a <- data[data$a <= upper_bound, ]

#print(data_no_outliers_a)
nrow(data_no_outliers_a)

#Removing un-necessary columns
columns_to_remove <- c("id", "full_name", "name", "class", "spkid", "pdes", "orbit_id")
# Remove specified columns
final_data <- data[, -which(names(data) %in% columns_to_remove)]

# Identify variables with only one unique value
single_value_vars <- sapply(final_data, function(x) length(unique(x)) == 1)
# Remove these variables from the data
final_data <- final_data[ , !single_value_vars]

#shuffle data
shuffled_data <- final_data[sample(nrow(final_data)), ]

# Subset the data where 'pha' is 1
data_1 <- shuffled_data[shuffled_data$pha == 1, ]

# Subset the data where 'pha' is 0
data_0 <- shuffled_data[shuffled_data$pha == 0, ]

# Randomly sample 1000 rows from the 0s data
set.seed(123) # for reproducibility
data_0_sample <- data_0[sample(nrow(data_0), size = 500), ]

# Combine the sampled 0s data with the 1s data
shuffled_data <- rbind(data_1, data_0_sample)

data_balanced <- ovun.sample(pha ~ ., data = shuffled_data, method = "both", N = 100000)$data
counts <- table(data_balanced$pha)

# Print the counts
print(counts)


# Split the data into training (70%) and testing (30%) sets
set.seed(123)  # for reproducibility
n <- nrow(data_balanced)
train_indices <- sample(1:n, 0.7 * n)
train_data <- data_balanced[train_indices, ]
test_data <- data_balanced[-train_indices, ]
unique(train_data$pha)

# correlation matrix
# Assuming 'train_data' is your data frame
cor_matrix <- cor(train_data)
cor_melted <- as.data.frame(as.table(cor_matrix))
colnames(cor_melted) <- c("Variable1", "Variable2", "Correlation")

# Create the correlation plot
ggplot(cor_melted, aes(x = Variable1, y = Variable2, fill = Correlation)) +
  geom_tile() +
  geom_text(aes(label = round(Correlation, 2)), vjust = 0.5, size = 2) + 
  scale_fill_gradient2(low = "blue", mid = "white", high = "red", midpoint = 0, limit = c(-1, 1)) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))


# Logistic Regression
lr_model <- glm(pha ~ ., family = binomial, data = train_data)
summary(lr_model)
# Logistic Regression refined
lr_model <- glm(pha ~ moid_ld+ q + moid + e+ diameter + neo, family = binomial, data = train_data)
# Make predictions
predictions <- predict(lr_model, newdata = test_data, type = "response")
# Convert probabilities to class labels
lr_predictions <- ifelse(predictions > 0.5, 1, 0)
# accuracy
lr_accuracy <- sum(lr_predictions == test_data$pha) / nrow(test_data)
print(paste("Logistic Regression Accuracy: ", lr_accuracy))
# recall
true_positives <- sum(lr_predictions == 1 & test_data$pha == 1)
false_negatives <- sum(lr_predictions == 0 & test_data$pha == 1)
lr_recall <- true_positives / (true_positives + false_negatives)
print(paste("Logistic Regression Recall: ", lr_recall))

# Decision Trees
library(rpart)
dt_model_pre <- rpart(pha ~ ., data = train_data, method = "class")
vip(dt_model_pre)
printcp(dt_model)
dt_model <- rpart(pha ~ q + moid_ld + moid + neo+ diameter + e, data = train_data, method = "class")
# Make predictions
dt_predictions <- predict(dt_model, newdata = test_data, type = "class")
test_conf <- factor(test_data$pha, levels = levels(dt_predictions))
levels(test_conf)
conf_matrix <- confusionMatrix(dt_predictions, test_conf)
conf_matrix
# Calculate accuracy
dt_accuracy <- sum(dt_predictions == test_data$pha) / nrow(test_data)
print(paste("Decision Tree Accuracy: ", dt_accuracy))
# recall
true_positives <- sum(dt_predictions == 1 & test_data$pha == 1)
false_negatives <- sum(dt_predictions == 0 & test_data$pha == 1)
dt_recall <- true_positives / (true_positives + false_negatives)
print(paste("Decision Tree Recall: ", dt_recall))

# Naive Bayes
library(e1071)
nb_model <- naiveBayes(pha ~ q + moid_ld + moid + neo+ diameter + e, data = train_data)
print(nb_model)
nb_predictions <- predict(nb_model, newdata = test_data)
# Calculate accuracy
nb_accuracy <- sum(nb_predictions == test_data$pha) / nrow(test_data)
print(paste("Decision Tree Accuracy: ", nb_accuracy))
# recall
true_positives <- sum(nb_predictions == 1 & test_data$pha == 1)
false_negatives <- sum(nb_predictions == 0 & test_data$pha == 1)
nb_recall <- true_positives / (true_positives + false_negatives)
print(paste("Naive Bayes Recall: ", nb_recall))

# SVM
library(e1071)
svm_model <- svm(pha ~ q + moid_ld + moid + neo+ diameter + e, data = train_data, type = 'C-classification', kernel = 'radial')
summary(svm_model)
# Make predictions
svm_predictions <- predict(svm_model, newdata = test_data)
# Calculate accuracy
svm_accuracy <- sum(svm_predictions == test_data$pha) / nrow(test_data)
print(paste("SVM Accuracy: ", svm_accuracy))
# recall
true_positives <- sum(svm_predictions == 1 & test_data$pha == 1)
false_negatives <- sum(svm_predictions == 0 & test_data$pha == 1)
svm_recall <- true_positives / (true_positives + false_negatives)
print(paste("SVM Recall: ", svm_recall))

lr_accuracy
lr_recall
dt_accuracy
dt_recall
nb_accuracy
nb_recall
svm_accuracy
svm_recall


