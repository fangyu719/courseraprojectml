library(readr)
library(caret)
library(randomForest)

# Data loading
train <- read_csv("/Users/fangyuliu/Downloads/pml-training.csv", na = c("", "NA", "#DIV/0!"), show_col_types = FALSE)
test <- read_csv("/Users/fangyuliu/Downloads/pml-testing.csv", na = c("", "NA", "#DIV/0!"), show_col_types = FALSE)

# Data splitting
set.seed(2139)
trainset <- createDataPartition(train$classe, p = 0.8, list = FALSE)
Training <- train[trainset, ]
Validation <- train[-trainset, ]

# Remove near-zero variance features
nonzerocol <- nearZeroVar(Training)
Training <- Training[, -nonzerocol]

# Remove descriptive and columns with excessive missing values
countlength <- colSums(!is.na(Training))
nullCol <- names(countlength[countlength < 0.6 * nrow(Training)])
descriptcol <- c("X", "user_name", "raw_timestamp_part_1", "raw_timestamp_part_2", 
                 "cvtd_timestamp", "new_window", "num_window")
excludecolumns <- c(descriptcol, nullCol)
Training <- Training[, !names(Training) %in% excludecolumns]

# Model training
rfModel <- randomForest(as.factor(classe) ~ ., data = Training, importance = TRUE, ntree = 10)

# Training set prediction
ptraining <- predict(rfModel, Training)
u1 <- union(ptraining, Training$classe)
t1 <- table(factor(ptraining, u1), factor(Training$classe, u1))
print(confusionMatrix(t1))

# Validation set prediction
pvalidation <- predict(rfModel, Validation)
u2 <- union(pvalidation, Validation$classe)
t2 <- table(factor(pvalidation, u2), factor(Validation$classe, u2))
print(confusionMatrix(t2))

# Test set prediction
ptest <- predict(rfModel, test)
ptest
