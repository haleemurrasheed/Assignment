#setwd("")

#Part A

subj_test <- read.table("test/subject_test.txt")
subj_train <- read.table("train/subject_train.txt")
subject <- rbind(subj_test, subj_train)

x_test <- read.table("test/X_test.txt")
x_train <- read.table("train/X_train.txt")
X <- rbind(x_test, x_train)

y_test <- read.table("test/y_test.txt")
y_train <- read.table("train/y_train.txt")
Y <- rbind(y_test, y_train)



#Part B

feature <- read.table("features.txt")
m_s_X <- grep("-mean\\(\\)|-std\\(\\)", feature[, 2])
X <- X[, m_s_X]
names(X) <- feature[m_s_X, 2]
names(X) <- gsub("\\(|\\)", "", names(X))
names(X) <- tolower(names(X))


#Part C

activ <- read.table("activity_labels.txt")
activ[, 2] = gsub("_", "", tolower(as.character(activ[, 2])))
Y[,1] = activ[Y[,1], 2]
names(Y) <- "activity"


#Part D

names(subject) <- "subject"
clean_data <- cbind(subject, X, Y)
write.table(clean_data, "data_clean.txt")

#Part E

unique_subject = unique(subject)[,1]
num_subject = length(unique(subject)[,1])
num_activity = length(activ[,1])
num_cols = dim(clean_data)[2]
result = clean_data[1:(num_subject*num_activity), ]

row = 1
for (subject in 1:num_subject) {
  for (i in 1:num_activity) {
    result[row, 1] = unique_subject[subject]
    result[row, 2] = activ[i, 2]
    pal <- clean_data[clean_data$subject & clean_data$activ[i, 2], ]
    result[row, 3:num_cols] <- colMeans(pal[, 3:num_cols])
    row = row+1
  }
}
write.table(result, "averages_data_set.txt")
