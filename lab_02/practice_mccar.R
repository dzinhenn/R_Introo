# =============================================================================
# R DATA CLEANING MINI PROJECT
# Dataset: mtcars
# Source: Built-in dataset in R
# Purpose: Demonstrate basic data cleaning workflow
# =============================================================================

# -----------------------------------------------------------------------------
# SECTION 1: INITIALIZATION
# -----------------------------------------------------------------------------

cat("============================================================\n")
cat("R DATA CLEANING PROJECT - MTCARS\n")
cat("============================================================\n\n")

# Copy dataset to new object
data_raw <- mtcars

# Check dimension
cat("Dataset dimension:\n")
print(dim(data_raw))

# Number of rows
cat("\nNumber of rows:\n")
print(nrow(data_raw))

# Number of columns
cat("\nNumber of columns:\n")
print(ncol(data_raw))

# -----------------------------------------------------------------------------
# SECTION 2: BASIC DATA EXPLORATION
# -----------------------------------------------------------------------------

cat("\n============================================================\n")
cat("STEP 1: DATA EXPLORATION\n")
cat("============================================================\n")

# Display first observations
cat("\nPreview data:\n")
print(head(data_raw))

# Display last rows
cat("\nLast rows:\n")
print(tail(data_raw))

# Structure of dataset
cat("\nStructure:\n")
str(data_raw)

# Summary statistics
cat("\nSummary statistics:\n")
summary(data_raw)

# Check column names
cat("\nColumn names:\n")
print(colnames(data_raw))

# -----------------------------------------------------------------------------
# SECTION 3: CREATE WORKING COPY
# -----------------------------------------------------------------------------

cat("\n============================================================\n")
cat("STEP 2: CREATE WORKING DATASET\n")
cat("============================================================\n")

# Duplicate dataset
cars <- data_raw

cat("\nDataset copied successfully\n")

# Verify copy
print(dim(cars))

# -----------------------------------------------------------------------------
# SECTION 4: SIMULATE MISSING VALUES
# -----------------------------------------------------------------------------

cat("\n============================================================\n")
cat("STEP 3: SIMULATE MISSING DATA\n")
cat("============================================================\n")

# Set seed for reproducibility
set.seed(123)

# Introduce NA values
cars$mpg[c(4,11)] <- NA
cars$hp[7] <- NA
cars$wt[c(16,21)] <- NA
cars$qsec[24] <- NA

cat("\nMissing values added\n")

# -----------------------------------------------------------------------------
# SECTION 5: DETECT MISSING DATA
# -----------------------------------------------------------------------------

cat("\n============================================================\n")
cat("STEP 4: DETECT MISSING VALUES\n")
cat("============================================================\n")

# Count NA
na_total <- sum(is.na(cars))

cat("\nTotal NA values:\n")
print(na_total)

# NA by column
cat("\nNA count by column:\n")
na_by_col <- colSums(is.na(cars))
print(na_by_col)

# Rows containing NA
cat("\nRows containing NA:\n")
rows_with_na <- cars[!complete.cases(cars),]
print(rows_with_na)

# -----------------------------------------------------------------------------
# SECTION 6: HANDLE MISSING DATA
# -----------------------------------------------------------------------------

cat("\n============================================================\n")
cat("STEP 5: HANDLE MISSING DATA\n")
cat("============================================================\n")

# Median imputation
median_mpg <- median(cars$mpg, na.rm=TRUE)
median_hp <- median(cars$hp, na.rm=TRUE)
median_wt <- median(cars$wt, na.rm=TRUE)
median_qsec <- median(cars$qsec, na.rm=TRUE)

# Replace NA values
cars$mpg[is.na(cars$mpg)] <- median_mpg
cars$hp[is.na(cars$hp)] <- median_hp
cars$wt[is.na(cars$wt)] <- median_wt
cars$qsec[is.na(cars$qsec)] <- median_qsec

cat("\nMissing values replaced using median\n")

# Verify NA removed
cat("\nRemaining NA values:\n")
print(sum(is.na(cars)))

# -----------------------------------------------------------------------------
# SECTION 7: CONVERT CATEGORICAL VARIABLES
# -----------------------------------------------------------------------------

cat("\n============================================================\n")
cat("STEP 6: CONVERT VARIABLES TO FACTOR\n")
cat("============================================================\n")

# Convert cylinders
cars$cyl <- factor(
  cars$cyl,
  levels=c(4,6,8),
  labels=c("4 cylinders","6 cylinders","8 cylinders"),
  ordered=TRUE)

# Convert gears
cars$gear <- factor(
  cars$gear,
  levels=c(3,4,5),
  labels=c("3 gears","4 gears","5 gears"),
  ordered=TRUE)

# Convert carburetors
cars$carb <- factor(
  cars$carb,
  levels=c(1,2,3,4,6,8),
  labels=c("1","2","3","4","6","8"),
  ordered=TRUE)

# Convert engine type
cars$vs <- factor(
  cars$vs,
  levels=c(0,1),
  labels=c("V engine","Straight engine")
)

# Convert transmission
cars$am <- factor(
  cars$am,
  levels=c(0,1),
  labels=c("Automatic","Manual")
)

cat("\nFactor conversion completed\n")

# -----------------------------------------------------------------------------
# SECTION 8: VERIFY DATA TYPES
# -----------------------------------------------------------------------------

cat("\n============================================================\n")
cat("STEP 7: VERIFY DATA STRUCTURE\n")
cat("============================================================\n")

str(cars)

cat("\nNumber numeric variables:\n")
print(sum(sapply(cars,is.numeric)))

cat("\nNumber factor variables:\n")
print(sum(sapply(cars,is.factor)))

# -----------------------------------------------------------------------------
# SECTION 9: FEATURE ENGINEERING
# -----------------------------------------------------------------------------

cat("\n============================================================\n")
cat("STEP 8: CREATE NEW FEATURES\n")
cat("============================================================\n")

# Fuel efficiency category
cars$efficiency <- cut(
  cars$mpg,
  breaks=c(0,15,25,Inf),
  labels=c("Low","Medium","High"),
  ordered_result=TRUE)

# Horsepower category
cars$power <- cut(
  cars$hp,
  breaks=3,
  labels=c("Low","Medium","High"),
  ordered_result=TRUE)

cat("\nNew variables created\n")

# Distribution
cat("\nEfficiency distribution:\n")
print(table(cars$efficiency))

cat("\nPower distribution:\n")
print(table(cars$power))

# -----------------------------------------------------------------------------
# SECTION 10: DESCRIPTIVE ANALYSIS
# -----------------------------------------------------------------------------

cat("\n============================================================\n")
cat("STEP 9: BASIC ANALYSIS\n")
cat("============================================================\n")

# Numeric columns
num_vars <- c("mpg","disp","hp","drat","wt","qsec")

# Correlation matrix
cat("\nCorrelation matrix:\n")
corr_matrix <- cor(cars[,num_vars])
print(round(corr_matrix,2))

# Average mpg by cylinder
cat("\nAverage MPG by cylinders:\n")
avg_mpg_cyl <- tapply(cars$mpg,cars$cyl,mean)
print(round(avg_mpg_cyl,2))

# Average horsepower by transmission
cat("\nAverage horsepower by transmission:\n")
avg_hp_am <- tapply(cars$hp,cars$am,mean)
print(round(avg_hp_am,2))

# -----------------------------------------------------------------------------
# SECTION 11: FINAL DATA CHECK
# -----------------------------------------------------------------------------

cat("\n============================================================\n")
cat("STEP 10: FINAL QUALITY CHECK\n")
cat("============================================================\n")

# Structure
str(cars)

# Summary
summary(cars)

# NA check
cat("\nFinal NA count:\n")
print(sum(is.na(cars)))

# Dataset info
cat("\nDataset information:\n")
cat("Rows:",nrow(cars),"\n")
cat("Columns:",ncol(cars),"\n")

# -----------------------------------------------------------------------------
# SECTION 12: SAVE CLEAN DATA
# -----------------------------------------------------------------------------

cat("\n============================================================\n")
cat("STEP 11: EXPORT CLEAN DATA\n")
cat("============================================================\n")

# File path
output_path <- "mtcars_cleaned.csv"

# Save CSV
write.csv(cars,output_path,row.names=TRUE)

cat("\nDataset exported successfully\n")
cat("File name:",output_path,"\n")

# -----------------------------------------------------------------------------
# SECTION 13: COMPLETION MESSAGE
# -----------------------------------------------------------------------------

cat("\n============================================================\n")
cat("DATA CLEANING PROCESS COMPLETED\n")
cat("============================================================\n")

cat("\nDataset is ready for:\n")
cat("1. Visualization\n")
cat("2. Statistical analysis\n")
cat("3. Regression models\n")
cat("4. Machine learning\n")
cat("5. Predictive analytics\n")

