#############################
#EXERCISE
#############################

# Create a data frame with missing values
data<- data.frame(
  ID = 1:10,
  Name = c("Alice", "Bob", "Charlie", "David", "Eva", "Frank", "Grace",
           "Hannah", "Ian", "Jack"),
  Age = c(25, 30, NA, 45, 35, NA, 29, 40, 33, NA),
  Salary = c(50000, 60000, NA, NA, 70000, 72000, NA, 65000, 56000, 59000),
  Gender = c("F", "M", "M", "M", "F", "M", "F", "F", "M", "M")
)
data

print(data)



# Load required library
library(devtools)

#1: Function to detect missing values in a dataset
detect_missing <- function(data) {
  return(sapply(data, function(x) sum(is.na(x))))
}

#2: Compute the median for each variable
compute_median <- function(data) {
  sapply(data, function(x) if (is.numeric(x)) median(x, na.rm = TRUE) else NA)
}

#3: Replace missing values with median values
replace_with_median <- function(data) {
  medians <- compute_median(data)
  data[] <- lapply(seq_along(data), function(i) {
    if (is.numeric(data[[i]])) {
      ifelse(is.na(data[[i]]), medians[i], data[[i]])
    } else {
      data[[i]]
    }
  })
  return(as.data.frame(data))
}

#4: Apply functions and print the dataset without missing values
missing_counts <- detect_missing(data_with_na)
print("Missing values in the dataset:")
print(missing_counts)

medians <- compute_median(data_with_na)
print("Median for each variable:")
print(medians)

data_clean <- replace_with_median(data_with_na)
print("Dataset without missing values:")
print(data_clean)
