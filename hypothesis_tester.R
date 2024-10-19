# Load necessary library
library(dplyr)

# Function to perform a t-test
perform_t_test <- function(data, group_col, value_col) {

  # Ensure data is a dataframe
  if (!is.data.frame(data)) {
    stop("Input data must be a data frame.")
  }
  
  # Perform t-test
  test_result <- t.test(data[[value_col]] ~ data[[group_col]])
  
  # Display output
  cat("T-test Result:\n")
  print(test_result)
}

# Function to perform a chi-square test
perform_chi_square_test <- function(data, col1, col2) {

  # Create a contingency table
  contingency_table <- table(data[[col1]], data[[col2]])
  
  # Check for the warning and decide which test to run
  if (any(contingency_table < 5)) {
    cat("Expected frequencies are too low. Running Fisher's Exact Test instead.\n")
    test_result <- fisher.test(contingency_table)
  } else {
    # Perform chi-square test
    test_result <- chisq.test(contingency_table)
  }
  
  # Display output
  cat("Test Result:\n")
  print(test_result)
}


# Function to read data from CSV
read_data_from_csv <- function(file_path) {

  # Read the data
  data <- read.csv(file_path)
  return(data)
}

# Function to demonstrate data types and loops
analyze_data_types <- function(data) {

  # Create a list to store data types
  data_types <- list()
  
  # Loop through each column in the dataframe
  for (col in names(data)) {
    data_types[[col]] <- class(data[[col]])
  }
  
  # Display the data types
  cat("Data Types:\n")
  print(data_types)
}

# Main function to run the statistical tests
run_statistical_tests <- function(csv_file_path) {
  
  # Read the data
  data <- read_data_from_csv(csv_file_path)
  
  # Analyze data types
  analyze_data_types(data)
  
  # Perform t-test
  cat("\nPerforming T-test...\n")
  perform_t_test(data, "Group", "Value")
  
  # Perform chi-square test
  cat("\nPerforming Chi-Square Test...\n")
  perform_chi_square_test(data, "Category1", "Category2")
}

run_statistical_tests("example_data.csv")

run_statistical_tests("large_example_data.csv")
