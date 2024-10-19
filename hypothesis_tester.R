# Load necessary library
# This library is essential for data manipulation within the program
library(dplyr)

# Function to perform a t-test
# This function computes a t-test based on the specified value and group columns
perform_t_test <- function(data, group_col, value_col) {
  
  # Ensure the input is a data frame
  if (!is.data.frame(data)) {
    stop("Input data must be a data frame.")
  }
  
  # Check if the specified columns exist in the data frame
  if (!(group_col %in% names(data)) || !(value_col %in% names(data))) {
    stop("Specified columns do not exist in the data frame.")
  }
  
  # Perform the t-test
  test_result <- t.test(data[[value_col]] ~ data[[group_col]])
  
  # Display the output results
  cat("T-test Result:\n")
  print(test_result)
  
  # Return the result for further use if needed
  return(test_result)
}

# Function to perform a chi-square test
# This function checks the relationship between two categorical variables
perform_chi_square_test <- function(data, col1, col2) {
  
  # Create a contingency table from the two specified columns
  contingency_table <- table(data[[col1]], data[[col2]])
  
  # Display the contingency table
  cat("Contingency Table:\n")
  print(contingency_table)
  
  # Check for low expected frequencies
  if (any(contingency_table < 5)) {
    cat("Expected frequencies are too low. Running Fisher's Exact Test instead.\n")
    test_result <- fisher.test(contingency_table)
  } else {
    # Perform chi-square test if expected frequencies are adequate
    test_result <- chisq.test(contingency_table)
  }
  
  # Display the output results
  cat("Chi-Square Test Result:\n")
  print(test_result)
  
  # Return the result for further use if needed
  return(test_result)
}

# Function to read data from a CSV file
# This function takes a file path and returns the data as a data frame
read_data_from_csv <- function(file_path) {
  
  # Read the data from the specified CSV file
  data <- read.csv(file_path)
  
  # Display message indicating data loading completion
  cat("Data loaded from:", file_path, "\n")
  
  return(data)
}

# Function to demonstrate data types and iterate through columns
analyze_data_types <- function(data) {
  
  # Create a list to store the data types of each column
  data_types <- list()
  
  # Loop through each column in the dataframe
  for (col in names(data)) {
    # Store the class/type of each column
    data_types[[col]] <- class(data[[col]])
  }
  
  # Display the data types for user reference
  cat("Data Types:\n")
  print(data_types)
  
  # Return the data types for further analysis if needed
  return(data_types)
}

# Main function to run the statistical tests
# This function orchestrates reading data and running tests
run_statistical_tests <- function(csv_file_path) {
  
  # Read the data from the CSV file
  data <- read_data_from_csv(csv_file_path)
  
  # Analyze the data types of the columns in the dataframe
  analyze_data_types(data)
  
  # Perform the t-test on the data
  cat("\nPerforming T-test...\n")
  perform_t_test(data, "Group", "Value")
  
  # Perform the chi-square test on the data
  cat("\nPerforming Chi-Square Test...\n")
  perform_chi_square_test(data, "Category1", "Category2")
}

# Example of running the main function with specified CSV files
run_statistical_tests("example_data.csv")  # Small dataset example
run_statistical_tests("large_example_data.csv")  # Larger dataset example

# Additional Test Runs
# Uncomment to test with different datasets or scenarios
# run_statistical_tests("another_example_data.csv")
# run_statistical_tests("yet_another_large_data.csv")

# Summary message for the user
cat("\nStatistical tests completed for the provided datasets.\n")
