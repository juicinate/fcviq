# Utilities required for the FCVIQ application -----
# 
# Version 1.2
# Updated 10.05.2025
#
# (C) 2025 Joseph Corazolla
# 
# Licensed under AGPL-v3
#

# Function to calculate age -----
#
# Example usage
# age <- test_age(as.Date("2019-09-10"))
# print(age)
# 
# Should work with shiny::dateInput() 

calculate_age <- function(birthdate, testdate = Sys.time()) {
  # Validate input
  if (!inherits(birthdate, "Date")) {
    stop("birthdate must be a Date object.")
  }
  if (!inherits(testdate, "Date")) {
    stop("testdate must be a Date object.")
  }

  # Extract year, month, and day from birthdate and testdate
  y <- as.integer(format(birthdate, "%Y"))
  m <- as.integer(format(birthdate, "%m"))
  d <- as.integer(format(birthdate, "%d"))

  ynow <- as.integer(format(testdate, "%Y"))
  mnow <- as.integer(format(testdate, "%m"))
  dnow <- as.integer(format(testdate, "%d"))

  # Calculate total months for birthdate and current date
  total_months_birth <- y * 12 + m - 1
  total_months_now <- ynow * 12 + mnow - 1
  delta_months <- total_months_now - total_months_birth

  # Calculate age components
  if (dnow >= d) {
    return(list(
      years = floor(delta_months / 12),
      months = delta_months %% 12,
      days = dnow - d
    ))
  }

  # Adjust for the case where the current day is less than the birth day
  delta_months <- delta_months - 1
  total_months_now <- total_months_now - 1

  # Calculate last birthday
  last_birthday_year <- ynow - 1
  if (mnow > m || (mnow == m && dnow > d)) {
    last_birthday_year <- ynow # Last birthday is this year
  }

  # Calculate days until the next birthday
  last_birthday <- as.Date(paste(last_birthday_year, m, d, sep = "-"))
  days_since_birthday <- as.integer(difftime(testdate, last_birthday, units = "days"))

  # Return results as a list containing $years $months and $days of age.
  return(list(
    years = floor(delta_months / 12),
    months = delta_months %% 12,
    days = days_since_birthday
  ))
}

# Function to generate a temporary directory and load the report template ------
# Returns a temporary directory containing the defined template as "template.qmd".
prepare_report <- function() {
  # Define which file should be the template
  template_file <- "fcviq_report.qmd"
  
  # Create a temporary directory
  tempDir <- tempdir()
  tempReport <- file.path(tempDir, "template.qmd")
  
  # Copy the report file to the temporary directory if you have a template
  file.copy(template_file, tempReport, overwrite = TRUE)
  
return(tempDir)
}
