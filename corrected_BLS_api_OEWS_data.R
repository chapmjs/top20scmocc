# ==========================================
# CORRECTED BLS API APPROACH FOR OEWS DATA
# Uses proper series ID format and handles API limitations
# ==========================================

library(blsAPI)
library(tidyverse)
library(jsonlite)

# Load API key from config file
source("config.R")

# Verify API key
if(Sys.getenv("BLS_KEY") == "") {
  stop("BLS API key not found. Please check your config.R file.")
}

# Correct OEWS Series ID construction based on BLS documentation
# Format: OE + U + area_code + industry_code + occupation_code + datatype_code
construct_oews_series_ids <- function(occupation_code) {
  
  # Clean the occupation code (remove dashes, pad to 6 digits)
  clean_code <- sprintf("%06s", gsub("-", "", occupation_code))
  
  # OEWS Series ID structure:
  # OE = Survey abbreviation (Occupational Employment)
  # U = Seasonal adjustment (U = not seasonally adjusted)
  # 0000000 = Area code (7 digits, 0000000 = National)  
  # 000000 = Industry code (6 digits, 000000 = Cross-industry)
  # XXXXXX = Occupation code (6 digits, your SOC code)
  # XX = Data type code (01=Employment, 04=Mean wage, 13=Median wage)
  
  base_id <- paste0("OEU0000000000000", clean_code)
  series_ids <- paste0(base_id, c("01", "04", "13"))
  names(series_ids) <- c("employment", "mean_wage", "median_wage")
  
  cat("Generated series IDs for occupation", occupation_code, ":\n")
  cat("  Employment:", series_ids[1], "\n")
  cat("  Mean wage:", series_ids[2], "\n") 
  cat("  Median wage:", series_ids[3], "\n\n")
  
  return(series_ids)
}

# Test with a single occupation first
test_single_occupation <- function(occupation_code = "11-1011", years = 2020:2024) {
  
  cat("Testing single occupation:", occupation_code, "\n")
  series_ids <- construct_oews_series_ids(occupation_code)
  
  # API payload
  payload <- list(
    'seriesid' = as.vector(series_ids),
    'startyear' = as.character(min(years)),
    'endyear' = as.character(max(years)),
    'registrationKey' = Sys.getenv("BLS_KEY")
  )
  
  tryCatch({
    response <- blsAPI(payload, api_version = 2)
    json_data <- fromJSON(response)
    
    cat("API Status:", json_data$status, "\n")
    if(!is.null(json_data$message)) {
      cat("Message:", json_data$message, "\n")
    }
    
    if(json_data$status == "REQUEST_SUCCEEDED") {
      if(!is.null(json_data$Results$series)) {
        cat("✓ Success! Received data for", nrow(json_data$Results$series), "series\n")
        
        # Check if we got actual data points
        for(i in 1:nrow(json_data$Results$series)) {
          series_info <- json_data$Results$series[i,]
          if(!is.null(series_info$data) && is.data.frame(series_info$data[[1]])) {
            data_points <- nrow(series_info$data[[1]])
            cat("  Series", series_info$seriesID, "has", data_points, "data points\n")
          }
        }
        return(json_data)
      } else {
        cat("⚠ Success but no series data returned\n")
      }
    } else {
      cat("❌ API request failed\n")
    }
    
    return(NULL)
    
  }, error = function(e) {
    cat("❌ Error:", e$message, "\n")
    return(NULL)
  })
}

# Alternative: Try using the current year's data endpoint
get_current_oews_data <- function(occupation_codes) {
  
  # For current year data, try the standard format used in your original code
  all_results <- list()
  
  for(i in seq_along(occupation_codes)) {
    code <- names(occupation_codes)[i]
    name <- occupation_codes[[code]]
    
    cat(sprintf("(%d/%d) Testing %s - %s\n", i, length(occupation_codes), code, name))
    
    # Try both formats
    formats_to_try <- list(
      # Your original format
      original = paste0("OEUN0000000000000", sprintf("%06s", gsub("-", "", code)), c("01", "04", "13")),
      # Corrected format  
      corrected = paste0("OEU0000000000000", sprintf("%06s", gsub("-", "", code)), c("01", "04", "13"))
    )
    
    for(format_name in names(formats_to_try)) {
      series_ids <- formats_to_try[[format_name]]
      
      payload <- list(
        'seriesid' = series_ids,
        'startyear' = '2024',
        'endyear' = '2024', 
        'registrationKey' = Sys.getenv("BLS_KEY")
      )
      
      tryCatch({
        response <- blsAPI(payload, api_version = 2)
        json_data <- fromJSON(response)
        
        if(json_data$status == "REQUEST_SUCCEEDED" && 
           !is.null(json_data$Results$series) && 
           nrow(json_data$Results$series) > 0) {
          
          cat("  ✓", format_name, "format worked!\n")
          all_results[[code]] <- list(
            code = code,
            name = name,
            format = format_name,
            data = json_data
          )
          break  # Stop trying other formats
        }
      }, error = function(e) {
        # Continue to next format
      })
    }
    
    if(!code %in% names(all_results)) {
      cat("  ❌ No format worked for", code, "\n")
    }
    
    Sys.sleep(0.5)  # Rate limiting
  }
  
  return(all_results)
}

# Main testing function
cat("🔍 TESTING BLS OEWS API ACCESS\n")
cat(paste(rep("=", 50), collapse = ""), "\n")

# Test SCM occupation codes
scm_test_codes <- list(
  "11-1011" = "Chief Executives",
  "11-3061" = "Purchasing Managers", 
  "11-3071" = "Transportation, Storage, and Distribution Managers",
  "13-1081" = "Logisticians",
  "13-1023" = "Purchasing Agents"
)

cat("Phase 1: Testing single occupation with multiple years...\n")
test_result <- test_single_occupation("11-1011", 2020:2024)

cat("\nPhase 2: Testing multiple occupations with current year...\n")
current_results <- get_current_oews_data(scm_test_codes)

cat("\n", paste(rep("=", 50), collapse = ""), "\n")
cat("TEST RESULTS SUMMARY\n")
cat(paste(rep("=", 50), collapse = ""), "\n")

if(length(current_results) > 0) {
  cat("✅ Successfully retrieved data for", length(current_results), "occupations:\n")
  for(code in names(current_results)) {
    result <- current_results[[code]]
    cat("  -", result$name, "(", result$format, "format )\n")
  }
  
  cat("\n💡 RECOMMENDATIONS:\n")
  cat("1. The API is working, but historical data availability is limited\n")
  cat("2. Consider using the Excel file download approach for 10-year trends\n") 
  cat("3. Use API for current year data only\n")
  cat("4. Check if your occupation codes match BLS SOC codes exactly\n")
  
} else {
  cat("❌ No successful API calls\n")
  cat("\n💡 LIKELY ISSUES:\n")
  cat("1. OEWS data may not be available as continuous time series via API\n")
  cat("2. Series ID format may have changed\n")
  cat("3. Historical OEWS data may only be available as annual snapshots\n")
  cat("4. Use the Excel download approach instead\n")
}