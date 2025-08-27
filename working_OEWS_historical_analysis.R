# ==========================================
# WORKING OEWS HISTORICAL ANALYSIS
# Downloads actual BLS OEWS files (the only way that works)
# ==========================================

library(tidyverse)
library(httr)
library(openxlsx)
library(scales)
library(DT)
library(htmlwidgets)

# ==========================================
# CONFIGURATION
# ==========================================

# SCM Occupation codes (6-digit format without dashes)
scm_occupations <- list(
  "111011" = "Chief Executives",
  "113061" = "Purchasing Managers", 
  "113071" = "Transportation, Storage, and Distribution Managers",
  "119199" = "Managers, All Other",
  "131081" = "Logisticians",
  "131023" = "Purchasing Agents, Except Wholesale, Retail, and Farm Products",
  "131022" = "Wholesale and Retail Buyers, Except Farm Products",
  "131199" = "Business Operations Specialists, All Other", 
  "131111" = "Management Analysts",
  "152031" = "Operations Research Analysts",
  "172112" = "Industrial Engineers",
  "435011" = "Cargo and Freight Agents",
  "435061" = "Production, Planning, and Expediting Clerks",
  "435071" = "Shipping, Receiving, and Traffic Clerks",
  "531047" = "Traffic Technicians"
)

# Years to analyze (BLS has data back to 1997, but let's focus on recent decade)
years_to_analyze <- 2014:2024
current_year <- 2024

# ==========================================
# OEWS FILE DOWNLOAD FUNCTIONS
# ==========================================

# BLS OEWS file URLs (these are the actual files BLS publishes)
get_oews_url <- function(year) {
  if(year >= 2012) {
    # Format: https://www.bls.gov/oes/special-requests/oesm[YY]nat.zip
    year_short <- substr(as.character(year), 3, 4)
    return(paste0("https://www.bls.gov/oes/special-requests/oesm", year_short, "nat.zip"))
  } else {
    return(NULL)
  }
}

# Download and process OEWS data for a single year
get_year_data <- function(year, occupations, temp_dir = tempdir()) {
  
  cat(sprintf("Processing %d... ", year))
  
  url <- get_oews_url(year)
  if(is.null(url)) {
    cat("❌ No URL available\n")
    return(NULL)
  }
  
  zip_file <- file.path(temp_dir, paste0("oews_", year, ".zip"))
  extract_dir <- file.path(temp_dir, paste0("oews_", year))
  
  tryCatch({
    # Download the ZIP file
    download.file(url, zip_file, mode = "wb", quiet = TRUE)
    
    # Extract it
    unzip(zip_file, exdir = extract_dir, overwrite = TRUE)
    
    # Find the Excel file (usually the largest .xlsx file)
    excel_files <- list.files(extract_dir, pattern = "\\.xlsx$", full.names = TRUE)
    
    if(length(excel_files) == 0) {
      cat("❌ No Excel file found\n")
      return(NULL)
    }
    
    excel_file <- excel_files[1]  # Take the first (usually only) Excel file
    
    # Read the Excel file
    sheets <- getSheetNames(excel_file)
    
    # Find the main data sheet (usually contains "National" and "dl")
    main_sheet <- sheets[grepl("National.*dl|nat.*dl", sheets, ignore.case = TRUE)][1]
    if(is.na(main_sheet)) {
      main_sheet <- sheets[1]  # Fallback to first sheet
    }
    
    # Read the data
    data <- read.xlsx(excel_file, sheet = main_sheet, startRow = 1)
    
    # Standardize column names (they vary slightly by year)
    names(data) <- tolower(names(data))
    names(data) <- gsub("[^a-z0-9]", "_", names(data))
    
    # Find the occupation code column (varies by year)
    occ_code_col <- names(data)[grepl("occ.*code|occupation.*code", names(data))][1]
    if(is.na(occ_code_col)) {
      # Try other patterns
      occ_code_col <- names(data)[grepl("^occ$|^code$", names(data))][1]
    }
    
    if(is.na(occ_code_col)) {
      cat("❌ Cannot find occupation code column\n")
      return(NULL)
    }
    
    # Find other important columns
    title_col <- names(data)[grepl("title|name", names(data))][1]
    emp_col <- names(data)[grepl("tot_emp|employment", names(data))][1] 
    median_col <- names(data)[grepl("a_median|median", names(data))][1]
    mean_col <- names(data)[grepl("a_mean|mean", names(data))][1]
    
    # Filter for our SCM occupations
    scm_data <- data %>%
      filter(.data[[occ_code_col]] %in% names(occupations)) %>%
      select(
        occupation_code = all_of(occ_code_col),
        occupation_title = if(!is.na(title_col)) all_of(title_col) else occupation_code,
        employment = if(!is.na(emp_col)) all_of(emp_col) else NA,
        median_wage = if(!is.na(median_col)) all_of(median_col) else NA,
        mean_wage = if(!is.na(mean_col)) all_of(mean_col) else NA
      ) %>%
      mutate(
        year = year,
        # Add occupation names from our list
        occupation_name = map_chr(occupation_code, ~occupations[[.x]] %||% "Unknown"),
        # Clean numeric columns
        employment = as.numeric(employment),
        median_wage = as.numeric(median_wage),
        mean_wage = as.numeric(mean_wage)
      ) %>%
      # Remove rows with all NA wages
      filter(!is.na(median_wage) | !is.na(mean_wage))
    
    # Clean up files
    unlink(zip_file)
    unlink(extract_dir, recursive = TRUE)
    
    if(nrow(scm_data) > 0) {
      cat(sprintf("✅ %d SCM occupations\n", nrow(scm_data)))
      return(scm_data)
    } else {
      cat("⚠️ No SCM data found\n")
      return(NULL)
    }
    
  }, error = function(e) {
    cat(sprintf("❌ Error: %s\n", e$message))
    return(NULL)
  })
}

# ==========================================
# MAIN DATA COLLECTION
# ==========================================

collect_historical_data <- function(years, occupations) {
  
  cat("🔍 COLLECTING OEWS HISTORICAL DATA\n")
  cat(paste(rep("=", 50), collapse = ""), "\n")
  cat("Years:", paste(years, collapse = ", "), "\n")
  cat("Occupations:", length(occupations), "\n\n")
  
  all_data <- list()
  successful_years <- 0
  
  for(year in years) {
    year_data <- get_year_data(year, occupations)
    
    if(!is.null(year_data) && nrow(year_data) > 0) {
      all_data[[as.character(year)]] <- year_data
      successful_years <- successful_years + 1
    }
    
    # Be respectful to BLS servers
    Sys.sleep(1)
  }
  
  if(length(all_data) > 0) {
    # Combine all years
    combined_data <- bind_rows(all_data)
    
    cat("\n✅ SUCCESS!\n")
    cat("Years with data:", successful_years, "/", length(years), "\n")
    cat("Total records:", nrow(combined_data), "\n")
    cat("Occupations found:", length(unique(combined_data$occupation_code)), "\n")
    
    return(combined_data)
  } else {
    cat("\n❌ No data collected\n")
    return(NULL)
  }
}

# ==========================================
# ANALYSIS FUNCTIONS
# ==========================================

create_top_20_analysis <- function(data) {
  
  cat("\n📊 CREATING TOP 20 ANALYSIS\n")
  cat(paste(rep("=", 50), collapse = ""), "\n")
  
  # Get current year rankings
  current_year_data <- data %>%
    filter(year == max(year, na.rm = TRUE)) %>%
    arrange(desc(median_wage)) %>%
    mutate(rank = row_number()) %>%
    head(20)
  
  cat("Top 20 SCM Positions (", max(data$year, na.rm = TRUE), "):\n")
  cat(paste(rep("-", 50), collapse = ""), "\n")
  
  for(i in 1:nrow(current_year_data)) {
    pos <- current_year_data[i, ]
    cat(sprintf("%2d. %s\n", i, pos$occupation_name))
    cat(sprintf("    Median: %s | Mean: %s | Employment: %s\n",
                dollar(pos$median_wage), 
                dollar(pos$mean_wage %||% 0), 
                comma(pos$employment %||% 0)))
    cat("\n")
  }
  
  return(current_year_data)
}

create_trend_analysis <- function(data, top_20) {
  
  cat("📈 CREATING 10-YEAR TREND ANALYSIS\n")
  cat(paste(rep("=", 50), collapse = ""), "\n")
  
  top_20_codes <- top_20$occupation_code
  
  trend_data <- data %>%
    filter(occupation_code %in% top_20_codes) %>%
    group_by(occupation_code, occupation_name) %>%
    summarise(
      years_with_data = n(),
      earliest_year = min(year, na.rm = TRUE),
      latest_year = max(year, na.rm = TRUE),
      earliest_median = first(median_wage[order(year)], na_rm = TRUE),
      latest_median = last(median_wage[order(year)], na_rm = TRUE),
      total_change = latest_median - earliest_median,
      percent_change = ifelse(earliest_median > 0, 
                              ((latest_median / earliest_median) - 1) * 100, NA),
      annual_growth = ifelse(earliest_year < latest_year,
                             (((latest_median / earliest_median)^(1/(latest_year - earliest_year))) - 1) * 100, 
                             NA),
      .groups = 'drop'
    ) %>%
    left_join(top_20 %>% select(occupation_code, rank), by = "occupation_code") %>%
    arrange(rank)
  
  cat("10-Year Salary Trends:\n")
  cat(paste(rep("-", 50), collapse = ""), "\n")
  
  for(i in 1:nrow(trend_data)) {
    trend <- trend_data[i, ]
    cat(sprintf("%2d. %s\n", trend$rank, trend$occupation_name))
    cat(sprintf("    %d-%d: %s → %s (%+.1f%%)\n",
                trend$earliest_year, trend$latest_year,
                dollar(trend$earliest_median %||% 0), 
                dollar(trend$latest_median %||% 0),
                trend$percent_change %||% 0))
    cat(sprintf("    Annual growth: %.1f%% | Years of data: %d\n\n",
                trend$annual_growth %||% 0, trend$years_with_data))
  }
  
  return(trend_data)
}

# ==========================================
# EXECUTE ANALYSIS
# ==========================================

cat("🚀 STARTING SCM SALARY ANALYSIS\n")
cat("Using BLS OEWS files (the method that actually works!)\n")
cat(paste(rep("=", 60), collapse = ""), "\n")

# Step 1: Collect all historical data
historical_data <- collect_historical_data(years_to_analyze, scm_occupations)

if(!is.null(historical_data)) {
  
  # Step 2: Create top 20 ranking
  top_20_positions <- create_top_20_analysis(historical_data)
  
  # Step 3: Analyze trends
  trend_analysis <- create_trend_analysis(historical_data, top_20_positions)
  
  # Step 4: Save results
  if(!dir.exists("output")) dir.create("output")
  
  write_csv(historical_data, "output/scm_complete_historical_data.csv")
  write_csv(top_20_positions, "output/scm_top_20_positions_2024.csv") 
  write_csv(trend_analysis, "output/scm_10_year_trends.csv")
  
  # Create summary
  cat("\n", paste(rep("=", 60), collapse = ""), "\n")
  cat("📁 FILES CREATED:\n")
  cat("  • scm_complete_historical_data.csv - All data (", nrow(historical_data), " records)\n")
  cat("  • scm_top_20_positions_2024.csv - Current rankings\n")
  cat("  • scm_10_year_trends.csv - Historical trends\n")
  
  cat("\n🎯 KEY INSIGHTS:\n")
  cat("  • Highest paying SCM job:", top_20_positions$occupation_name[1], "\n")
  cat("  • Top salary:", dollar(max(top_20_positions$median_wage, na.rm = TRUE)), "\n")
  
  if(nrow(trend_analysis) > 0) {
    best_growth <- trend_analysis[which.max(trend_analysis$percent_change), ]
    cat("  • Best 10-year growth:", best_growth$occupation_name, 
        " (", sprintf("%.1f%%", best_growth$percent_change), ")\n")
  }
  
  cat("  • Total SCM employment:", comma(sum(top_20_positions$employment, na.rm = TRUE)), "\n")
  cat("  • Years analyzed:", paste(range(historical_data$year, na.rm = TRUE), collapse = "-"), "\n")
  
  cat("\n✅ ANALYSIS COMPLETE! 🎉\n")
  
} else {
  cat("\n❌ ANALYSIS FAILED - No data could be retrieved\n")
  cat("Check your internet connection and try again.\n")
}