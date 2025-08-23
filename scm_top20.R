# ==========================================
# ENHANCED SCM SALARY ANALYSIS WITH 10-YEAR TRENDS
# Top 20 SCM Positions + Historical Analysis
# ==========================================

# Load required libraries
library(blsAPI)
library(tidyverse)
library(jsonlite)
library(scales)
library(openxlsx)
library(plotly)
library(DT)
library(htmlwidgets)

# Load API key from config file
#source("config.R")

# Verify API key
if(Sys.getenv("BLS_KEY") == "") {
  stop("BLS API key not found. Please check your config.R file.")
}

# ==========================================
# EXPANDED CONFIGURATION FOR TOP 20 ANALYSIS
# ==========================================

# Expanded list of SCM occupations to ensure we get top 20
scm_occupations_expanded <- list(
  # ========== SENIOR MANAGEMENT ==========
  "11-1011" = "Chief Executives (including Supply Chain VPs)",
  "11-3061" = "Purchasing Managers",
  "11-3071" = "Transportation, Storage, and Distribution Managers",
  "11-9199" = "Managers, All Other (includes Operations Managers)",
  "11-9041" = "Architectural and Engineering Managers",
  "11-2032" = "Public Relations Managers",
  
  # ========== CORE SCM PROFESSIONAL/ANALYTICAL ==========
  "13-1081" = "Logisticians",
  "13-1023" = "Purchasing Agents, Except Wholesale, Retail, and Farm Products",
  "13-1022" = "Wholesale and Retail Buyers, Except Farm Products",
  "13-1199" = "Business Operations Specialists, All Other (includes Supply Chain Analysts)",
  "13-1111" = "Management Analysts (often work on supply chain optimization)",
  "13-1121" = "Meeting, Convention, and Event Planners",
  "13-1131" = "Fundraisers",
  "13-1141" = "Compensation, Benefits, and Job Analysis Specialists",
  "13-1151" = "Training and Development Specialists",
  
  # ========== TECHNICAL/ENGINEERING ROLES ==========
  "15-2031" = "Operations Research Analysts",
  "15-1299" = "Computer Occupations, All Other",
  "17-2112" = "Industrial Engineers",
  "17-2141" = "Mechanical Engineers",
  "17-1011" = "Architects, Except Landscape and Naval",
  
  # ========== SPECIALIZED SCM ROLES ==========
  "13-2051" = "Financial Analysts",
  "13-2052" = "Personal Financial Advisors",
  "25-1011" = "Business Teachers, Postsecondary",
  "19-3051" = "Urban and Regional Planners",
  
  # ========== OPERATIONAL/SUPPORT ROLES ==========
  "43-5011" = "Cargo and Freight Agents",
  "43-5061" = "Production, Planning, and Expediting Clerks",
  "43-5071" = "Shipping, Receiving, and Traffic Clerks",
  "53-1047" = "Traffic Technicians",
  "43-1011" = "First-Line Supervisors of Office and Administrative Support Workers",
  "43-4051" = "Customer Service Representatives",
  
  # ========== ADDITIONAL SCM-RELATED ==========
  "13-1021" = "Buyers and Purchasing Agents, Farm Products",
  "43-5021" = "Couriers and Messengers",
  "43-5052" = "Postal Service Mail Carriers",
  "53-7064" = "Packers and Packagers, Hand",
  "53-7065" = "Stockers and Order Fillers",
  "53-3032" = "Heavy and Tractor-Trailer Truck Drivers"
)

# Define years for 10-year analysis
current_year <- 2024
historical_years <- seq(current_year - 10, current_year, by = 1)
cat("Analyzing years:", paste(historical_years, collapse = ", "), "\n")

# ==========================================
# ENHANCED FUNCTIONS
# ==========================================

# Function to construct BLS OEWS series IDs
construct_series_ids <- function(occupation_code) {
  clean_code <- sprintf("%06s", gsub("-", "", occupation_code))
  base_id <- paste0("OEUN0000000000000", clean_code)
  series_ids <- paste0(base_id, c("01", "04", "13"))
  names(series_ids) <- c("employment", "mean_wage", "median_wage")
  return(series_ids)
}

# Enhanced function to get historical data for multiple years
get_historical_occupation_data <- function(occupation_code, years, max_retries = 3) {
  series_ids <- construct_series_ids(occupation_code)
  
  # API payload for multiple years
  payload <- list(
    'seriesid' = as.vector(series_ids),
    'startyear' = as.character(min(years)),
    'endyear' = as.character(max(years)),
    'registrationKey' = Sys.getenv("BLS_KEY")
  )
  
  # Retry logic for API calls
  for(attempt in 1:max_retries) {
    tryCatch({
      response <- blsAPI(payload, api_version = 2)
      json_data <- fromJSON(response)
      
      if(json_data$status == "REQUEST_SUCCEEDED") {
        return(json_data)
      } else {
        warning(paste("API request failed for", occupation_code, ":", json_data$message))
        if(attempt == max_retries) return(NULL)
      }
    }, error = function(e) {
      warning(paste("Error fetching data for", occupation_code, "on attempt", attempt, ":", e$message))
      if(attempt == max_retries) return(NULL)
      Sys.sleep(1) # Brief pause before retry
    })
  }
  return(NULL)
}

# Process historical data
process_historical_data <- function(api_response, occupation_code, occupation_name, years) {
  if(is.null(api_response)) {
    return(create_empty_historical_result(occupation_code, occupation_name, years))
  }
  
  if(is.null(api_response$Results) || is.null(api_response$Results$series)) {
    return(create_empty_historical_result(occupation_code, occupation_name, years))
  }
  
  series_df <- api_response$Results$series
  
  if(!is.data.frame(series_df) || nrow(series_df) == 0) {
    return(create_empty_historical_result(occupation_code, occupation_name, years))
  }
  
  # Initialize results data frame
  results_list <- list()
  
  # Process each year
  for(year in years) {
    year_result <- data.frame(
      occupation_code = occupation_code,
      occupation_name = occupation_name,
      year = year,
      employment = NA,
      median_wage = NA,
      mean_wage = NA,
      data_available = FALSE
    )
    
    # Process each series type
    for(i in 1:nrow(series_df)) {
      tryCatch({
        series_id <- series_df$seriesID[i]
        series_data <- series_df$data[[i]]
        
        if(!is.null(series_data) && is.data.frame(series_data) && nrow(series_data) > 0) {
          # Find data for this specific year
          year_data <- series_data[series_data$year == as.character(year), ]
          
          if(nrow(year_data) > 0) {
            raw_value <- year_data$value[1]
            
            if(!is.na(raw_value) && raw_value != "" && raw_value != "-") {
              value <- as.numeric(raw_value)
              
              if(!is.na(value)) {
                year_result$data_available <- TRUE
                
                if(grepl("01$", series_id)) {
                  year_result$employment <- value
                } else if(grepl("04$", series_id)) {
                  year_result$mean_wage <- value
                } else if(grepl("13$", series_id)) {
                  year_result$median_wage <- value
                }
              }
            }
          }
        }
      }, error = function(e) {
        # Continue processing other series
      })
    }
    
    results_list[[length(results_list) + 1]] <- year_result
  }
  
  return(do.call(rbind, results_list))
}

# Helper function for empty historical results
create_empty_historical_result <- function(occupation_code, occupation_name, years) {
  results <- data.frame(
    occupation_code = rep(occupation_code, length(years)),
    occupation_name = rep(occupation_name, length(years)),
    year = years,
    employment = rep(NA, length(years)),
    median_wage = rep(NA, length(years)),
    mean_wage = rep(NA, length(years)),
    data_available = rep(FALSE, length(years))
  )
  return(results)
}

# ==========================================
# HISTORICAL ANALYSIS FUNCTION
# ==========================================

analyze_historical_trends <- function(occupations_list, years) {
  cat("Starting historical analysis for", length(occupations_list), "occupations across", length(years), "years...\n")
  
  all_historical_results <- list()
  successful <- 0
  failed <- 0
  
  for(i in seq_along(occupations_list)) {
    code <- names(occupations_list)[i]
    name <- occupations_list[[code]]
    
    cat(sprintf("(%d/%d) Analyzing historical data: %s - %s\n", i, length(occupations_list), code, name))
    
    # Add delay to be respectful to API
    if(i > 1) Sys.sleep(0.5)
    
    # Get historical data
    raw_data <- get_historical_occupation_data(code, years)
    processed_data <- process_historical_data(raw_data, code, name, years)
    
    all_historical_results[[i]] <- processed_data
    
    # Check if any year has data
    if(any(processed_data$data_available)) {
      successful <- successful + 1
      latest_year_data <- processed_data[processed_data$year == max(years), ]
      if(nrow(latest_year_data) > 0 && latest_year_data$data_available) {
        cat("  ✓ Success: Latest median =", dollar(latest_year_data$median_wage %||% 0), "\n")
      } else {
        cat("  ✓ Success: Historical data available\n")
      }
    } else {
      failed <- failed + 1
      cat("  ✗ Failed: No historical data available\n")
    }
  }
  
  cat(sprintf("\nHistorical analysis complete: %d successful, %d failed\n", successful, failed))
  
  # Combine all results
  final_results <- do.call(rbind, all_historical_results)
  return(final_results)
}

# ==========================================
# MAIN ANALYSIS EXECUTION
# ==========================================

cat("Starting enhanced SCM salary analysis with historical trends...\n")

# Get historical data for all occupations
historical_data <- analyze_historical_trends(scm_occupations_expanded, historical_years)

# ==========================================
# TOP 20 ANALYSIS
# ==========================================

# Get the most recent year's data for ranking
latest_year_data <- historical_data %>%
  filter(year == current_year, data_available == TRUE) %>%
  arrange(desc(median_wage))

# Get top 20 SCM positions
top_20_positions <- head(latest_year_data, 20)

cat("\n", paste(rep("=", 70), collapse=""), "\n")
cat("TOP 20 HIGHEST PAYING SCM POSITIONS (", current_year, ")\n")
cat(paste(rep("=", 70), collapse=""), "\n")

for(i in 1:nrow(top_20_positions)) {
  pos <- top_20_positions[i, ]
  cat(sprintf("%2d. %s\n", i, pos$occupation_name))
  cat(sprintf("    Median: %s | Mean: %s | Employment: %s\n", 
              dollar(pos$median_wage), dollar(pos$mean_wage), comma(pos$employment)))
  cat(sprintf("    Code: %s\n\n", pos$occupation_code))
}

# ==========================================
# 10-YEAR TREND ANALYSIS
# ==========================================

# Calculate trends for top 20 positions
top_20_codes <- top_20_positions$occupation_code

trend_analysis <- historical_data %>%
  filter(occupation_code %in% top_20_codes, data_available == TRUE) %>%
  group_by(occupation_code, occupation_name) %>%
  summarise(
    years_with_data = n(),
    earliest_year = min(year, na.rm = TRUE),
    latest_year = max(year, na.rm = TRUE),
    earliest_median = first(median_wage[order(year)], na_rm = TRUE),
    latest_median = last(median_wage[order(year)], na_rm = TRUE),
    median_10yr_change = latest_median - earliest_median,
    median_10yr_pct_change = ((latest_median / earliest_median) - 1) * 100,
    earliest_employment = first(employment[order(year)], na_rm = TRUE),
    latest_employment = last(employment[order(year)], na_rm = TRUE),
    employment_10yr_change = latest_employment - earliest_employment,
    employment_10yr_pct_change = ((latest_employment / earliest_employment) - 1) * 100,
    .groups = 'drop'
  ) %>%
  arrange(desc(latest_median))

cat("\n10-YEAR SALARY TRENDS FOR TOP 20 SCM POSITIONS:\n")
cat(paste(rep("=", 70), collapse=""), "\n")

for(i in 1:nrow(trend_analysis)) {
  trend <- trend_analysis[i, ]
  cat(sprintf("%2d. %s\n", i, trend$occupation_name))
  cat(sprintf("    %d Median: %s → %s (%+.1f%% over %d years)\n", 
              trend$earliest_year, dollar(trend$earliest_median),
              trend$latest_year, dollar(trend$latest_median),
              trend$median_10yr_pct_change, 
              trend$latest_year - trend$earliest_year))
  cat(sprintf("    Employment: %s → %s (%+.1f%%)\n\n", 
              comma(trend$earliest_employment), comma(trend$latest_employment),
              trend$employment_10yr_pct_change))
}

# ==========================================
# CREATE COMPREHENSIVE DATASET
# ==========================================

# Merge current year data with trend analysis
comprehensive_dataset <- top_20_positions %>%
  left_join(trend_analysis, by = c("occupation_code", "occupation_name")) %>%
  mutate(
    rank = row_number(),
    occupation_level = case_when(
      str_detect(occupation_code, "^11-") ~ "Management",
      str_detect(occupation_code, "^13-1081|^13-1023|^13-1022|^13-1199") ~ "Core SCM Professional",
      str_detect(occupation_code, "^13-1111|^15-2031|^17-2112") ~ "SCM-Adjacent Analytical", 
      str_detect(occupation_code, "^43-|^53-") ~ "Operational/Support",
      TRUE ~ "Other"
    ),
    scm_function = case_when(
      str_detect(occupation_code, "^11-3061|^13-1023|^13-1022") ~ "Procurement & Sourcing",
      str_detect(occupation_code, "^11-3071|^43-5011|^43-5071|^53-1047") ~ "Transportation & Logistics",
      str_detect(occupation_code, "^13-1081") ~ "Supply Chain Planning",
      str_detect(occupation_code, "^43-5061") ~ "Production Planning", 
      str_detect(occupation_code, "^13-1199") ~ "Supply Chain Analysis",
      str_detect(occupation_code, "^13-1111|^15-2031|^17-2112") ~ "Process Optimization",
      str_detect(occupation_code, "^11-9199|^11-1011") ~ "General Operations",
      TRUE ~ "Other SCM Functions"
    ),
    median_hourly_current = median_wage / 2080,
    mean_hourly_current = mean_wage / 2080,
    annual_salary_growth = median_10yr_change / (latest_year - earliest_year),
    compound_annual_growth_rate = ((latest_median / earliest_median)^(1/(latest_year - earliest_year)) - 1) * 100
  ) %>%
  select(
    rank, occupation_code, occupation_name, occupation_level, scm_function,
    employment, median_wage, mean_wage, median_hourly_current, mean_hourly_current,
    earliest_year, earliest_median, latest_year, latest_median,
    median_10yr_change, median_10yr_pct_change, annual_salary_growth, compound_annual_growth_rate,
    earliest_employment, latest_employment, employment_10yr_change, employment_10yr_pct_change,
    years_with_data
  )

# ==========================================
# CREATE SORTABLE/FILTERABLE OUTPUTS
# ==========================================

# Create output directory
if(!dir.exists("output")) dir.create("output")

# 1. Excel file with multiple sheets
wb <- createWorkbook()

# Sheet 1: Top 20 Current Rankings
addWorksheet(wb, "Top 20 SCM Positions 2024")
writeData(wb, "Top 20 SCM Positions 2024", comprehensive_dataset)

# Add formatting
headerStyle <- createStyle(textDecoration = "bold", fgFill = "#4472C4", fontColour = "white")
addStyle(wb, "Top 20 SCM Positions 2024", headerStyle, rows = 1, cols = 1:ncol(comprehensive_dataset))

# Sheet 2: Historical Data (All Years)
historical_long <- historical_data %>%
  filter(occupation_code %in% top_20_codes, data_available == TRUE) %>%
  arrange(occupation_code, year)

addWorksheet(wb, "Historical Data All Years")
writeData(wb, "Historical Data All Years", historical_long)
addStyle(wb, "Historical Data All Years", headerStyle, rows = 1, cols = 1:ncol(historical_long))

# Sheet 3: Summary Statistics
summary_stats <- comprehensive_dataset %>%
  group_by(occupation_level) %>%
  summarise(
    count = n(),
    avg_median_wage = mean(median_wage, na.rm = TRUE),
    avg_10yr_growth = mean(median_10yr_pct_change, na.rm = TRUE),
    total_employment = sum(employment, na.rm = TRUE),
    .groups = 'drop'
  )

addWorksheet(wb, "Summary by Level")
writeData(wb, "Summary by Level", summary_stats)
addStyle(wb, "Summary by Level", headerStyle, rows = 1, cols = 1:ncol(summary_stats))

# Save Excel file
excel_filename <- paste0("output/Top_20_SCM_Salaries_Analysis_", current_year, ".xlsx")
saveWorkbook(wb, excel_filename, overwrite = TRUE)

# 2. Create interactive HTML table
create_interactive_table <- function(data, filename) {
  # Create DT datatable
  dt <- datatable(
    data,
    options = list(
      pageLength = 20,
      scrollX = TRUE,
      searchHighlight = TRUE,
      dom = 'Bfrtip',
      buttons = c('copy', 'csv', 'excel', 'pdf', 'print'),
      columnDefs = list(
        list(targets = c(5, 6, 7, 8, 9, 12, 13, 16, 17), render = JS(
          "function(data, type, row, meta) {",
          "return type === 'display' && data != null ? '$' + data.toString().replace(/\\B(?=(\\d{3})+(?!\\d))/g, ',') : data;",
          "}"
        )),
        list(targets = c(14, 15, 18, 19, 21), render = JS(
          "function(data, type, row, meta) {",
          "return type === 'display' && data != null ? data.toFixed(1) + '%' : data;",
          "}"
        ))
      )
    ),
    extensions = 'Buttons',
    filter = 'top',
    caption = paste0('Top 20 SCM Positions - Salary Analysis ', current_year),
    class = 'cell-border stripe hover'
  ) %>%
    formatCurrency(c('employment', 'median_wage', 'mean_wage', 'median_hourly_current', 
                     'mean_hourly_current', 'earliest_median', 'latest_median', 
                     'median_10yr_change', 'annual_salary_growth', 'earliest_employment', 
                     'latest_employment', 'employment_10yr_change'), digits = 0) %>%
    formatPercentage(c('median_10yr_pct_change', 'employment_10yr_pct_change', 
                       'compound_annual_growth_rate'), digits = 1)
  
  # Save as HTML
  html_filename <- paste0("output/", filename, ".html")
  saveWidget(dt, html_filename, selfcontained = TRUE)
  
  return(dt)
}

# Create interactive table
interactive_table <- create_interactive_table(comprehensive_dataset, "Interactive_Top_20_SCM_Salaries")

# 3. CSV files for easy import
write_csv(comprehensive_dataset, paste0("output/top_20_scm_positions_", current_year, ".csv"))
write_csv(historical_long, paste0("output/scm_historical_data_", current_year, ".csv"))
write_csv(summary_stats, paste0("output/scm_summary_by_level_", current_year, ".csv"))

# ==========================================
# FINAL REPORTING
# ==========================================

cat("\n", paste(rep("=", 70), collapse=""), "\n")
cat("ANALYSIS COMPLETE - FILES CREATED:\n")
cat(paste(rep("=", 70), collapse=""), "\n")

cat("📊 INTERACTIVE OUTPUTS:\n")
cat("  • Interactive_Top_20_SCM_Salaries.html - Sortable/filterable web table\n")
cat("  • Top_20_SCM_Salaries_Analysis_", current_year, ".xlsx - Excel with multiple sheets\n\n")

cat("📁 CSV FILES:\n")
cat("  • top_20_scm_positions_", current_year, ".csv - Main analysis results\n")
cat("  • scm_historical_data_", current_year, ".csv - All historical data\n")
cat("  • scm_summary_by_level_", current_year, ".csv - Summary statistics\n\n")

cat("🎯 KEY INSIGHTS:\n")
top_position <- comprehensive_dataset[1, ]
cat("  • Highest paying SCM position:", top_position$occupation_name, "\n")
cat("  • Top salary:", dollar(top_position$median_wage), "median\n")
cat("  • Best 10-year growth:", 
    comprehensive_dataset$occupation_name[which.max(comprehensive_dataset$median_10yr_pct_change)], 
    sprintf("(%.1f%%)\n", max(comprehensive_dataset$median_10yr_pct_change, na.rm = TRUE)))

total_scm_employment <- sum(comprehensive_dataset$employment, na.rm = TRUE)
cat("  • Total employment in top 20:", comma(total_scm_employment), "positions\n")

avg_growth <- mean(comprehensive_dataset$median_10yr_pct_change, na.rm = TRUE)
cat("  • Average 10-year salary growth:", sprintf("%.1f%%\n", avg_growth))

cat("\n✅ Open the HTML file in your browser for interactive sorting and filtering!\n")
cat("✅ Use the Excel file for detailed analysis and pivot tables!\n")
