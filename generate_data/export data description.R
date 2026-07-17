# write data documentation

# Load required package
library(openxlsx2)

# Identify all data frames in the global environment
df_names <- ls()[sapply(ls(), function(x) is.data.frame(get(x)))]

# Create a new workbook
wb <- wb_workbook()

# Loop through each dataframe
for (df_name in df_names) {
  
  df <- get(df_name)
  
  # Create a template with variable names and empty description
  template <- data.frame(
    variable = names(df),
    description = "",
    stringsAsFactors = FALSE
  )
  
  # Add sheet
  wb$add_worksheet(df_name)
  
  # Write data
  wb$add_data(df_name, template)
}

# Save workbook
# wb$save("6_paper/data/CalcData_description.xlsx", overwrite = TRUE)
wb$save("data/CalcData_metadata.xlsx", overwrite = TRUE)
wb$save("data/CalcRevenues_metadata.xlsx", overwrite = TRUE)
wb$save("data/simulation_metadata.xlsx", overwrite = TRUE)
