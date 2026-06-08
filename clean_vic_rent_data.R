library(readxl)
library(dplyr)
library(tidyr)
library(stringr)
library(readr)

file_path <- "data/quarterly-median-rents-local-government-area-september-quarter-2025-excel.xlsx"

# Read raw sheet without headers
raw <- read_excel(file_path, sheet = "All Properties", col_names = FALSE)

# Remove fully empty rows
raw <- raw[rowSums(is.na(raw)) < ncol(raw), ]

# Header rows
quarter_row <- raw[2, ]
metric_row <- raw[3, ]

# Build column names
new_names <- c("region", "lga")

for (i in 3:ncol(raw)) {
  quarter <- as.character(quarter_row[[i]])
  metric <- as.character(metric_row[[i]])
  new_names <- c(new_names, paste0(str_trim(quarter), "_", str_trim(metric)))
}

names(raw) <- new_names

# Remove header rows
df <- raw[-c(1, 2, 3), ]

# Fill down region names
df <- df %>%
  fill(region, .direction = "down") %>%
  mutate(
    region = str_trim(as.character(region)),
    lga = str_trim(as.character(lga))
  )

# Remove invalid rows
invalid_lga <- c(
  "", "NA", "...",
  "Group Total",
  "Table Total",
  "Victoria",
  "Metro",
  "Non-Metro"
)

df <- df %>%
  filter(!is.na(lga)) %>%
  filter(!lga %in% invalid_lga)

# Identify Count and Median columns
count_cols <- names(df)[str_detect(names(df), "_Count$")]
median_cols <- names(df)[str_detect(names(df), "_Median$")]

# Convert Median columns to long format
median_long <- df %>%
  select(region, lga, all_of(median_cols)) %>%
  pivot_longer(
    cols = all_of(median_cols),
    names_to = "quarter",
    values_to = "median_rent"
  ) %>%
  mutate(quarter = str_remove(quarter, "_Median$"))

# Convert Count columns to long format
count_long <- df %>%
  select(region, lga, all_of(count_cols)) %>%
  pivot_longer(
    cols = all_of(count_cols),
    names_to = "quarter",
    values_to = "count"
  ) %>%
  mutate(quarter = str_remove(quarter, "_Count$"))

# Join median and count
cleaned <- median_long %>%
  left_join(count_long, by = c("region", "lga", "quarter")) %>%
  mutate(
    state = "VIC",
    property_type = "All Properties",
    median_rent = as.numeric(median_rent),
    count = as.numeric(count)
  ) %>%
  filter(!is.na(median_rent)) %>%
  select(
    state,
    region,
    lga,
    quarter,
    property_type,
    count,
    median_rent
  )

# Save output
write_csv(cleaned, "vic_all_properties_long.csv")