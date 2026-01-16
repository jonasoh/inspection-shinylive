#!/usr/bin/env Rscript
# preprocess_data.R
# This script processes raw TSV inspection data files, generates
# precomputed RDS files, and exports a deployable Shinylive site.
#
# Usage: Rscript preprocess_data.R
#
# Outputs:
#   data/       - RDS files for the app (bundled into site/)
#   tsv_data/   - TSV files for manual inspection
#   site/       - Deployable static Shinylive site

library(data.table)
library(stringr)
library(shinylive)

# Configuration
input_files <- c(
  "periodic_inspections_2017-2019.tsv.gz",
  "periodic_inspections_2020-2022.tsv.gz",
  "periodic_inspections_2023-2024.tsv.gz"
)
rds_dir <- "data"       # RDS files for the app
tsv_dir <- "tsv_data"   # TSV files for inspection
site_dir <- "site" # Deployable shinylive site
src_data_dir <- "source_data"

# Create output directories if they don't exist
if (!dir.exists(rds_dir)) dir.create(rds_dir)
if (!dir.exists(tsv_dir)) dir.create(tsv_dir)

# Helper function to save RDS (for app) and TSV (for inspection)
save_data <- function(dt, name) {
  rds_path <- file.path(rds_dir, paste0(name, ".rds"))
  tsv_path <- file.path(tsv_dir, paste0(name, ".tsv"))
  
  saveRDS(dt, rds_path)
  fwrite(dt, tsv_path, sep = "\t")
  
  message(sprintf("Saved %s: %d rows", name, nrow(dt)))
}

# Read and combine all TSV files
message("Reading input files...")
stats_list <- lapply(input_files, function(f) {
  message(sprintf("  Reading %s...", f))
  # Skip the title row (line 1), header is on line 2
  # Use encoding to handle Finnish characters properly
  dt <- fread(file.path(src_data_dir, f), skip = 2, header = TRUE, na.strings = ".", encoding = "Latin-1")
  # Convert character columns to proper UTF-8
  chr_cols <- names(dt)[sapply(dt, is.character)]
  for (col in chr_cols) {
    Encoding(dt[[col]]) <- "latin1"
    dt[[col]] <- enc2utf8(dt[[col]])
  }
  dt
})

stats <- rbindlist(stats_list)
message(sprintf("Combined data: %d rows", nrow(stats)))

# Normalize column names
old_names <- names(stats)
new_names <- c(
  "year_of_inspection",
  "brand_and_model_series",
  "registration_year",
  "main_fault_object",
  "number_of_inspections",
  "average_mileage",
  "median_mileage",
  "demand_for_repairs",
  "rejections",
  "driving_bans"
)
setnames(stats, old_names, new_names)

# Filter and clean data
message("Cleaning data...")

# Remove rows with missing inspection counts or "Years in total" aggregates
stats <- stats[!is.na(number_of_inspections) & number_of_inspections > 0]
stats <- stats[registration_year != "Years in total"]

# Convert types
stats[, number_of_inspections := as.integer(number_of_inspections)]
stats[, demand_for_repairs := as.integer(demand_for_repairs)]
stats[, rejections := as.integer(rejections)]
stats[, driving_bans := as.integer(driving_bans)]
stats[, average_mileage := as.integer(average_mileage)]
stats[, median_mileage := as.integer(median_mileage)]
stats[, year_of_inspection := as.integer(year_of_inspection)]
stats[, registration_year := as.integer(registration_year)]

# Calculate derived columns
stats[, vehicle_age := year_of_inspection - registration_year]
stats[, fault_pct := (demand_for_repairs + rejections + driving_bans) / number_of_inspections]
stats[, brand := str_match(brand_and_model_series, "^(.*) - ")[, 2]]
stats[, brand_and_model_series := str_replace(brand_and_model_series, fixed(" - "), " ")]

message(sprintf("Cleaned data: %d rows", nrow(stats)))
fwrite(stats, file.path(tsv_dir, "combined_stats.tsv"), sep = "\t") # no rds needed
message("Saved combined_stats.tsv to tsv_data/")

# Define model-related fault categories (used for rankings)
model_related <- c(
  "Axles, wheels and suspension (all objects)",
  "Chassis and body (all objects)",
  "Brake systems (all objects)",
  "Steering equipment (all objects)",
  "Environmental hazards (all objects)"
)

# Extract metadata for UI controls
years <- sort(unique(stats$registration_year))
ages <- sort(unique(stats$vehicle_age))
cars <- sort(unique(stats$brand_and_model_series))
fault_categories <- sort(unique(stats$main_fault_object))

metadata <- list(
  years = years,
  ages = ages,
  cars = cars,
  fault_categories = fault_categories,
  model_related = model_related
)
saveRDS(metadata, file.path(rds_dir, "metadata.rds"))

# Save metadata as separate TSV files for inspection (since vectors have different lengths)
fwrite(data.table(year = years), file.path(tsv_dir, "metadata_years.tsv"), sep = "\t")
fwrite(data.table(age = ages), file.path(tsv_dir, "metadata_ages.tsv"), sep = "\t")
fwrite(data.table(car = cars), file.path(tsv_dir, "metadata_cars.tsv"), sep = "\t")
fwrite(data.table(fault_category = fault_categories), file.path(tsv_dir, "metadata_fault_categories.tsv"), sep = "\t")
fwrite(data.table(model_related = model_related), file.path(tsv_dir, "metadata_model_related.tsv"), sep = "\t")
message("Saved metadata.rds")

# Precompute aggregated tables

message("Computing stats_model_year...")
stats_model_year <- stats[main_fault_object %in% model_related,
  .(
    fault_pct = sum(c(demand_for_repairs, rejections, driving_bans)) / number_of_inspections[1],
    average_mileage = average_mileage[1],
    brand = brand[1],
    number_of_inspections = number_of_inspections[1]
  ),
  by = .(year_of_inspection, brand_and_model_series, registration_year)
][,
  .(
    fault_pct = round(weighted.mean(fault_pct, number_of_inspections), 3),
    average_mileage = as.integer(weighted.mean(average_mileage, number_of_inspections)),
    number_of_inspections = sum(number_of_inspections),
    brand = brand[1]
  ),
  by = .(brand_and_model_series, registration_year)
]
save_data(stats_model_year, "stats_model_year")

message("Computing stats_by_fault...")
stats_by_fault <- stats[,
  .(
    fault_pct = sum(c(demand_for_repairs, rejections, driving_bans)) / number_of_inspections[1],
    number_of_inspections = number_of_inspections[1],
    brand = brand[1],
    average_mileage = average_mileage
  ),
  by = .(year_of_inspection, brand_and_model_series, registration_year, main_fault_object)
]
save_data(stats_by_fault, "stats_by_fault")

message("Computing avg_stats_by_fault...")
avg_stats_by_fault <- stats_by_fault[,
  .(
    fault_pct = weighted.mean(fault_pct, number_of_inspections),
    number_of_inspections = sum(number_of_inspections)
  ),
  by = .(year_of_inspection, registration_year, main_fault_object)
]
setorder(avg_stats_by_fault, year_of_inspection, registration_year)
save_data(avg_stats_by_fault, "avg_stats_by_fault")

message("Computing stats_age...")
stats_age <- stats[main_fault_object %in% model_related,
  .(
    fault_pct = sum(c(demand_for_repairs, rejections, driving_bans)) / number_of_inspections[1],
    average_mileage = average_mileage[1],
    number_of_inspections = number_of_inspections[1]
  ),
  by = .(year_of_inspection, brand_and_model_series, vehicle_age)
][,
  .(
    fault_pct = round(weighted.mean(fault_pct, number_of_inspections), 3),
    average_mileage = as.integer(weighted.mean(average_mileage, number_of_inspections)),
    number_of_inspections = sum(number_of_inspections)
  ),
  by = .(brand_and_model_series, vehicle_age)
]
setorder(stats_age, brand_and_model_series, vehicle_age)
save_data(stats_age, "stats_age")

message("Computing stats_age_by_year...")
stats_age_by_year <- stats[main_fault_object %in% model_related,
  .(
    number_of_inspections = number_of_inspections[1]
  ),
  by = .(year_of_inspection, brand_and_model_series, registration_year, vehicle_age)
][,
  .(
    number_of_inspections = sum(number_of_inspections)
  ),
  by = .(brand_and_model_series, registration_year, vehicle_age)
]
setorder(stats_age_by_year, brand_and_model_series, registration_year, vehicle_age)
save_data(stats_age_by_year, "stats_age_by_year")

message("Computing brand_stats_by_age...")
brand_stats_by_age <- stats_model_year[,
  .(
    fault_pct = weighted.mean(fault_pct, number_of_inspections),
    average_mileage = weighted.mean(as.numeric(average_mileage), number_of_inspections)
  ),
  by = .(brand, registration_year)
][,
  .(
    rank = frank(fault_pct),
    brand = brand,
    average_mileage = as.integer(average_mileage)
  ),
  by = .(registration_year)
]
setorder(brand_stats_by_age, registration_year, brand)
save_data(brand_stats_by_age, "brand_stats_by_age")

message("\nPreprocessing complete!")
message(sprintf("RDS files written to: %s/", rds_dir))
message(sprintf("TSV files written to: %s/", tsv_dir))

# Export Shinylive site
# Create a temporary app directory with only the files needed for the app
message("\nExporting Shinylive site...")
app_staging <- tempfile("shinylive_app_")
dir.create(app_staging)
file.copy("app.R", app_staging)
file.copy(rds_dir, app_staging, recursive = TRUE)

if (dir.exists(site_dir)) {
  unlink(site_dir, recursive = TRUE)
}
shinylive::export(appdir = app_staging, destdir = site_dir)
unlink(app_staging, recursive = TRUE)

message(sprintf("\nDeployable site created at: %s/", site_dir))
message("To preview locally: httpuv::runStaticServer(site_dir)")
