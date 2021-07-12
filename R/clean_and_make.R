# Header =======================================================================
# Author: Pablo Reyes Moctezuma (https://github.com/pablorm296/)
# Description: This script takes the raw data files from the repo's `Data`
# directory to create a clean, open format, and fully machine-readable version 
# of the IMF’s database summarizing key fiscal measures governments have announced 
# or taken in selected economies in response to the COVID-19 pandemic.
# Set up =======================================================================
# Load packages
library(tidyverse)
library(readxl)

# Read data ====================================================================

# FMI Fiscal measures data 
FMI_FISCAL <- read_xlsx("Data/FMI-FiscalMeasures-Data.xlsx", sheet = 2)

# Country codes
COUNTRY_CODES <- read_csv("Data/UNSD_codes.csv")

# Country income classification
COUNTRY_CLASS <- read_xls("Data/CLASS.xls", sheet = 3)

# World Bank economic indicators
ECON_INDICATORS <- read_csv("Data/WorldBankData.csv")

# Data cleaning ================================================================

## World Bank economic indicators ----

# Set NA
ECON_INDICATORS[ECON_INDICATORS == ".."] <- NA

# Remove last rows
ECON_INDICATORS %>%
  slice(1:1302) -> ECON_INDICATORS

# Rename variables
ECON_INDICATORS %>%
  rename(SeriesFullName = `Series Name`,
         SeriesCode = `Series Code`,
         Country = `Country Name`,
         ISO_3_Code = `Country Code`) -> ECON_INDICATORS

# Rename year variables
names(ECON_INDICATORS) %>%
  str_replace("(\\d{4}) \\[YR\\d{4}\\]", "YR_\\1") -> names(ECON_INDICATORS)

unique(ECON_INDICATORS$SeriesCode)
unique(ECON_INDICATORS$SeriesFullName)

# Replace indicator codes
ECON_INDICATORS %>%
  mutate(SeriesCode = case_when(
    SeriesCode == "SI.POV.GINI" ~ "GINI",
    SeriesCode == "SI.POV.MDIM" ~ "Poverty.MDIM",
    SeriesCode == "SI.POV.LMIC" ~ "Poverty.320",
    SeriesCode == "SI.POV.DDAY" ~ "Poverty.190",
    SeriesCode == "SI.POV.UMIC" ~ "Poverty.550",
    SeriesCode == "NY.GDP.MKTP.CD" ~ "GDP",
    TRUE ~ as.character(SeriesCode)
  )) -> ECON_INDICATORS

# Split DF
ECON_INDICATORS %>%
  group_split(SeriesCode) -> ECON_INDICATORS

# Define a function that get the last available element in a vector
get_last_value <- function(x, index = F) {
  # If all is NA, then NA
  if (all(is.na(x))) {
    return(NA)
  }
  
  for (i in length(x):1) {
    tmp <- x[i]
    if (is.na(tmp)) {
      next()
    } else {
      if (index) {
        return(i)
      }
      return(tmp)
    }
  }
}

# Define a function to get the last available value and year
get_last_value_and_year <- function(x) {
  # Create a subset of x without the first 4 values
  x_subset <- x[,5:ncol(x)]
  
  last_values <- c()
  last_years <- c()
  
  # Loop through each row
  for (i in 1:nrow(x_subset)) {
    # Get row
    row_tmp <- x[i,]
    row_tmp <- as.character(row_tmp)[5:length(row_tmp)]
    
    # Get last value
    last_val <- get_last_value(row_tmp)
    last_val <- as.numeric(last_val)
    # Get last index
    last_i <- get_last_value(row_tmp, index = T)
    # Get last year
    last_year <- 2011 + (last_i - 1)
    
    last_values <- c(last_values, last_val)
    last_years <- c(last_years, last_year)
  }
  
  # Get indicator code
  indicator_code <- unique(x$SeriesCode)
  x[str_c(indicator_code, "_lastVal")] <- last_values
  x[str_c(indicator_code, "_lastYear")] <- last_years
  
  return(x)
}

ECON_INDICATORS %>%
  as.list() -> ECON_INDICATORS
for (i in 1:length(ECON_INDICATORS)) {
  ECON_INDICATORS[[i]] <- get_last_value_and_year(ECON_INDICATORS[[i]])
}
rm(i)

## Country classification ----

COUNTRY_CLASS %>%
  filter(GroupCode %in% c("HIC", "LIC", "LMC", "UMC")) -> COUNTRY_CLASS

## Country codes ----

# Replace white spaces with _
names(COUNTRY_CODES) %>%
  str_replace_all("\\s+", "_") -> names(COUNTRY_CODES)

# Remove /
names(COUNTRY_CODES) %>%
  str_replace_all("\\/", "") -> names(COUNTRY_CODES)
names(COUNTRY_CODES) %>%
  str_replace_all("__", "_") -> names(COUNTRY_CODES)

## FMI Fiscal measures data ----

# Create a clean copy
FMI_FISCAL_CLEAN <- FMI_FISCAL

# Remove top and bottom rows that contain nothing
FMI_FISCAL_CLEAN <- FMI_FISCAL_CLEAN[1:205,]
FMI_FISCAL_CLEAN <- FMI_FISCAL_CLEAN[7:nrow(FMI_FISCAL_CLEAN),]

# Get columns where all cases are NA
NACols <- c()
for (i in 1:ncol(FMI_FISCAL_CLEAN)) {
  # Get col
  col_tmp <- FMI_FISCAL_CLEAN[,i]
  
  # Check if all is NA
  all_is_na <- all(is.na(col_tmp))
  
  if (all_is_na) {
    NACols <- c(i, NACols)
  }
}
rm(i)

# Remove cols that are NA
FMI_FISCAL_CLEAN[,NACols] <- NULL

# Create new columns names
NewColumnsNames <- c("ALM_ASFR_Subtotal", "ALM_ASFR_Health", "ALM_ASFR_NonHealth", 
                     "ALM_ASFR_Accelerated",
                     "Liquidity_Subtotal", "Liquidity_BLM", "Liquidity_Guarantees",
                     "Liquidity_Quasifiscal")

# Create a new set of column names (for percent)
NewColumnsNamesPercent <- str_c("PGDP_", NewColumnsNames)

# Concatenate
NewColumnsNamesConc <- c("Country", NewColumnsNames, NewColumnsNamesPercent)

# Set new colnames
colnames(FMI_FISCAL_CLEAN) <- NewColumnsNamesConc

# Now, get all the rows that are NA
NARows <- c()
for (i in 1:nrow(FMI_FISCAL_CLEAN)) {
  # Get row
  row_tmp <- FMI_FISCAL_CLEAN[i,2:ncol(FMI_FISCAL_CLEAN)]
  row_tmp <- as.numeric(row_tmp)
  
  # Check if all is NA
  all_is_na <- all(is.na(row_tmp))
  
  if(all_is_na) {
    NARows <- c(i, NARows)
  }
}
rm(i)

FMI_FISCAL_CLEAN %>%
  slice(-NARows) -> FMI_FISCAL_CLEAN

# Set as numeric
FMI_FISCAL_CLEAN %>%
  mutate(across(matches("ALM_|Liquidity_"), ~ as.numeric(.x) ) ) -> FMI_FISCAL_CLEAN

# Now, divide all the variables that start with PGDP
divide_by_100 <- function(x) {
  return(x/100)
}
FMI_FISCAL_CLEAN %>%
  mutate(across(starts_with("PGDP_"), ~ divide_by_100(.x) ) ) -> FMI_FISCAL_CLEAN

rm(all_is_na, NACols, NARows, NewColumnsNames, NewColumnsNamesConc, NewColumnsNamesPercent,
   row_tmp, col_tmp)

## Merge country codes and IMF data ----

# Get unique countries in country codes
CountriesInCountryCodes <- unique(COUNTRY_CODES$Country_or_Area)
CountriesInIMFData <- unique(FMI_FISCAL_CLEAN$Country)

# Inspect countries that doesn't appear in the UN country codes dataset
MissingCountries <- CountriesInIMFData[!(CountriesInIMFData %in% CountriesInCountryCodes)]

MissingCountries %>% sort() -> MissingCountries
CountriesInCountryCodes %>% sort() -> CountriesInCountryCodes

MissingCountries
CountriesInCountryCodes

# Set new country names (countries named differently in the UN codes dataset)
FMI_FISCAL_CLEAN %>%
  mutate(Country = case_when(
    Country == MissingCountries[1] ~ "Bahamas",
    Country == MissingCountries[2] ~ "Bolivia (Plurinational State of)",
    Country == MissingCountries[3] ~ "Congo",
    Country == MissingCountries[4] ~ "Côte d’Ivoire",
    Country == MissingCountries[5] ~ "Czechia",
    Country == MissingCountries[6] ~ NA_character_,
    Country == MissingCountries[7] ~ "Gambia",
    Country == MissingCountries[8] ~ NA_character_,
    Country == MissingCountries[9] ~ NA_character_,
    Country == MissingCountries[10] ~ "Iran (Islamic Republic of)",
    Country == MissingCountries[11] ~ "Republic of Korea",
    Country == MissingCountries[12] ~ NA_character_,
    Country == MissingCountries[13] ~ "Kyrgyzstan",
    Country == MissingCountries[14] ~ "Lao People's Democratic Republic",
    Country == MissingCountries[15] ~ NA_character_,
    Country == MissingCountries[16] ~ "Micronesia (Federated States of)",
    Country == MissingCountries[17] ~ "Republic of Moldova",
    Country == MissingCountries[18] ~ "Montenegro",
    Country == MissingCountries[19] ~ "Russian Federation",
    Country == MissingCountries[20] ~ "Sao Tome and Principe",
    Country == MissingCountries[21] ~ "Slovakia",
    Country == MissingCountries[22] ~ "Saint Kitts and Nevis",
    Country == MissingCountries[23] ~ "Saint Lucia",
    Country == MissingCountries[24] ~ "Saint Vincent and the Grenadines",
    Country == MissingCountries[25] ~ "United Republic of Tanzania",
    Country == MissingCountries[26] ~ "Netherlands",
    Country == MissingCountries[27] ~ "Timor-Leste",
    Country == MissingCountries[28] ~ "United Kingdom of Great Britain and Northern Ireland",
    Country == MissingCountries[29] ~ "United States of America",
    Country == MissingCountries[30] ~ "Viet Nam",
    TRUE ~ as.character(Country)
  )) -> FMI_FISCAL_CLEAN

# Merge datasets
FMI_FISCAL_CLEAN %>%
  left_join(COUNTRY_CODES, by = c("Country" = "Country_or_Area")) -> FMI_FISCAL_CLEAN

# Remove unused vars
vars2remove <- c(names(FMI_FISCAL_CLEAN)[18:19], names(FMI_FISCAL_CLEAN)[24:25], names(FMI_FISCAL_CLEAN)[29:32])
vars2remove

FMI_FISCAL_CLEAN %>%
  select(-any_of(vars2remove)) -> FMI_FISCAL_CLEAN

# Rename some vars
FMI_FISCAL_CLEAN %>%
  rename(ISO_2_Code = `ISO-alpha2_Code`,
         ISO_3_Code = `ISO-alpha3_Code`) -> FMI_FISCAL_CLEAN
FMI_FISCAL_CLEAN %>%
  rename(SubRegion_Name = `Sub-region_Name`,
         SubRegion_Code = `Sub-region_Code`) -> FMI_FISCAL_CLEAN

# Reorder variables
names(FMI_FISCAL_CLEAN)
FMI_FISCAL_CLEAN %>%
  relocate(Region_Code, Region_Name, SubRegion_Code, SubRegion_Name,
           M49_Code, ISO_2_Code, ISO_3_Code, .after = Country) -> FMI_FISCAL_CLEAN

# Remove countries that are NA
FMI_FISCAL_CLEAN %>%
  filter(!is.na(Country)) -> FMI_FISCAL_CLEAN

FMI_FISCAL_CLEAN %>%
  filter(ISO_3_Code != "HK",
         ISO_3_Code != "MO") -> FMI_FISCAL_CLEAN

rm(CountriesInCountryCodes, CountriesInIMFData, MissingCountries, vars2remove)

## Merge IMF data and income classification ----

# Merge
FMI_FISCAL_CLEAN %>%
  left_join(COUNTRY_CLASS %>% select(GroupCode, GroupName, CountryCode),
            by = c("ISO_3_Code" = "CountryCode")) -> FMI_FISCAL_CLEAN

# Rename columns
FMI_FISCAL_CLEAN %>%
  rename(IncomeGroup_Code = GroupCode,
         IncomeGroup_Name = GroupName) -> FMI_FISCAL_CLEAN

FMI_FISCAL_CLEAN %>%
  relocate(IncomeGroup_Code, IncomeGroup_Name, .after = ISO_3_Code) -> FMI_FISCAL_CLEAN

## Merge IMF data and Economic indicators ----

for (i in 1:length(ECON_INDICATORS)) {
  # Get data
  foo_data <- ECON_INDICATORS[[i]]
  
  # Get indicator name
  indicator_name <- unique(foo_data$SeriesCode)
  
  # Get name of lastValue var
  indicator_lastVal_name <- str_c(indicator_name, "_lastVal")
  indicator_lastYear_name <- str_c(indicator_name, "_lastYear")
  
  # Subset
  foo_data %>%
    select(any_of(c("ISO_3_Code", indicator_lastVal_name, indicator_lastYear_name))) -> foo_data
  
  # Clean names
  names(foo_data) %>%
    str_replace_all("\\.", "_") -> names(foo_data)
  names(foo_data) %>%
    str_replace_all("lastVal", "Value") -> names(foo_data)
  names(foo_data) %>%
    str_replace_all("lastYear", "Year") -> names(foo_data)
  
  # Merge
  FMI_FISCAL_CLEAN %>%
    left_join(foo_data, by = "ISO_3_Code") -> FMI_FISCAL_CLEAN
  
}
rm(foo_data, i, indicator_lastVal_name, indicator_lastYear_name, indicator_name)

## Recompute PGDP variables ----

# Get variables that need to be recomputed
VariablesToRecompute <- names(FMI_FISCAL_CLEAN) %>%
  str_detect("^ALM_|^Liquidity")
VariablesToRecompute <- names(FMI_FISCAL_CLEAN)[VariablesToRecompute]

for (var in VariablesToRecompute) {
  FMI_FISCAL_CLEAN[var] <- FMI_FISCAL_CLEAN[var] * 1000 * 1000000
}

for (var in VariablesToRecompute) {
  pgdp_var <- str_c("PGDP_", var)
  FMI_FISCAL_CLEAN[pgdp_var] <- FMI_FISCAL_CLEAN[var] / FMI_FISCAL_CLEAN["GDP_Value"]
}
rm(var, VariablesToRecompute, pgdp_var)

## Get totals ----

FMI_FISCAL_CLEAN %>%
  mutate(Total_measures = ALM_ASFR_Subtotal + Liquidity_Subtotal) -> FMI_FISCAL_CLEAN

FMI_FISCAL_CLEAN %>%
  relocate(Total_measures, .after = IncomeGroup_Name) -> FMI_FISCAL_CLEAN

# Export data ==================================================================

FMI_FISCAL_CLEAN %>%
  write_csv("Out/IMF_FiscalResponseData.csv")
FMI_FISCAL_CLEAN %>%
  write_excel_csv("Out/IMF_FiscalResponseData.csv")
FMI_FISCAL_CLEAN %>%
  write_rds("Out/IMF_FiscalResponseData.rds")
