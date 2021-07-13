# Header =======================================================================
# Author: Pablo Reyes Moctezuma (https://github.com/pablorm296/)
# Description: This script makes a quick exploratory analysis on the data
# Set up =======================================================================
# Load packages
library(tidyverse)
library(GGally)

# Load data ====================================================================

IMF_DATA <- read_rds("Out/IMF_FiscalResponseData.rds")

# Exploratory Analysis =========================================================

# Create an empty list of plots
Plots <- list()

## Poverty and above-the-line measures ----

IMF_DATA %>%
  select(PGDP_ALM_ASFR_Subtotal, Poverty_190_Value, GDP_Value) %>%
  ggpairs(columnLabels = c("Above-the-line measures",
                           "% Population under $1.90",
                           "GDP")) -> Plots[["ALM_Poverty_Corrplot"]]

# Poverty 1.90 line
IMF_DATA %>%
  ggplot() +
  geom_point(aes(x = Poverty_190_Value/100, y = PGDP_ALM_ASFR_Subtotal, 
                 size = GDP_Value)) + 
  geom_smooth(aes(x = Poverty_190_Value/100, y = PGDP_ALM_ASFR_Subtotal)) +
  scale_y_continuous(labels = scales::percent) +
  scale_x_continuous(labels = scales::percent) +
  scale_size_continuous(range = c(1, 10)) +
  labs(title = "Does bigger economies (with fewer poor people) deployed bigger fiscal responses?",
       subtitle = "Share of population living under the poverty line ($1.90),\nAbove-the-line measures (as share of 2020 GDP),\nand GDP.",
       x = "Share of population living under the poverty line",
       y = "Above-the-line measures (as share of 2020 GDP)",
       size = "GDP (2020)") -> Plots[["ALM_Poverty190"]]

# Poverty 3.20 line
IMF_DATA %>%
  ggplot() +
  geom_point(aes(x = Poverty_320_Value/100, y = PGDP_ALM_ASFR_Subtotal, 
                 size = GDP_Value)) + 
  geom_smooth(aes(x = Poverty_320_Value/100, y = PGDP_ALM_ASFR_Subtotal)) +
  scale_y_continuous(labels = scales::percent) +
  scale_x_continuous(labels = scales::percent) +
  scale_size_continuous(range = c(1, 10)) +
  labs(title = "Does bigger economies (with fewer poor people) deployed bigger fiscal responses?",
       subtitle = "Share of population living under the poverty line ($3.20),\nAbove-the-line measures (as share of 2020 GDP),\nand GDP.",
       x = "Share of population living under the poverty line",
       y = "Above-the-line measures (as share of 2020 GDP)",
       size = "GDP (2020)") -> Plots[["ALM_Poverty320"]]

## Gini and above-the-line measures ----

IMF_DATA %>%
  select(PGDP_ALM_ASFR_Subtotal, GINI_Value, GDP_Value) %>%
  ggpairs(columnLabels = c("Above-the-line measures",
                           "GINI_Value",
                           "GDP")) -> Plots[["ALM_Gini_Corrplot"]]

# Poverty 3.20 line
IMF_DATA %>%
  ggplot() +
  geom_point(aes(x = GINI_Value, y = PGDP_ALM_ASFR_Subtotal, 
                 size = GDP_Value)) + 
  geom_smooth(aes(x = GINI_Value, y = PGDP_ALM_ASFR_Subtotal)) +
  scale_y_continuous(labels = scales::percent) +
  scale_size_continuous(range = c(1, 10)) +
  labs(title = "Does bigger economies (with less inequality) deployed bigger fiscal responses?",
       subtitle = "Share of population living under the poverty line ($3.20),\nAbove-the-line measures (as share of 2020 GDP),\nand GDP.",
       x = "Gini Index",
       y = "Above-the-line measures (as share of 2020 GDP)",
       size = "GDP (2020)") -> Plots[["ALM_Gini"]]

