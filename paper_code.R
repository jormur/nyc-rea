# REE Final Paper

# Load libraries
library(readxl)
library(readr)
library(dplyr)
library(tidyr)

# Read data
queens <- read_excel("rollingsales_queens.xlsx", skip = 4)
statenisland <- read_excel("rollingsales_statenisland.xlsx", skip = 4)
manhattan <- read_excel("rollingsales_manhattan.xlsx", skip = 4)
brooklyn <- read_excel("rollingsales_brooklyn.xlsx", skip = 4)
bronx <- read_excel("rollingsales_bronx.xlsx", skip = 4)

# Rename boroughs
queens$BOROUGH[queens$BOROUGH == "4"] <- "Queens"
statenisland$BOROUGH[statenisland$BOROUGH == "5"] <- "Staten Island"
manhattan$BOROUGH[manhattan$BOROUGH == "1"] <- "Manhattan"
brooklyn$BOROUGH[brooklyn$BOROUGH == "3"] <- "Brooklyn"
bronx$BOROUGH[bronx$BOROUGH == "2"] <- "Bronx"

# Combine all boroughs into one table
NYC <- rbind(queens, statenisland, manhattan, brooklyn, bronx)

# Change column names with spaces to underscores
colnames(NYC) <- gsub(" ", "_", colnames(NYC))

# Format columns as numeric
numeric_columns <- c("SALE_PRICE", "RESIDENTIAL_UNITS", "COMMERCIAL_UNITS",
                     "TOTAL_UNITS", "LAND_SQUARE_FEET", "GROSS_SQUARE_FEET",
                     "YEAR_BUILT")

NYC[, numeric_columns] <- lapply(NYC[, numeric_columns], as.numeric)

# Create property age column
NYC$SALE_YEAR <- substr(NYC$SALE_DATE, 1, 4)
NYC$SALE_YEAR <- as.numeric(NYC$SALE_YEAR)
NYC$PROPERTY_AGE <- NYC$SALE_YEAR - NYC$YEAR_BUILT

# Remove duplicate rows
NYC <- NYC[!duplicated(NYC), ]

# Create price per square foot column
NYC$PRICE_PER_SQ_FT <- NYC$SALE_PRICE / NYC$GROSS_SQUARE_FEET
NYC$PRICE_PER_SQ_FT <- as.numeric(NYC$PRICE_PER_SQ_FT)

# Filter out NA and zero values
NYC <- NYC %>%
  filter(SALE_PRICE != 0 & !is.na(SALE_PRICE) &
           GROSS_SQUARE_FEET != 0 & !is.na(GROSS_SQUARE_FEET) &
           LAND_SQUARE_FEET != 0 & !is.na(LAND_SQUARE_FEET) &
           PRICE_PER_SQ_FT != 0 & !is.na(PRICE_PER_SQ_FT))

NYC <- NYC %>%
  mutate(unit_type = case_when(
    RESIDENTIAL_UNITS > 0 & COMMERCIAL_UNITS == 0 ~ "Residential Only",
    RESIDENTIAL_UNITS == 0 & COMMERCIAL_UNITS > 0 ~ "Commercial Only",
    RESIDENTIAL_UNITS > 0 & COMMERCIAL_UNITS > 0 ~ "Both Residential and Commercial",
    TRUE ~ "Other"
  ))

NYC <- NYC %>%
  filter(unit_type != "Other")

NYC <- NYC %>%
  mutate(BUILDING_TO_LAND_RATIO = GROSS_SQUARE_FEET/LAND_SQUARE_FEET) %>% 
  mutate(BUILDING_TO_LAND_RATIO = as.numeric(BUILDING_TO_LAND_RATIO))

NYC <- NYC %>%
  group_by(NEIGHBORHOOD) %>%
  mutate(avg_commercial_units = mean(COMMERCIAL_UNITS, na.rm = TRUE))

# Export NYC to csv
# write.csv(NYC, "NYC.csv")

# Read CSV
# NYC <- read_csv("NYC.csv")

#### Summary Statistics ####
#H1: Newer buildings (as measured by year built) sell for higher prices, controlling for other factors

#Avg Sale Price by decade of year built, excluding zeroes and NAs
NYC %>%
  mutate(YEAR_BUILT = floor(YEAR_BUILT/10)*10) %>%
  group_by(YEAR_BUILT) %>%
  summarise(mean = mean(SALE_PRICE, na.rm = TRUE))%>% 
  arrange(YEAR_BUILT) %>% 
  print(n = 100)

#Avg Price per square foot by decade of year built, excluding zeroes and NAs
NYC %>%
  mutate(YEAR_BUILT = floor(YEAR_BUILT/10)*10) %>%
  group_by(YEAR_BUILT) %>%
  summarise(mean = mean(SALE_PRICE/GROSS_SQUARE_FEET, na.rm = TRUE))%>% 
  arrange(YEAR_BUILT) %>% 
  print(n = 100)

#H2: Properties in neighborhoods with more commercial units sell for higher prices, controlling for other factors

#Avg Sale Price by building use by borough, excluding zeroes and NAs
NYC %>%
  filter(unit_type != "Other") %>%
  group_by(BOROUGH, unit_type) %>%
  summarise(avg_sale_price = mean(SALE_PRICE, na.rm = TRUE)) %>%
  pivot_wider(names_from = unit_type, values_from = avg_sale_price)

#H3: Larger lot sizes increase sales prices, controlling for other factors

#Compare land square footage to gross square footage by borough, excluding zeroes and NAs
NYC %>%
  group_by(BOROUGH) %>%
  summarise(mean = mean(BUILDING_TO_LAND_RATIO, na.rm = TRUE))

#Avg price per square foot by building to land ratio, excluding zeroes and NAs
#Aggregated by 0.1 intervals and excluding outliers in the top 1% and bottom 1%
NYC %>%
  mutate(BUILDING_TO_LAND_RATIO = floor(BUILDING_TO_LAND_RATIO*10)/10) %>%
  filter(BUILDING_TO_LAND_RATIO > 0) %>%
  filter(BUILDING_TO_LAND_RATIO < 2) %>%
  group_by(BUILDING_TO_LAND_RATIO) %>%
  summarise(mean = mean(SALE_PRICE/GROSS_SQUARE_FEET, na.rm = TRUE)) %>%
  arrange(BUILDING_TO_LAND_RATIO) %>%
  print(n = 100)


#### Regression Analysis ####
#H1: Newer buildings (as measured by year built) sell for higher prices, controlling for other factors
#The price per sq ft will be used as the outcome variable here

h1_est <- NYC %>%
  filter(!is.na(PRICE_PER_SQ_FT) & is.finite(PRICE_PER_SQ_FT)) %>%
  mutate(BOROUGH = as.factor(BOROUGH),
         unit_type = as.factor(unit_type)) %>%
  lm(PRICE_PER_SQ_FT ~ PROPERTY_AGE + BOROUGH + RESIDENTIAL_UNITS 
     + COMMERCIAL_UNITS + LAND_SQUARE_FEET + GROSS_SQUARE_FEET + unit_type, data = .)

summary(h1_est)

#H2: Properties in neighborhoods with more commercial units sell for higher prices, controlling for other factors
#The price per sq ft will be used as the outcome variable here
#Additionally, to consider the effects of proximity effects, the data will be grouped by neighborhood
#After grouping, the number of commercial units in the neighborhood will be used as the predictor variable

h2_est <- NYC %>%
  filter(!is.na(PRICE_PER_SQ_FT) & is.finite(PRICE_PER_SQ_FT)) %>%
  mutate(BOROUGH = as.factor(BOROUGH),
         unit_type = as.factor(unit_type)) %>%
  lm(PRICE_PER_SQ_FT ~ avg_commercial_units + BOROUGH + RESIDENTIAL_UNITS 
     + COMMERCIAL_UNITS + LAND_SQUARE_FEET + GROSS_SQUARE_FEET + unit_type, data = .)

summary(h2_est)

#H3: Larger lot sizes increase sales prices, controlling for other factors
#The price per sq ft will be used as the outcome variable here

h3_est <- NYC %>%
  filter(!is.na(PRICE_PER_SQ_FT) & is.finite(PRICE_PER_SQ_FT)) %>%
  mutate(BOROUGH = as.factor(BOROUGH),
         unit_type = as.factor(unit_type)) %>%
  lm(PRICE_PER_SQ_FT ~ BUILDING_TO_LAND_RATIO + BOROUGH + RESIDENTIAL_UNITS 
     + COMMERCIAL_UNITS + LAND_SQUARE_FEET + GROSS_SQUARE_FEET + unit_type, data = .)

summary(h3_est)
library(broom)
tidy(h3_est)









