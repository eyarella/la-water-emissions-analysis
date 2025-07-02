# data cleaning 

# load packages
source("setup.R")

EBEW <- read_csv("data/Existing_Buildings_Energy___Water_Efficiency__EBEWE__Program_20250609.csv")
# Check column names or view structure
glimpse(EBEW)
# Convert structure of column from char to num
EBEW$`TOTAL WATER USE (kgal)` <- as.numeric(gsub(",", "", EBEW$`TOTAL WATER USE (kgal)`))
EBEW$`CARBON DIOXIDE EMISSIONS (Metric Ton CO2e)` <- as.numeric(gsub(",", "", EBEW$`CARBON DIOXIDE EMISSIONS (Metric Ton CO2e)`))
# Create TWU data frame 
EBEW_TWU <- EBEW %>% 
  filter(!is.na(`TOTAL WATER USE (kgal)`),
         `TOTAL WATER USE (kgal)` > 0,
         `PROGRAM YEAR` == 2024)
# Create CO2 data frame
EBEW_CO <- EBEW %>% 
  filter(!is.na(`CARBON DIOXIDE EMISSIONS (Metric Ton CO2e)`),
         `CARBON DIOXIDE EMISSIONS (Metric Ton CO2e)` > 0,
         `PROGRAM YEAR` == 2024) 
# DF of Top Zip Codes with the highest Total Water Use in 2024
TabZipTWU <- EBEW %>% 
  filter(!is.na(`TOTAL WATER USE (kgal)`),!is.na(`POSTAL CODE`),
         `PROGRAM YEAR` == 2024) %>% 
  group_by(`POSTAL CODE`) %>% 
  summarise(total_TWU = sum(`TOTAL WATER USE (kgal)`, na.rm=TRUE)) %>% 
  arrange(desc(total_TWU)) %>% 
  slice_head(n=3) 

# DF of Top Zip Codes with the highest Carbon Emissions in 2024
TabZipCO<- EBEW %>% 
  filter(!is.na(`CARBON DIOXIDE EMISSIONS (Metric Ton CO2e)`),
         !is.na(`POSTAL CODE`), 
         `PROGRAM YEAR` == 2024) %>% 
  group_by(`POSTAL CODE`) %>% 
  summarise(total_CO = sum(`CARBON DIOXIDE EMISSIONS (Metric Ton CO2e)`, na.rm=TRUE)) %>% 
  arrange(desc(total_CO)) %>% 
  slice_head(n=3)
# Create DF of several variables
EBEW_FILT <- EBEW %>%
  select(`YEAR BUILT`,`TOTAL WATER USE (kgal)`,`CARBON DIOXIDE EMISSIONS (Metric Ton CO2e)`) %>% 
  filter(
    !is.na(`YEAR BUILT`),
    !is.na(`TOTAL WATER USE (kgal)`),
    !is.na(`CARBON DIOXIDE EMISSIONS (Metric Ton CO2e)`),
  ) 
# Create DF of several vars with more specific filters to show better insights
EBEW_FILT <- EBEW %>%
  select(`YEAR BUILT`,`TOTAL WATER USE (kgal)`,`CARBON DIOXIDE EMISSIONS (Metric Ton CO2e)`,`PROGRAM YEAR`) %>% 
  filter(
    `PROGRAM YEAR` == 2024,
    !is.na(`YEAR BUILT`),
    !is.na(`TOTAL WATER USE (kgal)`),
    !is.na(`CARBON DIOXIDE EMISSIONS (Metric Ton CO2e)`),
    `YEAR BUILT` >= 1800, #there was a building built in 1190, removed that data because its prob wrong
    `CARBON DIOXIDE EMISSIONS (Metric Ton CO2e)` < 65000, # remove extreme outliers
    `TOTAL WATER USE (kgal)`< 1000000
  )
# The data for CO2 emissions has too many large outliers, so I need to exclude them. I will do so by Computing thresholds for outliers of CO emissions, then filtering CO2 emissions that are larger than the threshold I computed.  
thresh_CO2 <- quantile(EBEW_FILT$`CARBON DIOXIDE EMISSIONS (Metric Ton CO2e)`, probs = 0.99, na.rm = TRUE)
EBEW_FILT <- EBEW %>%
  select(`YEAR BUILT`,`TOTAL WATER USE (kgal)`,`CARBON DIOXIDE EMISSIONS (Metric Ton CO2e)`) %>% 
  filter(
    !is.na(`YEAR BUILT`),
    !is.na(`TOTAL WATER USE (kgal)`),
    !is.na(`CARBON DIOXIDE EMISSIONS (Metric Ton CO2e)`),
    `YEAR BUILT` >= 1800, #there was a building built in 1190, removed that data because its probably wrong
    `CARBON DIOXIDE EMISSIONS (Metric Ton CO2e)` <= thresh_CO2, # remove extreme outlier
    `TOTAL WATER USE (kgal)`< 1000000) 
# Rather than use thresholds I computed before, I used a number to exclude the biggest outliers to better focus on the majority of the data. I did not use thresholds because it excluded too much of the data. 
EBEW_FILT <- EBEW %>%
  select(`YEAR BUILT`,`TOTAL WATER USE (kgal)`,`CARBON DIOXIDE EMISSIONS (Metric Ton CO2e)`) %>% 
  filter(
    !is.na(`YEAR BUILT`),
    !is.na(`TOTAL WATER USE (kgal)`),
    !is.na(`CARBON DIOXIDE EMISSIONS (Metric Ton CO2e)`),
    `YEAR BUILT` >= 1800, #there was a building built in 1190, removed that data because its probably wrong
    `CARBON DIOXIDE EMISSIONS (Metric Ton CO2e)` < 65000, # remove extreme outlier
    `TOTAL WATER USE (kgal)`< 1000000) 
EBEW_FILT$`YEAR BUILT` <- as.numeric(EBEW_FILT$`YEAR BUILT`)
EBEW_FILT <- EBEW_FILT %>% 
  mutate(yr_built = cut(`YEAR BUILT`,
                        breaks = seq(1875, 2025, by = 25),
                        right = FALSE,
                        labels = paste(seq(1875, 2025 - 25, by = 25), 
                                       seq(1875 + 24, 2025 - 1, by = 25), 
                                       sep = "-")))
# Create DF of several cols, filter out NA values and filter out outliers for both CO2 emissions and TWU
EBEW_long <- EBEW %>% 
  select(`PROGRAM YEAR`,`YEAR BUILT`, `CARBON DIOXIDE EMISSIONS (Metric Ton CO2e)`,`TOTAL WATER USE (kgal)`) %>% 
  filter(
    !is.na(`YEAR BUILT`),
    !is.na(`CARBON DIOXIDE EMISSIONS (Metric Ton CO2e)`),
    !is.na(`TOTAL WATER USE (kgal)`),
    `YEAR BUILT` >= 1800,
    `CARBON DIOXIDE EMISSIONS (Metric Ton CO2e)` < 6500, 
    `TOTAL WATER USE (kgal)` < 1000000)
# Create column with the function mutate. This column is the building age, where we subtract the year the building was built from the program year = the age of when the data was collected. 
EBEW_long <- EBEW_long %>%
  mutate(Building_Age = `PROGRAM YEAR` - `YEAR BUILT`)  # How old building was when data was collected



# I want to do noncompliance vs building type, create data frame to focus on these variables
# can also include gross building floor area OR occupancy vs compliance 
EBEW_COMP <- EBEW %>%
  select(`PROGRAM YEAR`,`BUILDING ADDRESS`,`PROPERTY TYPE`,`COMPLIANCE STATUS`,`GROSS BUILDING FLOOR AREA (ft²)`,OCCUPANCY,`TOTAL WATER USE (kgal)`,`CARBON DIOXIDE EMISSIONS (Metric Ton CO2e)`) %>% 
  filter(
    !is.na(`COMPLIANCE STATUS`),
    !is.na(`GROSS BUILDING FLOOR AREA (ft²)`),
    !is.na(OCCUPANCY),
    !is.na(`TOTAL WATER USE (kgal)`),
    !is.na(`CARBON DIOXIDE EMISSIONS (Metric Ton CO2e)`),
    !is.na(`PROPERTY TYPE`),
    !is.na(`BUILDING ADDRESS`),
    `COMPLIANCE STATUS` == "NOT COMPLIED") 
EBEW_COMPSUM <- EBEW_COMP %>% 
  filter( 
    `PROGRAM YEAR` == 2024) %>% 
  group_by(`PROPERTY TYPE`) %>% 
  summarise(
    totalCO2 = sum(`CARBON DIOXIDE EMISSIONS (Metric Ton CO2e)`),
    totalTWU = sum(`TOTAL WATER USE (kgal)`)) %>% 
  arrange(desc(totalCO2)) 
top5_CO_polluters <- EBEW_COMPSUM %>% 
  arrange(desc(totalCO2)) %>%
  slice_head(n=5)
top3_TWU_polluters <- EBEW_COMPSUM %>% 
  arrange(desc(totalTWU)) %>%
  slice_head(n=3)



# how has compliance shifted over the years?
compliance_trend <- EBEW %>% 
  select(`PROGRAM YEAR`, `COMPLIANCE STATUS`) %>% 
  filter( 
    !is.na(`COMPLIANCE STATUS`),
    !is.na(`PROGRAM YEAR`)) %>% 
  group_by(`PROGRAM YEAR`, `COMPLIANCE STATUS`) %>% 
  summarise(count = n(), .groups = "drop") %>%
  group_by(`PROGRAM YEAR`) %>% 
  mutate(
    total_buildings = sum(count),
    non_comp_count = sum(count[`COMPLIANCE STATUS` == "NOT COMPLIED"]),
    non_comp_rate = non_comp_count / total_buildings
  ) %>% 
  ungroup() %>% 
  filter(`COMPLIANCE STATUS` == "NOT COMPLIED")



# Which building pollutes the most? Get address
EBEW_AD<- EBEW_COMP %>% 
  distinct(`BUILDING ADDRESS`, .keep_all = TRUE) %>% 
  filter( 
    `PROGRAM YEAR` == 2024) 
top3_CO2_AD<- EBEW_AD %>% 
  arrange(desc(`CARBON DIOXIDE EMISSIONS (Metric Ton CO2e)`)) %>% 
  slice_head(n = 3)
top3_TWU_AD<- EBEW_AD %>% 
  arrange(desc(`TOTAL WATER USE (kgal)`)) %>% 
  slice_head(n = 3)



