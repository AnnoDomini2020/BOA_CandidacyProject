#### Libraries ####
library(tidyverse)
library(readxl)
library(jsonlite)
library(XML)
library(stringr)

#### Load ####
# Ship_Maintenance_Cleaned.xlsx and drive-download-20210608T162047Z-001.zip must be present in working directory for script to function.
unzip(zipfile = "drive-download-20210608T162047Z-001.zip", list = TRUE)  # Preview content of .zip file.
unzip(zipfile = "drive-download-20210608T162047Z-001.zip", list = FALSE) # Unzip said file.
StateTaxRates <- read_excel('StateTaxRates.xlsx')
Median_Income_County <- read_json(path = 'Median_Income_County.json', 
                                  simplifyVector = TRUE) %>% 
  data.frame(stringsAsFactors = TRUE)
ExportsByState2012 <- read_excel('Exports By State 2012.xlsx')
County_Tax_Rate <- read_excel('County_Tax_Rate.xlsx')
usa_county_list <- read_csv('usa_county_list.csv')
Unemployment_By_County <- read_excel('Unemployment_By_County.xlsx')
US_St_Cn_Table_Workforce_Wages <- xmlToDataFrame("US_St_Cn_Table_Workforce_Wages.xml")

Ship_Maintenance <- read_excel("Ship_Maintenance_Cleaned.xlsx")

#### County names ####
# Program common variable under format "County Name State" for two county datasets:
County_Tax_Rate <- County_Tax_Rate %>%
  mutate(CSN = NAME)
County_Tax_Rate$CSN <- str_replace(County_Tax_Rate$CSN, ",", "")
usa_county_list <- usa_county_list %>%
  mutate(CSN = paste(COUNTY, STATE, sep = " "))

# For third dataset, set column names and eliminate unneccessary row:
colnames(Median_Income_County) <- c("NAME", "B06011_001E", "State", "County")
Median_Income_County <- Median_Income_County[2:839,]
Median_Income_County <- Median_Income_County %>%
  mutate(CSN = NAME)
Median_Income_County$CSN <- str_replace(Median_Income_County$CSN, ",", "")

# For fourth dataset, include only non-static variables:
US_St_Cn_Table_Workforce_Wages_gen <- US_St_Cn_Table_Workforce_Wages %>%
  filter(Ownership == "Total Covered",
         Cnty != 999) %>%
  mutate(CSN = Area) %>%
  select(CSN, Area_Code, St, Cnty, St_Name, Annual_Average_Establishment_Count,
         Annual_Average_Employment, Annual_Total_Wages, Annual_Average_Weekly_Wage,
         Annual_Average_Pay, Total_Wage_Location_Quotient_Relative_to_US,
         Employment_Location_Quotient_Relative_to_US)

counties <- Unemployment_By_County$`County Name/State Abbreviation` # County name list.
abb2name <- function(x){                                            # Replace all state abbreviations with full names.
  for (c in x){
    newc <- ifelse(any(str_detect(c, state.abb)) == TRUE,
                   str_replace(c, state.abb[which(str_detect(c, state.abb) == TRUE)], 
                               state.name[which(str_detect(c, state.abb) == TRUE)]),
                   "District of Columbia")                          # Eliminate duplicate DC name.
    return(newc)
  }}
newcounties <- lapply(counties, abb2name) %>% unlist()              # List of county names without abbreviations.
Unemployment_By_County <- Unemployment_By_County %>%                # Save them to fifth dataset.
  mutate(CSN = newcounties)                                         # Eliminate commas in command below:
Unemployment_By_County$CSN <- str_replace(Unemployment_By_County$CSN, ",", "")

# Program new function to count CSN discrepencies between sets:
`%nin%` <- Negate(`%in%`)
sum(Median_Income_County$CSN %nin% County_Tax_Rate$CSN)               # 12
sum(Unemployment_By_County$CSN %nin% County_Tax_Rate$CSN)             # 13
sum(usa_county_list$CSN %nin% County_Tax_Rate$CSN)                    # 19
sum(US_St_Cn_Table_Workforce_Wages_gen$CSN %nin% County_Tax_Rate$CSN) # 52

# List discrepent CSN entries, treating County_Tax_Rate as the standard:
Median_Income_County[which(Median_Income_County$CSN %nin% County_Tax_Rate$CSN),]
Unemployment_By_County[which(Unemployment_By_County$CSN %nin% County_Tax_Rate$CSN),]
US_St_Cn_Table_Workforce_Wages_gen[which(US_St_Cn_Table_Workforce_Wages_gen$CSN 
                                         %nin% County_Tax_Rate$CSN),]

#### Median_Income_County ####
Median_Income_County[which(Median_Income_County$CSN %nin% County_Tax_Rate$CSN),]

# Correct DC and Dona Ana county names in County_Tax_Rate:
Median_Income_County$CSN[109] <- "District of Columbia"
str_subset(County_Tax_Rate$CSN, "District of Columbia")
str_which(County_Tax_Rate$CSN, "District of Columbia")
County_Tax_Rate$CSN[320] <- "District of Columbia"

Median_Income_County$CSN[439] <- "Dona Ana County New Mexico"
str_subset(County_Tax_Rate$CSN, "Ana County")
str_which(County_Tax_Rate$CSN, "Ana County")
County_Tax_Rate$CSN[1802] <- "Dona Ana County New Mexico"

# Eliminate Puerto Rican counties in Median_Income_County: (No useful information here or in any other sets.)
Median_Income_County <- Median_Income_County %>%
  filter(!is.na(B06011_001E)) %>%
  mutate(MedianIncome = B06011_001E)

# Discrepencies corrected:
sum(Median_Income_County$CSN %nin% County_Tax_Rate$CSN)

#### Unemployment_By_County ####
# Correct county names in Unemployment_By_County:
which(Unemployment_By_County$CSN %nin% County_Tax_Rate$CSN)
str_subset(County_Tax_Rate$CSN, "Anchorage")
Unemployment_By_County$CSN[70] <- "Anchorage Municipality Alaska"

str_subset(County_Tax_Rate$CSN, "Juneau")
Unemployment_By_County$CSN[78] <- "Juneau City and Borough Alaska"

str_subset(County_Tax_Rate$CSN, "Sitka")
Unemployment_By_County$CSN[90] <- "Sitka City and Borough Alaska"

str_subset(County_Tax_Rate$CSN, "Wrangell")
Unemployment_By_County$CSN[94] <- "Wrangell City and Borough Alaska"

str_subset(County_Tax_Rate$CSN, "Yakutat")
Unemployment_By_County$CSN[95] <- "Yakutat City and Borough Alaska"

str_subset(County_Tax_Rate$CSN, "San Francisco")
Unemployment_By_County$CSN[224] <- "San Francisco County California"

str_subset(County_Tax_Rate$CSN, "Broomfield")
Unemployment_By_County$CSN[252] <- "Broomfield County Colorado"

str_subset(County_Tax_Rate$CSN, "Denver")
Unemployment_By_County$CSN[261] <- "Denver County Colorado"

str_subset(County_Tax_Rate$CSN, "Honolulu")
Unemployment_By_County$CSN[548] <- "Honolulu County Hawaii"

str_subset(County_Tax_Rate$CSN, "Nantucket")
Unemployment_By_County$CSN[1226] <- "Nantucket County Massachusetts"

str_subset(County_Tax_Rate$CSN, "Philadelphia")
Unemployment_By_County$CSN[2294] <- "Philadelphia County Pennsylvania"

sum(Unemployment_By_County$CSN %nin% County_Tax_Rate$CSN)

#### US_St_Cn_Table_Workforce_Wages_gen ####
# Eliminate underscore, convert to numeric columns:
US_St_Cn_Table_Workforce_Wages_gen$Annual_Average_Employment <- 
  str_replace_all(US_St_Cn_Table_Workforce_Wages_gen$Annual_Average_Employment, "_", "") %>%
  as.numeric()
US_St_Cn_Table_Workforce_Wages_gen$Annual_Average_Establishment_Count <- 
  str_replace_all(US_St_Cn_Table_Workforce_Wages_gen$Annual_Average_Establishment_Count, "_", "") %>%
  as.numeric()
US_St_Cn_Table_Workforce_Wages_gen$Annual_Total_Wages <- 
  str_replace_all(US_St_Cn_Table_Workforce_Wages_gen$Annual_Total_Wages, "_", "") %>%
  as.numeric()
US_St_Cn_Table_Workforce_Wages_gen$Annual_Average_Weekly_Wage <- 
  str_replace_all(US_St_Cn_Table_Workforce_Wages_gen$Annual_Average_Weekly_Wage, "_", "") %>%
  as.numeric()
US_St_Cn_Table_Workforce_Wages_gen$Annual_Average_Pay <- 
  str_replace_all(US_St_Cn_Table_Workforce_Wages_gen$Annual_Average_Pay, "_", "") %>%
  as.numeric()

US_St_Cn_Table_Workforce_Wages_gen$CSN <- as.character(US_St_Cn_Table_Workforce_Wages_gen$CSN) # Convert from factor to character.
sum(US_St_Cn_Table_Workforce_Wages_gen$CSN %nin% County_Tax_Rate$CSN)                          # 50
US_St_Cn_Table_Workforce_Wages_gen$CSN[which(US_St_Cn_Table_Workforce_Wages_gen$CSN %nin%      # List CSN entries discrepent from County_Tax_Rate.
                                               County_Tax_Rate$CSN)]
which(US_St_Cn_Table_Workforce_Wages_gen$CSN %nin%                                             # List indices for discrepent CSN entries.
        County_Tax_Rate$CSN)

# Correct discrepencies in US_St_Cn_Table_Workforce_Wages_gen and County_Tax_Rate:
str_subset(County_Tax_Rate$CSN, "Maui")
US_St_Cn_Table_Workforce_Wages_gen$CSN[550] <- "Maui County Hawaii"
US_St_Cn_Table_Workforce_Wages_gen$CSN[550]

str_subset(County_Tax_Rate$CSN, "De Witt")
str_which(County_Tax_Rate$CSN, "De Witt")
County_Tax_Rate$CSN[615] <- "DeWitt County Illinois"

US_St_Cn_Table_Workforce_Wages_gen$CSN[740]
str_subset(County_Tax_Rate$CSN, "Grange")
US_St_Cn_Table_Workforce_Wages_gen$CSN[740] <- "LaGrange County Indiana"

US_St_Cn_Table_Workforce_Wages_gen$CSN[859]
str_subset(County_Tax_Rate$CSN, "Brien")
US_St_Cn_Table_Workforce_Wages_gen$CSN[859] <- "O'Brien County Iowa"

US_St_Cn_Table_Workforce_Wages_gen$CSN[944]
str_subset(County_Tax_Rate$CSN, "Pherson")
US_St_Cn_Table_Workforce_Wages_gen$CSN[944] <- "McPherson County Kansas"

US_St_Cn_Table_Workforce_Wages_gen$CSN[1216]
str_subset(County_Tax_Rate$CSN, "Baltimore")
str_which(County_Tax_Rate$CSN, "Baltimore city")
County_Tax_Rate$CSN[1217] <- "Baltimore City Maryland"

US_St_Cn_Table_Workforce_Wages_gen$CSN[1350]
str_subset(County_Tax_Rate$CSN, "Parle")
US_St_Cn_Table_Workforce_Wages_gen$CSN[1350] <- "Lac qui Parle County Minnesota"

US_St_Cn_Table_Workforce_Wages_gen$CSN[1352]
str_subset(County_Tax_Rate$CSN, "Lake of")
US_St_Cn_Table_Workforce_Wages_gen$CSN[1352] <- "Lake of the Woods County Minnesota"

US_St_Cn_Table_Workforce_Wages_gen$CSN[1597]
str_subset(County_Tax_Rate$CSN, "St. Louis")
str_which(County_Tax_Rate$CSN, "St. Louis city")
County_Tax_Rate$CSN[1598] <- "St. Louis City Missouri"

US_St_Cn_Table_Workforce_Wages_gen$CSN[1812]
str_subset(County_Tax_Rate$CSN, "McKinley")
US_St_Cn_Table_Workforce_Wages_gen$CSN[1812] <- "McKinley County New Mexico"

US_St_Cn_Table_Workforce_Wages_gen$CSN[2285] <- "McKean County Pennsylvania"

US_St_Cn_Table_Workforce_Wages_gen$CSN[2915]
County_Tax_Rate$CSN <- str_replace(County_Tax_Rate$CSN, "city", "City")

US_St_Cn_Table_Workforce_Wages_gen$CSN[3066]
str_subset(County_Tax_Rate$CSN, "Fond du Lac")
US_St_Cn_Table_Workforce_Wages_gen$CSN[3066] <- "Fond du Lac County Wisconsin"

sum(US_St_Cn_Table_Workforce_Wages_gen$CSN %nin% County_Tax_Rate$CSN)

# Convert factors to numeric variables:
US_St_Cn_Table_Workforce_Wages_gen$Employment_Location_Quotient_Relative_to_US <-
  as.numeric(US_St_Cn_Table_Workforce_Wages_gen$Employment_Location_Quotient_Relative_to_US)
US_St_Cn_Table_Workforce_Wages_gen$Total_Wage_Location_Quotient_Relative_to_US <-
  as.numeric(US_St_Cn_Table_Workforce_Wages_gen$Total_Wage_Location_Quotient_Relative_to_US)

#### Merge county-level data ####
County_Tax_Rate$NAME <- str_replace(County_Tax_Rate$NAME, ",.*", "") # Keep entry for county names without state names.
GeneralSet <- County_Tax_Rate %>%
  mutate(LocalTax = `Local Tax Rate`) %>%
  select(CSN, STATE, NAME, GEO_ID, LocalTax)
Median_Income_County2 <- Median_Income_County %>%
  select(CSN, MedianIncome)
GeneralSet <- left_join(GeneralSet, Median_Income_County2, by = "CSN")
GeneralSet <- left_join(GeneralSet, US_St_Cn_Table_Workforce_Wages_gen, by = "CSN")
GeneralSet <- left_join(GeneralSet, Unemployment_By_County, by = "CSN")

# usa_county_list excluded due to lack of unique value and useful information for other sets.

#### State-level data cleaning ####
# Now we work on sets 6-7. Correct state names:
StateTaxRates$State[which(StateTaxRates$State == "South Dakota (c)")] <- "South Dakota"
StateTaxRates$State[which(StateTaxRates$State == "Montana (d)")] <- "Montana"
StateTaxRates$State[which(StateTaxRates$State == "Hawaii (c)")] <- "Hawaii"
StateTaxRates$State[which(StateTaxRates$State == "D.C.")] <- "District of Columbia"
StateTaxRates$State[which(StateTaxRates$State == "Utah (b)")] <- "Utah"
StateTaxRates$State[which(StateTaxRates$State == "California (b)")] <- "California"
StateTaxRates$State[which(StateTaxRates$State == "New Jersey (e)")] <- "New Jersey"
StateTaxRates$State[which(StateTaxRates$State == "Virginia (b)")] <- "Virginia"
StateTaxRates$State[which(StateTaxRates$State == "New Mexico (c)")] <- "New Mexico"

# Convert tax rates to numeric variables and percentages:
StateTaxRates2 <- StateTaxRates %>%
  mutate(StateTax = 100*as.numeric(`State Tax Rate`),
         StateMeanLocalTax = 100*as.numeric(`Avg. Local Tax Rate (a)`),
         StateCombinedMeanTax = 100*as.numeric(`Combined Tax Rate`))

# Correcting [income] tax rates: (via smartasset.com)
StateTaxRates2$StateTax[which(StateTaxRates2$State == "Alaska")] <- 0

StateTaxRates2$StateTax[which(StateTaxRates2$State == "New Hampshire")] <- 0
StateTaxRates2$StateMeanLocalTax[which(StateTaxRates2$State == "New Hampshire")] <- 0
StateTaxRates2$StateCombinedMeanTax[which(StateTaxRates2$State == "New Hampshire")] <- 0

StateTaxRates2$StateTax[which(StateTaxRates2$State == "Montana")] <- 4.57
StateTaxRates2$StateMeanLocalTax[which(StateTaxRates2$State == "Montana")] <- 0
StateTaxRates2$StateCombinedMeanTax[which(StateTaxRates2$State == "Montana")] <- 4.57

StateTaxRates2$StateTax[which(StateTaxRates2$State == "Oregon")] <- 7.00
StateTaxRates2$StateMeanLocalTax[which(StateTaxRates2$State == "Oregon")] <- 0
StateTaxRates2$StateCombinedMeanTax[which(StateTaxRates2$State == "Oregon")] <- 7.00

StateTaxRates2$StateTax[which(StateTaxRates2$State == "Delaware")] <- 4.73
StateTaxRates2$StateMeanLocalTax[which(StateTaxRates2$State == "Delaware")] <- 0
StateTaxRates2$StateCombinedMeanTax[which(StateTaxRates2$State == "Delaware")] <- 4.73

# Eliminate non-static values in export dataset:
ExportsByState2012 <- ExportsByState2012 %>%
  select(!c(`CFS Metro Area`, `Export mode category code...5`, `Export mode category code...7`,
            `Commodity code`, `Meaning of Commodity code`, `Geo Footnote`, `Ton-miles (percent change)`,
            `Standard error for Ton-miles (percent change)`))

# Convert character to numeric variables:
ExportsByState2012$`Ton-miles (millions)` <- as.numeric(ExportsByState2012$`Ton-miles (millions)`)
ExportsByState2012$`Coefficient of variation for Ton-miles` <- as.numeric(ExportsByState2012$`Coefficient of variation for Ton-miles`)
ExportsByState2012$`Tons (thousands)` <- as.numeric(ExportsByState2012$`Tons (thousands)`)
ExportsByState2012$`Tons (percent change)` <- as.numeric(ExportsByState2012$`Tons (percent change)`)
ExportsByState2012$`Standard error for Tons (percent change)` <- as.numeric(ExportsByState2012$`Standard error for Tons (percent change)`)
ExportsByState2012$`Coefficient of variation for Tons` <- as.numeric(ExportsByState2012$`Coefficient of variation for Tons`)
ExportsByState2012$`Value ($ million)` <- as.numeric(ExportsByState2012$`Value ($ million)`)
ExportsByState2012$`Value (percent change)` <- as.numeric(ExportsByState2012$`Value (percent change)`)
ExportsByState2012$`Standard error for Value (percent change)` <- as.numeric(ExportsByState2012$`Standard error for Value (percent change)`)
ExportsByState2012$`Coefficient of variation for Value` <- as.numeric(ExportsByState2012$`Coefficient of variation for Value`)

# Create separate provisional dataset for 2007 exports:
ExportsByState2007 <- ExportsByState2012 %>%
  filter(Year == 2007) %>%
  select(!c(Year, `Ton-miles (millions)`, `Coefficient of variation for Ton-miles`,
            `Tons (percent change)`, `Standard error for Tons (percent change)`,
            `Value (percent change)`, `Standard error for Value (percent change)`),
         `Geo Id`, `Geographic Area Name`, 
         `2007 Tons (thousands)` = `Tons (thousands)`,
         `2007 Coefficient of variation for Tons` = `Coefficient of variation for Tons`,
         `2007 Value ($ million)` = `Value ($ million)`,
         `2007 Coefficient of variation for Value` = `Coefficient of variation for Value`)

ExportsByState2012 <- ExportsByState2012 %>%        # Eliminate 2007 data from 2012 dataset.
  filter(Year == 2012) %>%
  select(!Year)
ExportsByState2012 <- left_join(ExportsByState2012, # Re-unite export datasets.
                                ExportsByState2007[, 2:6], by = "Geographic Area Name")
colnames(ExportsByState2012)[2] <- "STATE"

#### Merge county- and state-level data ####
StateTaxRates2 <- StateTaxRates2 %>%
  mutate(STATE = State) %>%
  select(STATE, StateTax, StateMeanLocalTax, StateCombinedMeanTax)
GeneralSet <- left_join(GeneralSet, StateTaxRates2, by = "STATE")
GeneralSet <- left_join(GeneralSet, ExportsByState2012, by = "STATE")

colnames(GeneralSet)[21] <- "STATE_GEO_ID"
GeneralSet$LocalTax <- as.numeric(GeneralSet$LocalTax)

#### Export and Query ####
# 44 variables for 3142 counties:
write.csv(GeneralSet, "OfficeLocationDataCompilation.csv", row.names = TRUE)
FullCountyData <- read.csv("OfficeLocationDataCompilation.csv")

# Find all counties with a state tax rate below 4% and an unemployment rate above 6%. Sort by annual total wages from highest to lowest:
FullCountyData %>%
  filter(StateTax < 4 & Unemployment.Rate > 6) %>%
  arrange(desc(Annual_Total_Wages)) %>%
  select(CSN)

# Find all counties with an annual average pay under $40K and a local tax rate under 3%. Sort from highest population to lowest:
FullCountyData %>%
  filter(Annual_Average_Pay < 40000 & LocalTax < 3) %>%
  arrange(desc(Labor.Force)) %>%
  select(CSN) %>% head()

#### Ship Maintenance ####
# Split sets between identified and unidentified service order ID numbers:
SM_Ident <- Ship_Maintenance %>% 
  filter(!is.na(`Service Order ID`))
SM_Unident <- Ship_Maintenance %>%
  filter(is.na(`Service Order ID`))

# Generate a new ID for unidentified orders (ignoring variance within orders):
SMU_Left <- SM_Unident[,1:27]
SM_Unident$`Service Order ID` <- 50625769 + cumsum(!duplicated(SMU_Left))

# Merge previously and newly identified orders, condense by service order ID:
Ship_Maintenance3 <- rbind(SM_Ident, SM_Unident)
Ship_Maintenance3 <- Ship_Maintenance3 %>%
  group_by(`Service Order ID`) %>%
  mutate(TotalQuantity = sum(Quantity)) %>%
  ungroup() %>%
  select(!c(`Item Commercial Cost $`, `Item Factory Cost $`, Quantity)) %>%
  distinct()

# Correct remaining unidentified orders:
Ship_Maintenance3[40744,] <- Ship_Maintenance3[40744,] %>%
  mutate(`Travel Hours` = 0.25, `Total Hours` = 1.25,
         `Travel Cost $` = mean(0, 77.75),
         `Total Commercial Call Cost $` = mean(155.5, 233.25),
         `Total Factory Call Cost $` = mean(460.5, 382.75))
Ship_Maintenance3 <- Ship_Maintenance3[c(1:40744, 40746:nrow(Ship_Maintenance3)),]

Ship_Maintenance3$`Service Order ID`[which(duplicated(Ship_Maintenance3$`Service Order ID`) == TRUE)] <- 
  seq(max(Ship_Maintenance3$`Service Order ID`)+1, max(Ship_Maintenance3$`Service Order ID`)+12, 1)

# Export and read condensed dataset:
write.csv(Ship_Maintenance3, "ShipMaintenanceSimplified.csv", row.names = TRUE)
SMS <- read.csv("ShipMaintenanceSimplified.csv")