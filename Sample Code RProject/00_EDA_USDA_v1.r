
## File Created by R. Hall 12/9/2022
##
## Purpose: This file brings in the USDA data to summarize crop information by county

## 

##########
## Step 0: Load the required packages
##########

library("tidyverse")
library("dplyr")
library("lubridate")
library("stringr")


##########
## Step 1: Bring in the data
##########

####
##  Bring in the NASS county level summary data. 
####

# This data was downloaded 12/10/2022 from the site:
# https://quickstats.nass.usda.gov/results/C6E181A0-3848-3E1B-86BF-16FA9032D755
# using the following criteria:
# Program = c("Census", "Survey")
# Sector = "CROPS"
# Group = c("COMMODITIES", "FIELD CROPS", "FRUIT & TREE NUTS", "HORTICULTURE", "VEGETABLES")
# Geographic Level = "COUNTY"
# State = "ARIZONA"
# County = C("COCHISE", "PIMA", "SANTA CRUZ")

usda_dat <- read_csv("Data/USDA - Arizona County level data_2022.12.10.csv")

####
##  Bring in the USDA GATS (Global Agricultural Trade System) data
####

# The import and the export data was downloaded 12/23/2022 from the site:
# https://apps.fas.usda.gov/gats/default.aspx
# See OneNote Project notebook for full query

# Exports
# Note that in the downloaded file, the export data starts in row 5, and there are 275 lines of data
usda_gats_exports <- read_csv("Data/USDA_GATS_Exports.2022.12.23.csv", skip = 4, n_max = 275)

# Imports
# Note that in the downloaded file, the export data starts in row 5, and there are 422 lines of data
usda_gats_imports <- read_csv("Data/USDA_GATS_Imports_2022.12.23.csv", skip = 4, n_max = 422)


##########
## Step 2: Format the data
##########

####
## Format the NASS data
####

# replace any spaces in variable names with a period
colnames(usda_dat) <- gsub(" ", ".", colnames(usda_dat))

# Note that the following additional filters were added after the data was downloaded.
usda_dat_1 <- usda_dat %>%
  # Keep only the census data
  filter(Program == "CENSUS") %>%
  # Keep the cases where Period is the year
  filter(Period == "YEAR") %>%
  # Keep only the numeric values
  filter(!(Value %in% c("(D)", "(Z)"))) %>%
  # Examine Santa Cruz county. 
  filter(County == "SANTA CRUZ")

# Set the value to numeric
usda_dat_1$Value <- as.numeric(gsub(",", "", usda_dat_1$Value))

####
## Format the GATS data
####

# Create a list of the column names
usda_gats_names <- c("drop", "U.S.Custom.Districts","drop2", "Partner", "Outline_Value", 
                     "HS.Code", "Product", "Value.2017", "Value.2018", "Value.2019", 
                     "Value.2020", "Value.2021", "Value.2021_Jan_Oct", "Value.2022_Jan_Oct", 
                     "Period.Change", "Reporter.Code", "Partner.Code", "Product Code", "drop3")

# Rename the columns in the GATS data
names(usda_gats_exports) <- usda_gats_names
names(usda_gats_imports) <-  usda_gats_names
                            
# Remove the columns indicated as drop
usda_gats_imports <- usda_gats_imports %>%
  select(!c(drop, drop2, drop3))

usda_gats_exports <- usda_gats_exports %>%
  select(!c(drop, drop2, drop3))

##########
## Step 3: Create subsets of the NASS data set
##########

####
## Create the acres harvested data set
####

# Create a data set that has the number of acres harvested information
usda_acres <- usda_dat_1 %>%
  # Filter out the domains that are not the totals
  filter(Domain == "TOTAL") %>%
  # Filter out the commodities that contain the phrase "total"
  filter(!grepl("TOTAL", Data.Item)) %>%
  # Keep only the acres harvested for each product.
  filter(grepl("ACRES HARVESTED", Data.Item) & 
           !grepl("FRESH MARKET - ", Data.Item)  & 
           !grepl("PROCESSING - ", Data.Item) &
            !grepl("IRRIGATED - ", Data.Item)) %>%
  # Extract the total number of acres harvested by year and commodity
  select(Year, Data.Item, Value) %>%
  # Remove  "- SALES, MEASURED IN $" from each item name
  mutate(Data.Item = str_remove_all(Data.Item, " - ACRES HARVESTED")) %>%
  # Remove the extra categories for hay
  filter(!Data.Item %in% c("HAY, (EXCL ALFALFA)", "HAYLAGE")) %>%
  # Remove duplicate lines
  unique()

####
## Create the sales measured in $ data set
####

usda_sales <- usda_dat_1 %>%
  # Filter out the domains that are not the totals
  filter(Domain == "TOTAL") %>%
  # Keep only the sales values
  filter(grepl("SALES, MEASURED IN [$]", Data.Item)) %>%
  # Remove the commodities that have been identified as totals in order to avoid duplication
  filter(!grepl("TOTALS", Commodity)) %>%
  # Extract the total sales (in $) by year and commodity
  select(Year, Data.Item, Value) %>%
  # Remove  "- SALES, MEASURED IN $" from each item name
  mutate(Data.Item = str_remove_all(Data.Item, " - SALES, MEASURED IN \\$")) %>%
  # Remove duplicate lines
  unique()


####
## Create the total agland data set
####

usda_agland <- usda_dat_1 %>%
  # Filter out the domains that are not the totals
  filter(Commodity == "AG LAND") %>%
  filter(Domain == "TOTAL") %>%
  # Filter out the Acres values
  filter(grepl(" - ACRES", Data.Item) &
         !grepl("NUMBER OF OPERATIONS", Data.Item) &
         !grepl("AREA, MEASURED IN PCT OF AG LAND", Data.Item) &
         !grepl("OPERATIONS WITH TREATED", Data.Item) &
         !grepl("TREATED, MEASURED IN ACRES", Data.Item) &
         !grepl("INSURANCE", Data.Item) &
         !grepl("IRRIGATED", Data.Item) &
         !grepl("EXCL", Data.Item) &
         !grepl("PASTURED ONLY", Data.Item)) %>%
  select(Year, Data.Item, Value) %>%
  mutate(Data.Item = str_remove_all(Data.Item, " - ACRES")) %>%
  unique()

##########
## Step 4: Plot the data
##########

# Summarize the crop totals
# Create a chart of the acres harvested in the area based off of the 2017 census data
usda_acres %>%
  # Keep only the 2017 data
  filter(Year == 2017) %>%
  # Create the plot
  ggplot(aes(x = reorder(Data.Item, Value), y = Value)) +
  # Format the plot
  ggtitle("Total Number of Acres by Commodity - 2017 Census") +
  xlab("Commodity") +
  ylab("Number of Acres") +
  geom_bar(stat="identity", fill="steelblue") + 
  geom_text(aes(label = Value), position = position_dodge(width=1), size = 4) +
  # Flip the axis so that the products are on the y axis
  coord_flip() +
  theme_bw()
  # Save the plot as a png
  # ggsave("usda_commodity_by_acres_2017_census.png" , width=16, height=14)
  
# Create a chart of the total sales of each product in the area based off of the 2017 census data
usda_sales_1 %>% 
  # Keep only the 2017 data
  filter(Year == 2017) %>%
  filter(Data.Item != "VEGETABLE SEEDS" & 
           Data.Item != "FLORICULTURE, OTHER" & 
           Data.Item != "GOATS" &
           Data.Item != "GOATS, MEAT & OTHER") %>%
  group_by(Year) %>%
  # Create the plot
  ggplot(aes(x = reorder(Data.Item, Value), y = Value)) +
  # Format the plot
  ggtitle("Total Sales by Commodity") +
  xlab("Commodity") +
  ylab("Sales") +
  geom_bar(stat="identity", fill="steelblue") + 
  geom_text(aes(label = Value), position = position_dodge(width=1), size = 4) +
  # Flip the axis so that the products are on the y axis
  coord_flip() +
  theme_bw()
  # Save the plot as a png
  # ggsave("usda_sales.png" , width=10, height=10)

# Summarize the total number of ag land in the 2017 census
usda_agland %>%
  # Keep only the 2017 data
  filter(Year == 2017) %>%
  # Create the plot
  ggplot(aes(x = reorder(Data.Item, Value), y = Value)) +
  # Format the plot
  ggtitle("USDA 2017 Census - Total Acres by Item") +
  xlab("Item") +
  ylab("Number of Acres") +
  geom_bar(stat="identity", fill="steelblue") + 
  geom_text(aes(label = Value), position = position_dodge(width=1)) +
  # Flip the axis so that the products are on the y axis
  coord_flip() +
  theme_bw()
  # Save the plot as a png
  # ggsave("usda_agland.png" , width=10, height=10)
  
####
# Summarize the export totals
####

# Summarize the parent categories for the export data
usda_gats_exports %>%
  # Keep all of the cases where the HS code is na. These are the parent categories.
  filter(is.na(HS.Code)) %>%
  # Keep the top 12 values
  slice_max(order_by = Value.2022_Jan_Oct, n = 12) %>%
  # Create the plot
  ggplot(aes(x = reorder(Product, Value.2022_Jan_Oct), y = Value.2022_Jan_Oct)) +
  # Format the plot
  ggtitle("USDA 2022 Exports (Values in $1,000s)") +
  xlab("Product") +
  ylab("Value") +
  geom_bar(stat="identity", fill="steelblue") + 
  geom_text(aes(label = scales::comma(Value.2022_Jan_Oct)), position = position_dodge(width=1), size = 4) +
  scale_y_continuous(labels = scales::comma) +
  # Flip the axis so that the products are on the y axis
  coord_flip() +
  theme_bw()
  # Save the plot as a png
  # ggsave("usda_gats_exports.png" , width=16, height=12)

# Summarize the Fresh Fruit export totals
usda_gats_exports %>%
  # Keep only the fresh fruit products
  filter(grepl("1.1.1", Outline_Value) & 
           !grepl("1.1.10", Outline_Value) &
           !grepl("1.1.11", Outline_Value) &
           !grepl("1.1.12", Outline_Value) &
           !grepl("1.1.13", Outline_Value) &
           !grepl("1.1.14", Outline_Value) &
           !grepl("1.1.15", Outline_Value) &
           !grepl("1.1.16", Outline_Value) &
           !grepl("1.1.17", Outline_Value) &
           !grepl("1.1.18", Outline_Value) &
           !grepl("1.1.19", Outline_Value)) %>%
  # Keep the top 12 values
  slice_max(order_by = Value.2022_Jan_Oct, n = 12) %>%
  # Create the plot
  ggplot(aes(x = reorder(Product, Value.2022_Jan_Oct), y = Value.2022_Jan_Oct)) +
  # Format the plot
  ggtitle("Top 12 USDA Exports 2022 - Fresh Fruit (Values in $1,000s)") +
  xlab("Product") +
  ylab("Value") +
  geom_bar(stat="identity", fill="steelblue") + 
  geom_text(aes(label = scales::comma(Value.2022_Jan_Oct)), position = position_dodge(width=1)) +
  scale_y_continuous(labels = scales::comma) +
  # Flip the axis so that the products are on the y axis
  coord_flip() +
  theme_bw()
  # Save the plot as a png
  # ggsave("usda_fresh_fruit_exports.png" , width=16, height=12)


# Summarize the Fresh Vegetable export totals
usda_gats_exports %>%
  # Keep only the fresh vegetable products
  filter(Outline_Value %in% c("1.2.1.1", "1.2.1.2", "1.2.1.3", "1.2.1.4", "1.2.1.5", "1.2.1.6", "1.2.1.7",
                              "1.2.1.8", "1.2.1.9", "1.2.1.10", "1.2.1.11", "1.2.1.12", "1.2.1.13", "1.2.1.14",
                              "1.2.1.15", "1.2.1.16", "1.2.1.17", "1.2.1.18", "1.2.1.19", "1.2.1.20", "1.2.1.21",
                              "1.2.1.22", "1.2.1.23", "1.2.1.24", "1.2.1.25", "1.2.1.26", "1.2.1.27", "1.2.1.28",
                              "1.2.1.29", "1.2.1.30", "1.2.1.31", "1.2.1.32", "1.2.1.33", "1.2.1.34", "1.2.1.35",
                              "1.2.1.36", "1.2.1.37", "1.2.1.38", "1.2.1.39", "1.2.1.40", "1.2.1.41", "1.2.1.42",
                              "1.2.1.43", "1.2.1.44", "1.2.1.45", "1.2.1.46", "1.2.1.47", "1.2.1.48", "1.2.1.49",
                              "1.2.1.50", "1.2.1.51", "1.2.1.52","1.2.1.53")) %>%
  # Keep the top 12 values
  slice_max(order_by = Value.2022_Jan_Oct, n = 12) %>%
  # Create the plot
  ggplot(aes(x = reorder(Product, Value.2022_Jan_Oct), y = Value.2022_Jan_Oct)) +
  # Format the plot
  ggtitle("Top 12 USDA Exports 2022 - Fresh Vegetables (Values in $1,000s)") +
  xlab("Product") +
  ylab("Value") +
  geom_bar(stat="identity", fill="steelblue") + 
  geom_text(aes(label = scales::comma(Value.2022_Jan_Oct)), position = position_dodge(width=1)) +
  scale_y_continuous(labels = scales::comma) +
  # Flip the axis so that the products are on the y axis
  coord_flip() +
  theme_bw()
  # Save the plot as a png
  # ggsave("usda_fresh_veg_exports.png" , width=10, height=10)

####
# Summarize the import totals
####

# Summarize the parent categories for the export data
usda_gats_imports %>%
  # Keep all of the cases where the HS code is na. These are the parent categories.
  filter(is.na(HS.Code)) %>%
  # Keep the top 12 values
  slice_max(order_by = Value.2022_Jan_Oct, n = 12) %>%
  # Create the plot
  ggplot(aes(x = reorder(Product, Value.2022_Jan_Oct), y = Value.2022_Jan_Oct)) +
  # Format the plot
  ggtitle("USDA 2022 Imports (Values in $1,000s)") +
  xlab("Product") +
  ylab("Value") +
  geom_bar(stat="identity", fill="steelblue") + 
  geom_text(aes(label = scales::comma(Value.2022_Jan_Oct)), position = position_dodge(width=1)) +
  scale_y_continuous(labels = scales::comma) +
  # Flip the axis so that the products are on the y axis
  coord_flip() +
  theme_bw()
  # Save the plot as a png
  # ggsave("usda_gats_imports.png" , width=16, height=12)

# Summarize the Fresh Vegetable export totals
usda_gats_imports %>%
  # Keep only the fresh vegetable products
  filter(grepl("1.1.1.", Outline_Value) & 
           !grepl("1.1.10", Outline_Value) &
           !grepl("1.1.11", Outline_Value) &
           !grepl("1.1.12", Outline_Value) &
           !grepl("1.1.13", Outline_Value) &
           !grepl("1.1.14", Outline_Value) &
           !grepl("1.1.15", Outline_Value) &
           !grepl("1.1.16", Outline_Value) &
           !grepl("1.1.17", Outline_Value) &
           !grepl("1.1.18", Outline_Value) &
           !grepl("1.1.19", Outline_Value)) %>%

  # Keep the top 12 values
  slice_max(order_by = Value.2022_Jan_Oct, n = 12) %>%
  # Create the plot
  ggplot(aes(x = reorder(Product, Value.2022_Jan_Oct), y = Value.2022_Jan_Oct)) +
  # Format the plot
  ggtitle("Top 12 USDA Imorts 2022 - Fresh Vegetables (Values in $1,000s)") +
  xlab("Product") +
  ylab("Value") +
  geom_bar(stat="identity", fill="steelblue") + 
  geom_text(aes(label = scales::comma(Value.2022_Jan_Oct)), position = position_dodge(width=1)) +
  scale_y_continuous(labels = scales::comma) +
  # Flip the axis so that the products are on the y axis
  coord_flip() +
  theme_bw()
  # Save the plot as a png
  # ggsave("usda_fresh_veg_imports.png" , width=16, height=14)

# Summarize the Fresh Fruit import totals
usda_gats_imports %>%
  # Keep only the fresh fruit products
  filter(Outline_Value %in% c("1.2.1.1", "1.2.1.2", "1.2.1.3", "1.2.1.4", "1.2.2.1", "1.2.2.2", "1.2.2.3",
                              "1.2.2.4", "1.2.2.5", "1.2.2.6", "1.2.2.7", "1.2.2.8", "1.2.3.1", "1.2.3.2",
                              "1.2.3.3", "1.2.3.4", "1.2.3.5", "1.2.3.6", "1.2.3.7", "1.2.3.8", "1.2.3.9",
                              "1.2.3.10", "1.2.3.11", "1.2.3.12", "1.2.3.13", "1.2.3.14", "1.2.3.15",
                              "1.2.3.16", "1.2.3.17", "1.2.3.18", "1.2.3.19", "1.2.3.20", "1.2.3.21",
                              "1.2.3.22", "1.2.3.23", "1.2.3.24", "1.2.3.25", "1.2.3.26", "1.2.3.27",
                              "1.2.3.28", "1.2.3.29", "1.2.3.30", "1.2.3.31", "1.2.3.32", "1.2.3.33",
                              "1.2.3.34", "1.2.4.1", "1.2.4.2", "1.2.4.3", "1.2.4.4", "1.2.4.5",
                              "1.2.4.6", "1.2.4.7", "1.2.4.8", "1.2.4.9", "1.2.4.10", "1.2.4.11")) %>%
  # Keep the top 12 values
  slice_max(order_by = Value.2022_Jan_Oct, n = 12) %>%
  # Create the plot
  ggplot(aes(x = reorder(Product, Value.2022_Jan_Oct), y = Value.2022_Jan_Oct)) +
  # Format the plot
  ggtitle("Top 12 USDA Imports 2022 - Fresh Fruit (Values in $1,000s)") +
  xlab("Product") +
  ylab("Value") +
  geom_bar(stat="identity", fill="steelblue") + 
  geom_text(aes(label = scales::comma(Value.2022_Jan_Oct)), position = position_dodge(width=1)) +
  scale_y_continuous(labels = scales::comma) +
  # Flip the axis so that the products are on the y axis
  coord_flip() +
  theme_bw()
  # Save the plot as a png
  # ggsave("usda_fresh_fruit_imports.png" , width=16, height=14)