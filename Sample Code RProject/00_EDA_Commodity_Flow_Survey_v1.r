
## File Created by R. Hall 6/15/2022
##
## Purpose: This file brings in the Commodity Flow Public Use Data (CFS PUF).
##          It also brings in the location information in for the from the file 
##          cfs-2017-puf-users-guide-app-a-aug2020


##########
## Step 0: Load the required packages
##########

library("openxlsx")
library("tidyverse")
library("dplyr")
library("ggplot2")

##########
## Step 1: Bring in the cfs data
##########

# Bring in the CFS PUF data
cfs_puf_dat <- read.csv("Data/CFS 2017 PUF CSV.csv")

##########
## Step 2: Bring in the area data
##########

# Bring in the CFS Area data
cfs_area_dat <- read.xlsx("Data/cfs-2017-puf-users-guide-app-a-aug2020.xlsx", sheet = "App A1", startRow =  3)

# Bring in the rest of the data from the users guide
# Bring in the CFS NAICS data
cfs_naics_dat <- read.xlsx("Data/cfs-2017-puf-users-guide-app-a-aug2020.xlsx", sheet = "App A2", startRow = 2)

# Bring in the CFS SCTG data
cfs_sctg_dat <- read.xlsx("Data/cfs-2017-puf-users-guide-app-a-aug2020.xlsx", sheet = "App A3", startRow = 2)

# Bring in the CFS Mode data
cfs_mode_dat <- read.xlsx("Data/cfs-2017-puf-users-guide-app-a-aug2020.xlsx", sheet = "App A4", startRow = 2)

##########
## Step 3: Format the data
##########

# Remove the first two columns from the cfs data. 
# The MA column contains blank values. These variables will be recreated based off the CFS_Area information
cfs_area_dat_1 <- cfs_area_dat[ , 3:ncol(cfs_area_dat)]

# Rename the columns
names(cfs_area_dat_1) <- c("CFS_AREA", "MA_Type", "Description")

# Remove extra columns from the mode data
cfs_mode_dat <- cfs_mode_dat %>%
  select(Mode.Code, Mode.of.transportation.Description)

##########
## Step 4: Add in the origin and destination information 
##########

# Remove the obs where the orig_state is not Arizona. According to the data dictionary file:
# CFS Data Dictionary for Downloadable Tables.xlsx, the state code for AZ = 4
cfs_puf_dat_1 <- cfs_puf_dat %>%
  filter(ORIG_STATE == 4 | cfs_puf_dat$DEST_STATE == 4)

# Add in the origin information
cfs_puf_dat_1 <- merge(cfs_puf_dat_1, cfs_area_dat_1, by.x = c("ORIG_CFS_AREA"), by.y = c("CFS_AREA"))
names(cfs_puf_dat_1)[names(cfs_puf_dat_1) == "Description"] <- "Origin_Description"

# Add in the destination information
cfs_puf_dat_2 <- merge(cfs_puf_dat_1, cfs_area_dat_1[ , names(cfs_area_dat_1) != "MA_Type"], by.x = c("DEST_CFS_AREA"), by.y = c("CFS_AREA"))
names(cfs_puf_dat_2)[names(cfs_puf_dat_2) == "Description"] <- "Destination_Description"


##########
## Step 5: Add in the naics info
##########

cfs_puf_dat_3 <- merge(cfs_puf_dat_2, cfs_naics_dat, by = "NAICS")
names(cfs_puf_dat_3)[names(cfs_puf_dat_3) == "Description"] <- "NAICS_Description"

##########
## Step 6: Add in the transportation mode info
##########

## Format the mode data
cfs_mode_dat_1 <- cfs_mode_dat %>%
  # Remove the text values from the Mode Code
  filter(!Mode.Code %in% c("Mode Collapsing Pattern", "Most Detailed Mode Codes")) %>%
  # Set the Mode Code to a numeric value
  mutate(Mode.Code = as.numeric(Mode.Code)) %>%
  # Remove duplicate lines
  unique() %>%
  # Remove the air and other mode values 
  filter(Mode.of.transportation.Description != "Other mode" & 
           Mode.of.transportation.Description != "Air (incl truck & air)" )
  
names(cfs_mode_dat_1)[names(cfs_mode_dat_1) == "Mode.Code"] <- "MODE"

cfs_puf_dat_4 <- merge(cfs_puf_dat_3, cfs_mode_dat_1, by = "MODE")

# Exam the resulting modes
table(cfs_puf_dat_4$Mode.of.transportation.Description)

# Examine the cases that are flagged as an export
table(cfs_puf_dat_4$Mode.of.transportation.Description[cfs_puf_dat_4$EXPORT_YN == "Y"])

# Examine the countries that the exports are going to
table(cfs_puf_dat_4$EXPORT_CNTRY[cfs_puf_dat_4$EXPORT_YN == "Y"])

# A    C    E    M    S 
# 1016  492 1094 1244  243 

# Split out the cases where the Origin and the Destination are both the Tucson-Nogales, AZ  CFS Area 
local_shipments <- cfs_puf_dat_4 %>%
  filter(Destination_Description == "Tucson-Nogales, AZ  CFS Area " & 
           Origin_Description == "Tucson-Nogales, AZ  CFS Area ") 


##########
## Step 7: Create subsets of the data for the imports and the exports and summarize the tables
##########

# Create a data set for the cases where the origin is Tucson-Nogales, AZ  CFS Area
tn_cfs_origin <- cfs_puf_dat_4 %>% 
  filter(Origin_Description == "Tucson-Nogales, AZ  CFS Area " & 
           Destination_Description != "Tucson-Nogales, AZ  CFS Area ")
  

# Count the number of shipments that originate in the Tucson-Nogales, AZ  CFS Area
tn_cfs_origin %>%
  select(SHIPMT_ID) %>%
  unique() %>%
  summarise(n())

# Find the total value of shipments that originate in the Tucson-Nogales, AZ  CFS Area
tn_cfs_origin %>%
  group_by(Origin_Description) %>%
  mutate(SHIPMT_VALUE = sum(SHIPMT_VALUE*WGT_FACTOR)) %>%
  select(SHIPMT_VALUE) %>%
  unique()
  

# Examine the number of shipments leaving Tucson-Nogales, AZ CFS Area by destination
destinations <- tn_cfs_origin %>%
  group_by(Destination_Description) %>%
  summarise(Destination_Count = n()) %>%
  unique()

# Examine the NAICS for the shipments moved around in the area
local_naics <- local_shipments %>%
  group_by(NAICS_Description) %>%
  summarise(NAICS_Count = n()) %>%
  unique()


# Create a data set for the cases where the destination is Tucson-Nogales, AZ  CFS Area
tn_cfs_destination <- cfs_puf_dat_4 %>%
     filter(Destination_Description == "Tucson-Nogales, AZ  CFS Area " & 
              Origin_Description != "Tucson-Nogales, AZ  CFS Area ")
  


# Count the number of shipments that have a destination of Tucson-Nogales, AZ  CFS Area
tn_cfs_destination %>%
  select(SHIPMT_ID) %>%
  unique() %>%
  summarise(n())

# Find the total value of shipments where the destination was the Tucson-Nogales, AZ  CFS Area
tn_cfs_destination %>%
  group_by(Destination_Description) %>%
  mutate(SHIPMT_VALUE = sum(SHIPMT_VALUE)) %>%
  select(SHIPMT_VALUE) %>%
  unique()

# Examine the number of shipments entering Tucson-Nogales, AZ CFS Area by origin
origins <- tn_cfs_destination %>%
  group_by(Origin_Description) %>%
  summarise(Origin_Count = n()) %>%
  unique()

# Create a summary data set of the exports to each destination
summary_totals_by_dest <- tn_cfs_origin %>%
  group_by(NAICS, DEST_CFS_AREA, NAICS_Description) %>%
  mutate(total.weight = sum(SHIPMT_WGHT)) %>%
  mutate(total.value = sum(SHIPMT_VALUE)) %>%
  select(NAICS, DEST_CFS_AREA, NAICS_Description, Destination_Description, total.weight, total.value) %>%
  unique()

# Create a summary data set of the imports from each destination
summary_totals_by_orig <- tn_cfs_destination %>%
  group_by(NAICS, ORIG_CFS_AREA, NAICS_Description) %>%
  mutate(total.weight = sum(SHIPMT_WGHT)) %>%
  mutate(total.value = sum(SHIPMT_VALUE)) %>%
  select(NAICS, ORIG_CFS_AREA, NAICS_Description, Origin_Description, total.weight, total.value) %>%
  unique()


# Create a summary data set of the exports
summary_totals_orig_tn <- tn_cfs_origin %>%
  group_by(NAICS, NAICS_Description) %>%
  mutate(total.weight = sum(SHIPMT_WGHT)) %>%
  mutate(total.value = sum(SHIPMT_VALUE)) %>%
  select(NAICS, NAICS_Description, total.weight, total.value) %>%
  unique()

# Create a summary data set of the imports from each destination
summary_totals_dest_tn <- tn_cfs_destination %>%
  group_by(NAICS, NAICS_Description) %>%
  mutate(total.weight = sum(SHIPMT_WGHT)) %>%
  mutate(total.value = sum(SHIPMT_VALUE)) %>%
  select(NAICS, NAICS_Description, total.weight, total.value) %>%
  unique()

# Examine the imported NAICS by mode
tn_cfs_destination %>%
  mutate(Mode = ifelse(Mode.of.transportation.Description %in% 
                         c("Rail", "Truck and rail", "Company-owned truck", "For-hire truck") , 
                       Mode.of.transportation.Description, "Other")) %>%
  # Summarize the data
  group_by(NAICS_Description, Mode) %>%
  mutate(total.weight = sum(SHIPMT_WGHT)) %>%
  select(NAICS_Description, Mode, total.weight) %>%
  unique()

##########
## Step 3: Plot the summary tables
##########

# NAICS moved from Tucson_Nogales by weight
summary_totals_orig_tn %>%
  #top_n(total.weight, 10) %>%
  ggplot(aes(x = reorder(NAICS_Description, total.weight), y = total.weight)) +
  ggtitle("NAICS moved from Tucson - Nogales, AZ CFS Area by weight") +
  xlab("NAICS Description") +
  ylab("Total Weight") +
  scale_y_continuous(labels = scales::comma) +
  geom_bar(stat="identity", fill="steelblue") + 
  geom_text(aes(label = total.weight), position = position_dodge(width=1), size = 2.5) +
  coord_flip() +
  theme_bw()
  # Saving as png
  ggsave("cfs_summary_totals_by_dest_weight.png" , width=14, height=10)

# NAICS moved from Tucson_Nogales by Value
summary_totals_orig_tn %>%
  ggplot(aes(x = reorder(NAICS_Description, total.value), y = total.value )) +
  ggtitle("NAICS Moved From Tucson - Nogales, AZ CFS Area by Value") +
  xlab("NAICS Description") +
  ylab("Total Value") +
  scale_y_continuous(labels = scales::comma) +
  geom_bar(stat="identity", fill="steelblue") + 
  geom_text(aes(label = total.value), position = position_dodge(width=1), size = 2.5) +
  coord_flip() +
  theme_bw()
  # Saving as png
  ggsave("cfs_summary_totals_by_dest_value.png" , width=14, height=10)

# NAICS moved to Tucson_Nogales by weight
summary_totals_dest_tn %>%
  ggplot(aes(x = reorder(NAICS_Description, total.weight), y = total.weight)) +
  ggtitle("NAICS Moved To Tucson - Nogales, AZ CFS Area by weight") +
  xlab("NAICS Description") +
  ylab("Total Weight") +
  scale_y_continuous(labels = scales::comma) +
  geom_bar(stat="identity", fill="steelblue") + 
  geom_text(aes(label = total.weight), position = position_dodge(width=1), size = 2.5) +
  coord_flip() +
  theme_bw()
  # Saving as png
  ggsave("cfs_summary_totals_by_orig_weight.png" , width=14, height=10)

# NAICS moved to Tucson_Nogales by Value
summary_totals_dest_tn %>%
  ggplot(aes(x = reorder(NAICS_Description, total.value), y = total.value)) +
  ggtitle("NAICS Moved To Tucson - Nogales, AZ CFS Area By Value") +
  xlab("NAICS Description") +
  ylab("Total Value") +
  scale_y_continuous(labels = scales::comma) +
  geom_bar(stat="identity", fill="steelblue") + 
  geom_text(aes(label = total.value), position = position_dodge(width=1), size = 2.5) +
  coord_flip() +
  theme_bw()
  # Saving as png
  ggsave("cfs_summary_totals_by_orig_value.png" , width=14, height=10)


# Summarize the mode of transportation by NAICS leaving the Tucson - Nogales, AZ CFS Area
summary_naics_n_by_mode_from_tn <- t_n_cfs_origin %>% 
  group_by(NAICS, NAICS_Description, Mode.of.transportation.Description)  %>% 
  summarise(Mode_Count = n()) 

# Summarize the mode of transportation leaving the Tucson - Nogales, AZ CFS Area 
t_n_cfs_origin %>%
  # Summarize the data
  group_by(Mode.of.transportation.Description)  %>% 
  summarise(Mode_Count = n())

# Create a piechart to visualize the table from above
t_n_cfs_origin %>%
  # Create a mode category
  mutate(Mode = ifelse(Mode.of.transportation.Description %in% 
                         c("Rail", "Truck and rail", "Company-owned truck", "For-hire truck") , Mode.of.transportation.Description, "Other")) %>%
  # Summarize the data
  group_by(Mode)  %>% 
  summarise(Mode_Count = n()) %>%
  # Create the percent values
  mutate(Total = sum(Mode_Count), percent_total = Mode_Count/Total*100) %>%
  # Plot the data
  # Create a basic bar
  ggplot(aes(x = "", y = percent_total, fill = Mode)) +
  geom_bar(stat="identity", width=1) +
  # Convert to pie (polar coordinates) and add labels
  coord_polar("y", start=0) + geom_text(aes(label = paste0(round(percent_total), "%")), position = position_stack(vjust = 0.5), size = 3) +
  scale_fill_brewer(palette = "GnBu") +
  # Remove labels and add title
  labs(x = NULL, y = NULL, fill = NULL, title = "Number of Shipments Leaving the Tucson - Nogales, AZ CFS Area by Mode of Transportation") +
  theme_classic() + theme(axis.line = element_blank(),
                          axis.text = element_blank(),
                          axis.ticks = element_blank(),
                          plot.title = element_text(hjust = 0.5, color = "#666666"))
# Saving as pdf
ggsave("cfs_summary_shipments_by_orig_mode.pdf")
ggsave("cfs_summary_shipments_by_orig_mode.png" , width=10, height=10)

# Summarize the mode of transportation entering the Tucson - Nogales, AZ CFS Area 
t_n_cfs_destination %>%
  # Summarize the data
  group_by(Mode.of.transportation.Description)  %>% 
  summarise(Mode_Count = n())

# Create a piechart to visualize the table from above
t_n_cfs_destination %>%
  # Create a mode category
  mutate(Mode = ifelse(Mode.of.transportation.Description %in% 
                         c("Rail", "Truck and rail", "Company-owned truck", "For-hire truck") , Mode.of.transportation.Description, "Other")) %>%
  # Summarize the data
  group_by(Mode)  %>% 
  summarise(Mode_Count = n()) %>%
  # Create the percent values
  mutate(Total = sum(Mode_Count), percent_total = Mode_Count/Total*100) %>%
  # Plot the data
  # Create a basic bar
  ggplot(aes(x = "", y = percent_total, fill = Mode)) +
  geom_bar(stat="identity", width=1) +
  # Convert to pie (polar coordinates) and add labels
  coord_polar("y", start=0) + geom_text(aes(label = paste0(round(percent_total), "%")), position = position_stack(vjust = 0.75), size = 2) +
  scale_fill_brewer(palette = "GnBu") +
  # Remove labels and add title
  labs(x = NULL, y = NULL, fill = NULL, title = "Number of Shipments Entering the Tucson - Nogales, AZ CFS Area by Mode of Transportation") +
  theme_classic() + theme(axis.line = element_blank(),
                          axis.text = element_blank(),
                          axis.ticks = element_blank(),
                          plot.title = element_text(hjust = 0.5, color = "#666666"))
# Saving as pdf
ggsave("cfs_summary_shipments_by_destination_mode.pdf")
ggsave("cfs_summary_shipments_by_destination_mode.png" , width=14, height=10)







### Note that these have not been plotted. Not sure if they are quite necessary.
# Summarize the Number of NAICS by Origin
summary_totals_by_orig %>%
  # Create a unique list of the NAICS and origin combinations
  select(Origin_Description, NAICS_Description) %>%
  unique() %>%
  # Count the number of NAICS for each origin
  group_by(Origin_Description) %>%
  summarise(NAICS_Count = n()) %>%
  unique() 
  
# Summarize the NAICS by Destination
summary_totals_by_dest %>%
  select(NAICS_Description, Destination_Description) %>%
  unique() %>%
  # Count the number of NAICS for each origin
  group_by(Destination_Description) %>%
  summarise(NAICS_Count = n())
  


  