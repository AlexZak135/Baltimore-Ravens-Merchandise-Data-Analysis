# Packages used
library(data.table)
library(dplyr)
library(tidyr)
library(lubridate)
library(purrr)
library(stringr)

# Import the data
ravens <- fread("RavensData.csv")

# Inspect the data
glimpse(ravens)

# Find duplicate totals
sum(duplicated(ravens))

# Get data frame with all duplicates
ravens_duplicates <- ravens %>%
  filter(duplicated(ravens) | duplicated(ravens, fromLast = TRUE)) %>%
  arrange(ORDER_NUMBER)

# Get distinct totals
count(distinct(ravens_duplicates))

# Filter for distinct records
ravens <- ravens %>%
  distinct()

# Confirm no duplicates
sum(duplicated(ravens))

# Filter for only character data types
characters <- ravens %>% 
  select_if(is.character)

# View column names
colnames(characters)

# Check character data types for blank strings
no_blanks <- ravens %>%
  filter(ORDER_DATE == "" | PROD_CATEGORY == "" | ITEM_SIZE == "")

blank_prod_subcat <- ravens %>%
  filter(PROD_SUBCATEGORY == "")

blank_prod_subcl <- ravens %>%
  filter(PROD_SUB_CLASS == "")

# Make blank strings NA
ravens <- ravens %>% 
  mutate_all(na_if, "")

# Get missing value counts for each column
na_count <- ravens %>%
  summarise(across(.fns = ~sum(is.na(.)))) %>%
  pivot_longer(cols = ORDER_NUMBER : ORDER_SHIP_TOTAL,
               names_to = "variables",
               values_to = "na_count") %>%
  filter(na_count > 0) %>%
  arrange(desc(na_count))

# Produce data frames with missing values
na_prod_subcat <- ravens %>%
  filter(is.na(PROD_SUBCATEGORY))

na_prod_subcl <- ravens %>%
  filter(is.na(PROD_SUB_CLASS))

na_item_disc <- ravens %>%
  filter(is.na(ITEM_DISC_TOTAL))

# Impute missing values
ravens <- ravens %>% 
  replace_na(list(PROD_SUBCATEGORY = "NPSC", PROD_SUB_CLASS  = "NPSCL"))

ravens <- ravens %>%
  mutate(ITEM_DISC_TOTAL = coalesce(ITEM_DISC_TOTAL, ORDER_DISCOUNT_TOTAL))

# Confirm no missing values exist
sum(is.na(ravens))

# Change data type
ravens$ORDER_DATE <- mdy(ravens$ORDER_DATE)

# Take the following data cleaning steps:
# Sort rows
# Reorder columns
# Drop rows
# Rename columns
# Create new columns
ravens <- ravens %>%
  arrange(ORDER_DATE, ORDER_NUMBER) %>%
  relocate(ORDER_DATE, .before = ORDER_NUMBER) %>%
  select(-ORDER_DISCOUNT_TOTAL, -ITEM_DISC_TOTAL, -ORDER_GROSS_SUBTOTAL,
         -ORDER_TAX_TOTAL, -ORDER_NET_TOTAL, -ORDER_SHIP_TOTAL) %>%
  rename(PROD_CAT = PROD_CATEGORY,
         PROD_SUB_CAT = PROD_SUBCATEGORY,
         PROD_SUB_CL = PROD_SUB_CLASS,
         SIZE = ITEM_SIZE) %>%
  mutate(REVENUE  = QTY_SOLD * UNIT_PRICE,
         MONTH = month(ORDER_DATE),
         SEASON = case_when(
           MONTH %in% 9:11 ~ "Fall",
           MONTH %in% c(12, 1 , 2) ~ "Winter",
           MONTH %in% 3:5 ~ "Spring",
           TRUE ~ "Summer"),
         PROD_SUB_CAT =  case_when(
           PROD_SUB_CAT == "MEN" ~ "Men",
           PROD_SUB_CAT == "LAD" ~ "Ladies",
           TRUE ~ "OTHER")) %>%
  relocate(SEASON, .after = ORDER_DATE) %>%
  select(-PROD_SUB_CL,-MONTH)

# Find top five jerseys for revenue
playerid_top4_rev <- ravens %>%
  filter(PLAYER_ID != 0) %>%
  group_by(PLAYER_ID) %>%
  summarize(REVENUE = sum(REVENUE)) %>%
  arrange(desc(REVENUE)) %>%
  top_n(4)

# Modify values
ravens <- ravens %>%
  mutate(PLAYER_ID = case_when(
    PLAYER_ID == 115 ~ "Lamar Jackson",
    PLAYER_ID == 130 ~ "Justin Tucker",
    PLAYER_ID == 56 ~ "Ray Lewis",
    PLAYER_ID == 4 ~ "Ed Reed",
    PLAYER_ID == 0 ~ "No Player",
    TRUE ~ "Other Player"))

# Get data types
map(ravens, class)

# Change data types
ravens <- ravens %>%
  mutate(SEASON = factor(SEASON, levels = c("Spring", "Summer",
                                            "Fall", "Winter")),
         ORDER_NUMBER = as.character(ORDER_NUMBER),
         CLIENT_ID = as.character(CLIENT_ID),
         ZIP_CODE = as.character(ZIP_CODE))

# See levels
levels(ravens$SEASON)

# Get descriptive statistics
summary(ravens)

# Produce data frame with monthly revenue and units sold
dviz1_data <- ravens %>%
  mutate(M_Y_DATE = ORDER_DATE) %>%
  mutate(M_Y_DATE = as.character(M_Y_DATE)) %>%
  mutate(M_Y_DATE = str_sub(M_Y_DATE, 1, 7)) %>%
  group_by(M_Y_DATE, SEASON) %>%
  summarize(Monthly_Revenue = round(sum(REVENUE), 0),
            Monthly_Units_SOLD = sum(QTY_SOLD)) %>%
  mutate(Monthly_Revenue = as.integer(Monthly_Revenue),
         M_Y_DATE = factor(M_Y_DATE))

# Produce data frame with top zip codes for revenue
dviz2_data <- ravens %>%
  group_by(ZIP_CODE) %>%
  summarize(Total_Revenue = round(sum(REVENUE), 0)) %>%
  arrange(desc(Total_Revenue))

PROD_CAT_TOP5 <- ravens %>%
  group_by(PROD_CAT) %>%
  summarize(Total_Revenue = sum(REVENUE)) %>% 
  arrange(desc(Total_Revenue)) %>%
  top_n(5)

# Get data frame with revenue for top five product categories 
dviz3_data <- ravens %>%
  filter(PROD_CAT == "JER" | 
         PROD_CAT == "TEE"| 
         PROD_CAT == "SWT"| 
         PROD_CAT == "HAT"| 
         PROD_CAT == "JAC") %>%
  group_by(PROD_CAT, PROD_SUB_CAT) %>%
  summarize(Total_Revenue = sum(REVENUE)) 

# Get data frame with revenue breakdown for jerseys
dviz4_data <- ravens %>%
  filter(PROD_CAT == "JER", PLAYER_ID != "No Player") %>%
  group_by(PLAYER_ID) %>%
  summarize(Total_Revenue = sum(REVENUE)) %>%
  arrange(desc(Total_Revenue))
