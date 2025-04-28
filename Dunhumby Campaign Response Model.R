# Load libraries
library(tidyverse)

# Set data path
data_path <- "/Users/shimuliu/Desktop/First-year Paper/archive"

# Load datasets
campaign_desc       <- read_csv(file.path(data_path, "campaign_desc.csv"))
campaign_table      <- read_csv(file.path(data_path, "campaign_table.csv"))
causal_data         <- read_csv(file.path(data_path, "causal_data.csv"))
coupon_redempt      <- read_csv(file.path(data_path, "coupon_redempt.csv"))
coupon              <- read_csv(file.path(data_path, "coupon.csv"))
hh_demographic      <- read_csv(file.path(data_path, "hh_demographic.csv"))
product             <- read_csv(file.path(data_path, "product.csv"))
transaction_data    <- read_csv(file.path(data_path, "transaction_data.csv"))

#Start from all transaction_data
transaction_base <- transaction_data %>%
  mutate(coupon_used = if_else(COUPON_DISC < 0 | COUPON_MATCH_DISC < 0, 1, 0))
# Y = 1 if coupon used, 0 otherwise
head(transaction_base)
summary(transaction_base)

#Merge Demographics
transaction_base <- transaction_base %>%
  left_join(hh_demographic, by = "household_key")
# Merge household demographics (AGE_DESC, INCOME_DESC, etc.)

# Match with Display/Mailer Exposure (causal_data)
transaction_base <- transaction_base %>%
  mutate(WEEK_NO = floor(DAY / 7) + 1) %>%
  left_join(causal_data, by = c("STORE_ID", "PRODUCT_ID", "WEEK_NO")) %>%
  mutate(Redeem_Display = if_else(!is.na(display) & display != "0", 1, 0),
         Redeem_Mailer  = if_else(!is.na(mailer)  & mailer != "0", 1, 0)) %>%
  mutate(Redeem_Display = replace_na(Redeem_Display, 0),
         Redeem_Mailer  = replace_na(Redeem_Mailer, 0))
# If missing display/mailer info, treat as 0 (no exposure)


#Attach Campaign Information
# Coupon lookup: COUPON_UPC to PRODUCT_ID + CAMPAIGN
coupon_lookup <- coupon %>%
  select(COUPON_UPC, CAMPAIGN, PRODUCT_ID)

# Coupon redemption linked to product and campaign
redeemed_coupons <- coupon_redempt %>%
  mutate(WEEK_NO = floor(DAY / 7) + 1) %>%
  left_join(coupon_lookup, by = c("COUPON_UPC", "CAMPAIGN")) %>%
  select(household_key, CAMPAIGN, WEEK_NO, PRODUCT_ID)

# Merge campaigns into transactions
transaction_base <- transaction_base %>%
  left_join(redeemed_coupons, 
            by = c("household_key", "PRODUCT_ID", "WEEK_NO")) %>%
  left_join(campaign_desc %>% select(CAMPAIGN, DESCRIPTION, START_DAY, END_DAY),
            by = "CAMPAIGN") %>%
  mutate(DESCRIPTION = replace_na(DESCRIPTION, "No Campaign"),
         CampaignDuration = if_else(!is.na(END_DAY - START_DAY), END_DAY - START_DAY, NA_real_))
# If no campaign info matched, label as "No Campaign"

#check
glimpse(transaction_base)
summary(transaction_base)
coupon_used_data <- transaction_base %>%
  filter(coupon_used == 1)

#Handeling NAs
# Add NA indicators and clean demographic variables
transaction_base <- transaction_base %>%
  mutate(
    # Fill missing values with "Unknown"
    INCOME_DESC = replace_na(INCOME_DESC, "Unknown"),
    MARITAL_STATUS_CODE = replace_na(MARITAL_STATUS_CODE, "U"),
    HOUSEHOLD_SIZE_DESC = replace_na(HOUSEHOLD_SIZE_DESC, "Unknown"),
  
    # Turn into factors
    INCOME_DESC = as.factor(INCOME_DESC),
    MARITAL_STATUS_CODE = as.factor(MARITAL_STATUS_CODE),
    HOUSEHOLD_SIZE_DESC = as.factor(HOUSEHOLD_SIZE_DESC),
    
    #Set Baseline
    DESCRIPTION = relevel(as.factor(DESCRIPTION), ref = "TypeB"),
    INCOME_DESC = relevel(as.factor(INCOME_DESC), ref = "Under 15K"),
    MARITAL_STATUS_CODE = relevel(as.factor(MARITAL_STATUS_CODE), ref = "B"),
    HOUSEHOLD_SIZE_DESC = relevel(as.factor(HOUSEHOLD_SIZE_DESC), ref = "1")
  )
  

#Fit model
model_logit <- glm(coupon_used ~ 
                     DESCRIPTION + CampaignDuration +
                     INCOME_DESC + 
                     MARITAL_STATUS_CODE + 
                     HOUSEHOLD_SIZE_DESC + 
                     Redeem_Display + Redeem_Mailer,
                   data = transaction_base,
                   family = binomial(link = "logit"))

summary(model_logit)

#practice committing changes
