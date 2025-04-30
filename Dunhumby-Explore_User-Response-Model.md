Dunhumby Explore_User Responde Model
================
2025-04-28

1.  Load the dataset

``` r
# Load libraries
library(tidyverse)
```

    ## ── Attaching core tidyverse packages ──────────────────────── tidyverse 2.0.0 ──
    ## ✔ dplyr     1.1.4     ✔ readr     2.1.5
    ## ✔ forcats   1.0.0     ✔ stringr   1.5.1
    ## ✔ ggplot2   3.5.1     ✔ tibble    3.2.1
    ## ✔ lubridate 1.9.4     ✔ tidyr     1.3.1
    ## ✔ purrr     1.0.4     
    ## ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
    ## ✖ dplyr::filter() masks stats::filter()
    ## ✖ dplyr::lag()    masks stats::lag()
    ## ℹ Use the conflicted package (<http://conflicted.r-lib.org/>) to force all conflicts to become errors

``` r
# Set data path
data_path <- "/Users/shimuliu/Desktop/First-year Paper/archive"

# Load datasets
campaign_desc       <- read_csv(file.path(data_path, "campaign_desc.csv"))
```

    ## Rows: 30 Columns: 4
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr (1): DESCRIPTION
    ## dbl (3): CAMPAIGN, START_DAY, END_DAY
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` r
campaign_table      <- read_csv(file.path(data_path, "campaign_table.csv"))
```

    ## Rows: 7208 Columns: 3
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr (1): DESCRIPTION
    ## dbl (2): household_key, CAMPAIGN
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` r
causal_data         <- read_csv(file.path(data_path, "causal_data.csv"))
```

    ## Rows: 36786524 Columns: 5
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr (2): display, mailer
    ## dbl (3): PRODUCT_ID, STORE_ID, WEEK_NO
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` r
coupon_redempt      <- read_csv(file.path(data_path, "coupon_redempt.csv"))
```

    ## Rows: 2318 Columns: 4
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## dbl (4): household_key, DAY, COUPON_UPC, CAMPAIGN
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` r
coupon              <- read_csv(file.path(data_path, "coupon.csv"))
```

    ## Rows: 124548 Columns: 3
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## dbl (3): COUPON_UPC, PRODUCT_ID, CAMPAIGN
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` r
hh_demographic      <- read_csv(file.path(data_path, "hh_demographic.csv"))
```

    ## Rows: 801 Columns: 8
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr (7): AGE_DESC, MARITAL_STATUS_CODE, INCOME_DESC, HOMEOWNER_DESC, HH_COMP...
    ## dbl (1): household_key
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` r
product             <- read_csv(file.path(data_path, "product.csv"))
```

    ## Rows: 92353 Columns: 7
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr (5): DEPARTMENT, BRAND, COMMODITY_DESC, SUB_COMMODITY_DESC, CURR_SIZE_OF...
    ## dbl (2): PRODUCT_ID, MANUFACTURER
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` r
transaction_data    <- read_csv(file.path(data_path, "transaction_data.csv"))
```

    ## Rows: 2595732 Columns: 12
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr  (1): TRANS_TIME
    ## dbl (11): household_key, BASKET_ID, DAY, PRODUCT_ID, QUANTITY, SALES_VALUE, ...
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

2.Join transaction and product tables

``` r
# Join transaction with product information
transaction_with_product <- transaction_data %>%
  left_join(product, by = "PRODUCT_ID")

#check unknowns
sum(is.na(transaction_with_product$DEPARTMENT))
```

    ## [1] 7839

``` r
colSums(is.na(transaction_with_product))
```

    ##        household_key            BASKET_ID                  DAY 
    ##                    0                    0                    0 
    ##           PRODUCT_ID             QUANTITY          SALES_VALUE 
    ##                    0                    0                    0 
    ##             STORE_ID          RETAIL_DISC           TRANS_TIME 
    ##                    0                    0                    0 
    ##              WEEK_NO          COUPON_DISC    COUPON_MATCH_DISC 
    ##                    0                    0                    0 
    ##         MANUFACTURER           DEPARTMENT                BRAND 
    ##                    0                 7839                    0 
    ##       COMMODITY_DESC   SUB_COMMODITY_DESC CURR_SIZE_OF_PRODUCT 
    ##                 7839                 7839               466746

``` r
#Replace unkown department with NA
transaction_with_product <- transaction_with_product %>%
  mutate(DEPARTMENT = ifelse(is.na(DEPARTMENT), "Unknown", DEPARTMENT))

#Replace unknown COMMODITY_DESC with NA
transaction_with_product <- transaction_with_product %>%
  mutate(COMMODITY_DESC = ifelse(is.na(COMMODITY_DESC), "Unknown", COMMODITY_DESC))

#Replace SUB_COMMODITY_DESC with NA
transaction_with_product <- transaction_with_product %>%
  mutate(SUB_COMMODITY_DESC = ifelse(is.na(SUB_COMMODITY_DESC), "Unknown", SUB_COMMODITY_DESC))
```

7839 missing values in DEPARTMENT, COMMODITY_DESC and
SUB_COMMODITY_DESC.

3.  Compute total sales of each department (sales in dollars)

``` r
#Create a new dataset to store department sales
department_sales <- transaction_with_product %>%
  group_by(DEPARTMENT) %>%
  summarise(TOTAL_DEPARTMENT_SALES = sum(SALES_VALUE, na.rm = TRUE)) %>%
  arrange(desc(TOTAL_DEPARTMENT_SALES))

# Summarize the department names only
department_summary <- transaction_with_product %>%
  count(DEPARTMENT, sort = TRUE)

# View the result
print(department_summary)
```

    ## # A tibble: 44 × 2
    ##    DEPARTMENT          n
    ##    <chr>           <int>
    ##  1 GROCERY       1646076
    ##  2 DRUG GM        277232
    ##  3 PRODUCE        257290
    ##  4 MEAT-PCKGD     111957
    ##  5 MEAT            88416
    ##  6 DELI            62787
    ##  7 PASTRY          38179
    ##  8 NUTRITION       32164
    ##  9 KIOSK-GAS       22059
    ## 10 SEAFOOD-PCKGD   11216
    ## # ℹ 34 more rows

``` r
#Inspect
summary(department_sales)
```

    ##   DEPARTMENT        TOTAL_DEPARTMENT_SALES
    ##  Length:44          Min.   :      0       
    ##  Class :character   1st Qu.:     20       
    ##  Mode  :character   Median :    840       
    ##                     Mean   : 183124       
    ##                     3rd Qu.:  45513       
    ##                     Max.   :4093814

``` r
head(department_sales, 10)
```

    ## # A tibble: 10 × 2
    ##    DEPARTMENT      TOTAL_DEPARTMENT_SALES
    ##    <chr>                            <dbl>
    ##  1 GROCERY                       4093814.
    ##  2 DRUG GM                       1055358.
    ##  3 PRODUCE                        557452.
    ##  4 MEAT                           548787.
    ##  5 KIOSK-GAS                      544222.
    ##  6 MEAT-PCKGD                     412437.
    ##  7 DELI                           260867.
    ##  8 PASTRY                         121740.
    ##  9 MISC SALES TRAN                119960.
    ## 10 NUTRITION                       97669.

4.  Pick a category with high sales

``` r
# Select a department with high sales
top_10_departments <- department_sales %>%
  arrange(desc(TOTAL_DEPARTMENT_SALES)) %>%
  slice_head(n = 10)

# View result
print(top_10_departments)
```

    ## # A tibble: 10 × 2
    ##    DEPARTMENT      TOTAL_DEPARTMENT_SALES
    ##    <chr>                            <dbl>
    ##  1 GROCERY                       4093814.
    ##  2 DRUG GM                       1055358.
    ##  3 PRODUCE                        557452.
    ##  4 MEAT                           548787.
    ##  5 KIOSK-GAS                      544222.
    ##  6 MEAT-PCKGD                     412437.
    ##  7 DELI                           260867.
    ##  8 PASTRY                         121740.
    ##  9 MISC SALES TRAN                119960.
    ## 10 NUTRITION                       97669.

Grocery department has the highest sales, followed by Produce and Frozen
Foods.

5.  Find the brand with th highest sales

``` r
# 1. Filter for only Grocery department
grocery_data <- transaction_with_product %>%
  filter(DEPARTMENT == "GROCERY")

# 2. Group by BRAND and summarize total sales
brand_sales_in_grocery <- grocery_data %>%
  group_by(BRAND) %>%
  summarise(TOTAL_BRAND_SALES = sum(SALES_VALUE, na.rm = TRUE)) %>%
  arrange(desc(TOTAL_BRAND_SALES))

# 3. Pick the top brand
top_brand_in_grocery <- brand_sales_in_grocery %>%
  slice_head(n = 5)

# View the result
print(top_brand_in_grocery)
```

    ## # A tibble: 2 × 2
    ##   BRAND    TOTAL_BRAND_SALES
    ##   <chr>                <dbl>
    ## 1 National          2987981.
    ## 2 Private           1105833.

6.  Find the SUB_COMMODITY_DESC with the highest sales in the Grocery
    department

``` r
# 1. Filter for only Grocery department
grocery_data <- transaction_with_product %>%
  filter(DEPARTMENT == "GROCERY")

# 2. Group by BRAND and summarize total sales
commodity_desc_sales_in_grocery <- grocery_data %>%
  group_by(SUB_COMMODITY_DESC) %>%
  summarise(SUB_COMMODITY_DESC_SALES = sum(SALES_VALUE, na.rm = TRUE)) %>%
  arrange(desc(SUB_COMMODITY_DESC_SALES))

# 3. Pick the top brand
top_commodity_in_grocery <- commodity_desc_sales_in_grocery %>%
  slice_head(n = 5)

# View the result
print(commodity_desc_sales_in_grocery)
```

    ## # A tibble: 736 × 2
    ##    SUB_COMMODITY_DESC             SUB_COMMODITY_DESC_SALES
    ##    <chr>                                             <dbl>
    ##  1 FLUID MILK WHITE ONLY                           161907.
    ##  2 SOFT DRINKS 12/18&15PK CAN CAR                  158188.
    ##  3 BEERALEMALT LIQUORS                             148720.
    ##  4 SHREDDED CHEESE                                  67663.
    ##  5 TOILET TISSUE                                    58630.
    ##  6 SFT DRNK 2 LITER BTL CARB INCL                   53603.
    ##  7 POTATO CHIPS                                     51481.
    ##  8 PREMIUM                                          50581.
    ##  9 SNACKS/APPETIZERS                                48469.
    ## 10 DAIRY CASE 100% PURE JUICE - O                   47539.
    ## # ℹ 726 more rows

The top two sub-commodities in the grocery department is FLUID MILK
WHITE ONLY and SOFT DRINKS 12/18&15PK CAN CAR.

7.  Check if the top sub-commodities have ads presented

``` r
# 1. Get all PRODUCT_IDs belonging to that SUB_COMMODITY_DESC
top_commodity_products <- transaction_with_product %>%
  filter(DEPARTMENT == "GROCERY", SUB_COMMODITY_DESC == "FLUID MILK WHITE ONLY") %>% 
  distinct(PRODUCT_ID)

# 2.From causal_data, filter only those products
top_commodity_causal <- causal_data %>%
  filter(PRODUCT_ID %in% top_commodity_products$PRODUCT_ID)

# 3. Summarize number of different displays
display_summary <- top_commodity_causal %>%
  count(display, sort = TRUE)

print(display_summary)
```

    ## # A tibble: 10 × 2
    ##    display      n
    ##    <chr>    <int>
    ##  1 0       190835
    ##  2 3        43440
    ##  3 9        22898
    ##  4 2         1683
    ##  5 7         1239
    ##  6 A         1186
    ##  7 1          836
    ##  8 6          273
    ##  9 4          245
    ## 10 5           95

``` r
# 4. Summarize number of different mailers
mailer_summary <- top_commodity_causal %>%
  count(mailer, sort = TRUE)

print(mailer_summary)
```

    ## # A tibble: 6 × 2
    ##   mailer      n
    ##   <chr>   <int>
    ## 1 D      181725
    ## 2 0       53982
    ## 3 H        7602
    ## 4 X        7203
    ## 5 C        7046
    ## 6 A        5172

8.  Prepare the data for user-level sales response model Y\_{ist} =
    sales of “FLUID MILK WHITE ONLY” to household i in store s in week t
    X\_{ist} = price to household i in store s in week t - tricky what
    price did they face when they don’t buy? display mailer coupon

8.a.Set target sub-commodity “FLUID MILK WHITE ONLY”

``` r
target_sub_commodity <- "FLUID MILK WHITE ONLY"

# Get all Product IDs under this sub-commodity
target_products <- product %>%
  filter(SUB_COMMODITY_DESC == target_sub_commodity) %>%
  distinct(PRODUCT_ID)

print(target_products)
```

    ## # A tibble: 194 × 1
    ##    PRODUCT_ID
    ##         <dbl>
    ##  1      28272
    ##  2      32553
    ##  3      32916
    ##  4      34160
    ##  5      34873
    ##  6      35082
    ##  7      40067
    ##  8      40885
    ##  9      41015
    ## 10      42100
    ## # ℹ 184 more rows

8.b.Build the full dataset: Sales of Household \* Store \* Week

``` r
# Get all unique households, stores, and weeks
households <- transaction_data %>% distinct(household_key)
stores <- transaction_data %>% distinct(STORE_ID)
weeks <- transaction_data %>% distinct(WEEK_NO)

# Expand grid: Household x Store x Week
panel <- crossing(households, stores, weeks)

print(panel)
```

    ## # A tibble: 148,410,000 × 3
    ##    household_key STORE_ID WEEK_NO
    ##            <dbl>    <dbl>   <dbl>
    ##  1             1        1       1
    ##  2             1        1       2
    ##  3             1        1       3
    ##  4             1        1       4
    ##  5             1        1       5
    ##  6             1        1       6
    ##  7             1        1       7
    ##  8             1        1       8
    ##  9             1        1       9
    ## 10             1        1      10
    ## # ℹ 148,409,990 more rows

8.c. Merge sales data

``` r
# Filter transactions for target milk products
milk_sales <- transaction_data %>%
  filter(PRODUCT_ID %in% target_products$PRODUCT_ID) %>%
  group_by(household_key, STORE_ID, WEEK_NO) %>%
  summarise(
    SALES_VALUE = sum(SALES_VALUE, na.rm = TRUE),
    QUANTITY = sum(QUANTITY, na.rm = TRUE),
    RETAIL_DISC = sum(RETAIL_DISC, na.rm = TRUE),
    COUPON_DISC = sum(COUPON_DISC, na.rm = TRUE),
    COUPON_MATCH_DISC = sum(COUPON_MATCH_DISC, na.rm = TRUE),
    .groups = "drop"
  )

print(milk_sales)
```

    ## # A tibble: 54,448 × 8
    ##    household_key STORE_ID WEEK_NO SALES_VALUE QUANTITY RETAIL_DISC COUPON_DISC
    ##            <dbl>    <dbl>   <dbl>       <dbl>    <dbl>       <dbl>       <dbl>
    ##  1             1      436      13        1           1       -0.89           0
    ##  2             1      436      14        2           2       -1.78           0
    ##  3             1      436      15        1.89        1        0              0
    ##  4             1      436      16        1           1       -0.89           0
    ##  5             1      436      19        1.89        1        0              0
    ##  6             1      436      20        1           1       -0.89           0
    ##  7             1      436      22        1.89        1        0              0
    ##  8             1      436      23        1.5         1       -0.39           0
    ##  9             1      436      25        1           1       -0.89           0
    ## 10             1      436      30        1.89        1        0              0
    ## # ℹ 54,438 more rows
    ## # ℹ 1 more variable: COUPON_MATCH_DISC <dbl>

8.d. Merge with the full panel

``` r
panel <- panel %>%
  left_join(milk_sales, by = c("household_key", "STORE_ID", "WEEK_NO")) %>%
  mutate(
    SALES_VALUE = replace_na(SALES_VALUE, 0),
    QUANTITY = replace_na(QUANTITY, 0),
    RETAIL_DISC = replace_na(RETAIL_DISC, 0),
    COUPON_DISC = replace_na(COUPON_DISC, 0),
    COUPON_MATCH_DISC = replace_na(COUPON_MATCH_DISC, 0)
  )

print(panel)
```

smaller left join

``` r
# Loop over weeks
for (w in weeks) {
  cat("Processing week", w, "\n")
  
  panel_small <- panel %>% filter(WEEK_NO == w)
  
  joined_small <- panel_small %>%
    left_join(milk_sales, by = c("household_key", "STORE_ID", "WEEK_NO"))
  
  # Check and add missing columns if necessary
  if (!"SALES_VALUE" %in% names(joined_small)) joined_small$SALES_VALUE <- NA
  if (!"QUANTITY" %in% names(joined_small)) joined_small$QUANTITY <- NA
  if (!"RETAIL_DISC" %in% names(joined_small)) joined_small$RETAIL_DISC <- NA
  if (!"COUPON_DISC" %in% names(joined_small)) joined_small$COUPON_DISC <- NA
  if (!"COUPON_MATCH_DISC" %in% names(joined_small)) joined_small$COUPON_MATCH_DISC <- NA
  
  # Now safe to replace NA with 0
  joined_small <- joined_small %>%
    mutate(
      SALES_VALUE = replace_na(SALES_VALUE, 0),
      QUANTITY = replace_na(QUANTITY, 0),
      RETAIL_DISC = replace_na(RETAIL_DISC, 0),
      COUPON_DISC = replace_na(COUPON_DISC, 0),
      COUPON_MATCH_DISC = replace_na(COUPON_MATCH_DISC, 0)
    )
  
  result_list[[as.character(w)]] <- joined_small
}

# After all weeks processed, bind them together
panel_final <- bind_rows(result_list)
head(panel_final)
```

sampling a subset only 500 households

``` r
# Set seed 
set.seed(123)

# Sample 500 households (you can change this number if needed)
households_small <- households %>% sample_n(500)

# Expand grid: Household x Store x Week (only sampled households)
panel_small <- crossing(households_small, stores, weeks)

# Check size
print(dim(panel_small))
```

    ## [1] 29682000        3

``` r
# Merge the small panel with milk sales
panel_small_joined <- panel_small %>%
  left_join(milk_sales, by = c("household_key", "STORE_ID", "WEEK_NO"))

# Replace NAs with 0s
needed_cols <- c("SALES_VALUE", "QUANTITY", "RETAIL_DISC", "COUPON_DISC", "COUPON_MATCH_DISC") 
panel_small_joined <- panel_small_joined %>%
  mutate(across(all_of(needed_cols), ~replace_na(., 0)))

# Check
summary(panel_small_joined)
```

    ##  household_key       STORE_ID        WEEK_NO       SALES_VALUE      
    ##  Min.   :   9.0   Min.   :    1   Min.   :  1.0   Min.   : 0.00000  
    ##  1st Qu.: 629.5   1st Qu.:  415   1st Qu.: 26.0   1st Qu.: 0.00000  
    ##  Median :1252.5   Median : 1084   Median : 51.5   Median : 0.00000  
    ##  Mean   :1243.7   Mean   : 3315   Mean   : 51.5   Mean   : 0.00102  
    ##  3rd Qu.:1883.5   3rd Qu.: 3068   3rd Qu.: 77.0   3rd Qu.: 0.00000  
    ##  Max.   :2500.0   Max.   :34280   Max.   :102.0   Max.   :51.80000  
    ##     QUANTITY        RETAIL_DISC         COUPON_DISC       COUPON_MATCH_DISC
    ##  Min.   :0.0e+00   Min.   :-2.72e+01   Min.   :-2.5e+00   Min.   :-0.45    
    ##  1st Qu.:0.0e+00   1st Qu.: 0.00e+00   1st Qu.: 0.0e+00   1st Qu.: 0.00    
    ##  Median :0.0e+00   Median : 0.00e+00   Median : 0.0e+00   Median : 0.00    
    ##  Mean   :5.9e-04   Mean   :-2.49e-04   Mean   :-9.0e-07   Mean   : 0.00    
    ##  3rd Qu.:0.0e+00   3rd Qu.: 0.00e+00   3rd Qu.: 0.0e+00   3rd Qu.: 0.00    
    ##  Max.   :3.2e+01   Max.   : 0.00e+00   Max.   : 0.0e+00   Max.   : 0.00

8.e. Merge with price data (replace NAs with store average price at that
week if a sale has happened in that store at that week, then NAs with
average store price during all periods if no sales happened during that
week but at other weeks, and drop stores with no sales at all bcs it
means that store does not sell milk)

``` r
# Compute average price at each store-week (across all milk buyers)
avg_price_store_week <- transaction_data %>%
  filter(PRODUCT_ID %in% target_products$PRODUCT_ID, QUANTITY > 0) %>%
  mutate(unit_price = SALES_VALUE / QUANTITY) %>%
  group_by(STORE_ID, WEEK_NO) %>%
  summarise(avg_price = mean(unit_price, na.rm = TRUE), .groups = "drop")

# Preview
print(head(avg_price_store_week))
```

    ## # A tibble: 6 × 3
    ##   STORE_ID WEEK_NO avg_price
    ##      <dbl>   <dbl>     <dbl>
    ## 1       19      98      1.99
    ## 2       22      32      3.02
    ## 3       27       3      2.49
    ## 4       27       4      2.49
    ## 5       27       6      3.09
    ## 6       27       7      3.49

``` r
# Compute overall store-level average price 
avg_price_store <- transaction_data %>%
  filter(PRODUCT_ID %in% target_products$PRODUCT_ID, QUANTITY > 0) %>%
  mutate(unit_price = SALES_VALUE / QUANTITY) %>%
  group_by(STORE_ID) %>%
  summarise(store_avg_price = mean(unit_price, na.rm = TRUE), .groups = "drop")

print(head(avg_price_store))
```

    ## # A tibble: 6 × 2
    ##   STORE_ID store_avg_price
    ##      <dbl>           <dbl>
    ## 1       19            1.99
    ## 2       22            3.02
    ## 3       27            2.64
    ## 4       45            1.97
    ## 5       46            3.16
    ## 6       53            1.99

``` r
# Merge store-week avg price into panel 
panel_small_joined <- panel_small_joined %>%
  left_join(avg_price_store_week, by = c("STORE_ID", "WEEK_NO"))

# Merge overall store avg price into panel 
panel_small_joined <- panel_small_joined %>%
  left_join(avg_price_store, by = "STORE_ID")

# Fill missing avg price: first store-week, then store avg 
panel_small_joined <- panel_small_joined %>%
  mutate(
    avg_price_filled = coalesce(avg_price, store_avg_price)
  )

# Drop stores that have no milk sales ever
panel_small_joined <- panel_small_joined %>%
  filter(!is.na(avg_price_filled))

# Calculate final price variable 
panel_small_joined <- panel_small_joined %>%
  mutate(
    observed_price = if_else(QUANTITY > 0, SALES_VALUE / QUANTITY, NA_real_),
    price = coalesce(observed_price, avg_price_filled)
  )

#Check 
summary(panel_small_joined$price)
```

    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    ##   0.250   1.560   1.990   1.985   2.390   4.390

``` r
sum(is.na(panel_small_joined$price))
```

    ## [1] 0

9.  Merge ad types data

``` r
#From causal_data, filter only our targeted products
causal_milk <- causal_data %>%
  filter(PRODUCT_ID %in% target_products$PRODUCT_ID)

# Collapse to store-week level: if any product is displayed or mailed, mark it as 1
causal_milk_store_week <- causal_milk %>%
  group_by(STORE_ID, WEEK_NO) %>%
  summarise(
    display = max(display, na.rm = TRUE),
    mailer = max(mailer, na.rm = TRUE),
    .groups = "drop"
  )

# Preview
print(causal_milk_store_week)
```

    ## # A tibble: 6,854 × 4
    ##    STORE_ID WEEK_NO display mailer
    ##       <dbl>   <dbl> <chr>   <chr> 
    ##  1      286       9 0       D     
    ##  2      286      11 0       D     
    ##  3      286      13 0       D     
    ##  4      286      14 0       D     
    ##  5      286      16 0       D     
    ##  6      286      18 0       D     
    ##  7      286      20 0       D     
    ##  8      286      21 0       A     
    ##  9      286      23 0       D     
    ## 10      286      25 0       D     
    ## # ℹ 6,844 more rows

``` r
#Merge into main
panel_small_joined <- panel_small_joined %>%
  left_join(causal_milk_store_week, by = c("STORE_ID", "WEEK_NO"))

#Replace NAs with 0 (is this reasonable?)
panel_small_joined <- panel_small_joined %>%
  mutate(
    display = if_else(is.na(display), "0", as.character(display)),
    mailer = if_else(is.na(mailer), "0", as.character(mailer))
  )

#Check
panel_small_joined %>%
  filter(STORE_ID == 286, WEEK_NO %in% c(9, 11, 13, 14,21)) %>%
  select(display, mailer)
```

    ## # A tibble: 2,500 × 2
    ##    display mailer
    ##    <chr>   <chr> 
    ##  1 0       D     
    ##  2 0       D     
    ##  3 0       D     
    ##  4 0       D     
    ##  5 0       A     
    ##  6 0       D     
    ##  7 0       D     
    ##  8 0       D     
    ##  9 0       D     
    ## 10 0       A     
    ## # ℹ 2,490 more rows

10. User response Model Yist = SALES_VALUE ist Xist = price ist display
    ist RETAIL_DISC ist COUPON_DISC ist COUPON_MATCH_DISC ist

``` r
#Convert to factors
panel_small_joined <- panel_small_joined %>%
  mutate(
    display_cat = factor(display),
    mailer_cat = factor(mailer)
  )

# Fit the model
ols_model_cat <- lm(
  SALES_VALUE ~ price + display_cat + mailer_cat +
    RETAIL_DISC + COUPON_DISC + COUPON_MATCH_DISC,
  data = panel_small_joined
)

options(scipen = 999)
summary(ols_model_cat)
```

    ## 
    ## Call:
    ## lm(formula = SALES_VALUE ~ price + display_cat + mailer_cat + 
    ##     RETAIL_DISC + COUPON_DISC + COUPON_MATCH_DISC, data = panel_small_joined)
    ## 
    ## Residuals:
    ##    Min     1Q Median     3Q    Max 
    ## -7.052 -0.002 -0.001  0.000 51.798 
    ## 
    ## Coefficients:
    ##                      Estimate  Std. Error   t value             Pr(>|t|)    
    ## (Intercept)       -0.00266223  0.00006993   -38.070 < 0.0000000000000002 ***
    ## price              0.00170390  0.00003259    52.285 < 0.0000000000000002 ***
    ## display_cat1       0.00398797  0.00059599     6.691      0.0000000000221 ***
    ## display_cat2       0.00167831  0.00037083     4.526      0.0000060154802 ***
    ## display_cat3       0.00264820  0.00010033    26.394 < 0.0000000000000002 ***
    ## display_cat4       0.00206873  0.00117213     1.765             0.077576 .  
    ## display_cat5      -0.00055579  0.00148239    -0.375             0.707715    
    ## display_cat6       0.00353914  0.00099997     3.539             0.000401 ***
    ## display_cat7       0.00194253  0.00044097     4.405      0.0000105709794 ***
    ## display_cat9       0.00323253  0.00010595    30.509 < 0.0000000000000002 ***
    ## display_catA       0.00343602  0.00048437     7.094      0.0000000000013 ***
    ## mailer_catA        0.00360518  0.00016243    22.195 < 0.0000000000000002 ***
    ## mailer_catC        0.00403805  0.00022814    17.700 < 0.0000000000000002 ***
    ## mailer_catD        0.00163788  0.00005625    29.120 < 0.0000000000000002 ***
    ## mailer_catH        0.00350855  0.00032142    10.916 < 0.0000000000000002 ***
    ## mailer_catX        0.00065997  0.00022697     2.908             0.003641 ** 
    ## RETAIL_DISC       -1.33384708  0.00048971 -2723.723 < 0.0000000000000002 ***
    ## COUPON_DISC       -1.60862066  0.00975705  -164.868 < 0.0000000000000002 ***
    ## COUPON_MATCH_DISC -4.56202532  0.16511781   -27.629 < 0.0000000000000002 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.07411 on 15605981 degrees of freedom
    ## Multiple R-squared:  0.3237, Adjusted R-squared:  0.3237 
    ## F-statistic: 4.15e+05 on 18 and 15605981 DF,  p-value: < 0.00000000000000022

11. Another model at brand level (“National” vs. “Private”)

Prepare the data for user-level sales response model Y\_{ist} = sales of
“National” to household i in store s in week t X\_{ist} = price to
household i in store s in week t

``` r
#.Set target sub-commodity “National”
target_sub_commodity <- "National"

# Get all Product IDs under this sub-commodity
target_products <- product %>%
  filter(BRAND == target_sub_commodity) %>%
  distinct(PRODUCT_ID)

print(target_products)
```

    ## # A tibble: 78,537 × 1
    ##    PRODUCT_ID
    ##         <dbl>
    ##  1      25671
    ##  2      26081
    ##  3      26889
    ##  4      27021
    ##  5      28513
    ##  6      28748
    ##  7      28919
    ##  8      28929
    ##  9      29096
    ## 10      29252
    ## # ℹ 78,527 more rows

Build the full dataset: Sales of Household \* Store \* Week

``` r
national_sales <- transaction_data %>%
  filter(PRODUCT_ID %in% target_products$PRODUCT_ID) %>%
  group_by(household_key, STORE_ID, WEEK_NO) %>%
  summarise(
    SALES_VALUE = sum(SALES_VALUE, na.rm = TRUE),
    QUANTITY = sum(QUANTITY, na.rm = TRUE),
    RETAIL_DISC = sum(RETAIL_DISC, na.rm = TRUE),
    COUPON_DISC = sum(COUPON_DISC, na.rm = TRUE),
    COUPON_MATCH_DISC = sum(COUPON_MATCH_DISC, na.rm = TRUE),
    .groups = "drop"
  )

print(national_sales)
```

    ## # A tibble: 143,449 × 8
    ##    household_key STORE_ID WEEK_NO SALES_VALUE QUANTITY RETAIL_DISC COUPON_DISC
    ##            <dbl>    <dbl>   <dbl>       <dbl>    <dbl>       <dbl>       <dbl>
    ##  1             1      436       8        65.9       28      -11.1        -1   
    ##  2             1      436      10        38.1       13       -7           0   
    ##  3             1      436      13        18.4        8       -5.54        0   
    ##  4             1      436      14        56.6       28       -7.21       -1.5 
    ##  5             1      436      15        43.7       16      -11.4         0   
    ##  6             1      436      16        18.8        7       -1.98        0   
    ##  7             1      436      19        99.0       39       -5.81       -2.25
    ##  8             1      436      20        78.7       36      -12.3         0   
    ##  9             1      436      22        61.9       27      -10.6         0   
    ## 10             1      436      23        46.6       19       -2.72        0   
    ## # ℹ 143,439 more rows
    ## # ℹ 1 more variable: COUPON_MATCH_DISC <dbl>

Sampling 500 households

``` r
# Get all unique households, stores, and weeks
households <- transaction_data %>% distinct(household_key)
stores <- transaction_data %>% distinct(STORE_ID)
weeks <- transaction_data %>% distinct(WEEK_NO)

# Set seed 
set.seed(123)

# Sample 500 households (you can change this number if needed)
households_small <- households %>% sample_n(100)

# Expand grid: Household x Store x Week (only sampled households)
panel_small <- crossing(households_small, stores, weeks)

# Merge the small panel with milk sales
panel_small_joined <- panel_small %>%
  left_join(national_sales, by = c("household_key", "STORE_ID", "WEEK_NO"))

# Replace NAs with 0s
needed_cols <- c("SALES_VALUE", "QUANTITY", "RETAIL_DISC", "COUPON_DISC", "COUPON_MATCH_DISC") 
panel_small_joined <- panel_small_joined %>%
  mutate(across(all_of(needed_cols), ~replace_na(., 0)))

# Check
summary(panel_small_joined)
```

    ##  household_key       STORE_ID        WEEK_NO       SALES_VALUE      
    ##  Min.   :  24.0   Min.   :    1   Min.   :  1.0   Min.   :  0.0000  
    ##  1st Qu.: 648.8   1st Qu.:  415   1st Qu.: 26.0   1st Qu.:  0.0000  
    ##  Median :1141.0   Median : 1084   Median : 51.5   Median :  0.0000  
    ##  Mean   :1251.1   Mean   : 3315   Mean   : 51.5   Mean   :  0.0357  
    ##  3rd Qu.:1905.0   3rd Qu.: 3068   3rd Qu.: 77.0   3rd Qu.:  0.0000  
    ##  Max.   :2499.0   Max.   :34280   Max.   :102.0   Max.   :400.7900  
    ##     QUANTITY          RETAIL_DISC          COUPON_DISC       
    ##  Min.   :  0.00000   Min.   :-180.79000   Min.   :-34.00000  
    ##  1st Qu.:  0.00000   1st Qu.:   0.00000   1st Qu.:  0.00000  
    ##  Median :  0.00000   Median :   0.00000   Median :  0.00000  
    ##  Mean   :  0.01506   Mean   :  -0.00673   Mean   : -0.00021  
    ##  3rd Qu.:  0.00000   3rd Qu.:   0.00000   3rd Qu.:  0.00000  
    ##  Max.   :163.00000   Max.   :   0.00000   Max.   :  0.00000  
    ##  COUPON_MATCH_DISC 
    ##  Min.   :-8.50000  
    ##  1st Qu.: 0.00000  
    ##  Median : 0.00000  
    ##  Mean   :-0.00004  
    ##  3rd Qu.: 0.00000  
    ##  Max.   : 0.00000

Merge with price data

``` r
# Compute average price at each store-week (across all national products buyers)
avg_price_store_week <- transaction_data %>%
  filter(PRODUCT_ID %in% target_products$PRODUCT_ID, QUANTITY > 0) %>%
  mutate(unit_price = SALES_VALUE / QUANTITY) %>%
  group_by(STORE_ID, WEEK_NO) %>%
  summarise(avg_price = mean(unit_price, na.rm = TRUE), .groups = "drop")

# Compute overall store-level average price 
avg_price_store <- transaction_data %>%
  filter(PRODUCT_ID %in% target_products$PRODUCT_ID, QUANTITY > 0) %>%
  mutate(unit_price = SALES_VALUE / QUANTITY) %>%
  group_by(STORE_ID) %>%
  summarise(store_avg_price = mean(unit_price, na.rm = TRUE), .groups = "drop")

# Merge store-week avg price into panel 
panel_small_joined <- panel_small_joined %>%
  left_join(avg_price_store_week, by = c("STORE_ID", "WEEK_NO"))

# Merge overall store avg price into panel 
panel_small_joined <- panel_small_joined %>%
  left_join(avg_price_store, by = "STORE_ID")

# Fill missing avg price: first store-week, then store avg 
panel_small_joined <- panel_small_joined %>%
  mutate(
    avg_price_filled = coalesce(avg_price, store_avg_price)
  )

# Drop stores that have no milk sales ever
panel_small_joined <- panel_small_joined %>%
  filter(!is.na(avg_price_filled))

# Calculate final price variable 
panel_small_joined <- panel_small_joined %>%
  mutate(
    observed_price = if_else(QUANTITY > 0, SALES_VALUE / QUANTITY, NA_real_),
    price = coalesce(observed_price, avg_price_filled)
  )

#Check 
summary(panel_small_joined$price)
```

    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    ##   0.000   2.123   2.604   2.862   3.210 200.395

``` r
sum(is.na(panel_small_joined$price))
```

    ## [1] 0

Merge ad types data

``` r
#From causal_data, filter only our targeted products
causal_national <- causal_data %>%
  filter(PRODUCT_ID %in% target_products$PRODUCT_ID)

# Collapse to store-week level: if any product is displayed or mailed, mark it as 1
causal_national_store_week <- causal_national %>%
  group_by(STORE_ID, WEEK_NO) %>%
  summarise(
    display = max(display, na.rm = TRUE),
    mailer = max(mailer, na.rm = TRUE),
    .groups = "drop"
  )

#Merge into main
panel_small_joined <- panel_small_joined %>%
  left_join(causal_national_store_week, by = c("STORE_ID", "WEEK_NO"))

#Replace NAs with 0 (is this reasonable?)
panel_small_joined <- panel_small_joined %>%
  mutate(
    display = if_else(is.na(display), "0", as.character(display)),
    mailer = if_else(is.na(mailer), "0", as.character(mailer))
  )

#Convert to factors
panel_small_joined <- panel_small_joined %>%
  mutate(
    display_cat = factor(display),
    mailer_cat = factor(mailer)
  )

# Fit the model
ols_model_cat <- lm(
  SALES_VALUE ~ price + display_cat + mailer_cat +
    RETAIL_DISC + COUPON_DISC + COUPON_MATCH_DISC,
  data = panel_small_joined
)

options(scipen = 999)
summary(ols_model_cat)
```

    ## 
    ## Call:
    ## lm(formula = SALES_VALUE ~ price + display_cat + mailer_cat + 
    ##     RETAIL_DISC + COUPON_DISC + COUPON_MATCH_DISC, data = panel_small_joined)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -549.40   -0.01    0.00    0.00  399.45 
    ## 
    ## Coefficients:
    ##                     Estimate Std. Error   t value             Pr(>|t|)    
    ## (Intercept)       -0.0189795  0.0008034   -23.623 < 0.0000000000000002 ***
    ## price              0.0067857  0.0002261    30.014 < 0.0000000000000002 ***
    ## display_cat5      -0.0470089  0.0447665    -1.050             0.293676    
    ## display_cat6      -0.0546915  0.0993371    -0.551             0.581932    
    ## display_cat7       0.0103295  0.0087478     1.181             0.237679    
    ## display_cat9       0.0247992  0.0063566     3.901  0.00009567413816503 ***
    ## display_catA      -0.0028790  0.0061951    -0.465             0.642135    
    ## mailer_catD        0.0028342  0.0991447     0.029             0.977194    
    ## mailer_catF        0.0524681  0.0066136     7.933  0.00000000000000213 ***
    ## mailer_catH        0.0555280  0.0062150     8.935 < 0.0000000000000002 ***
    ## mailer_catJ        0.0322047  0.0092555     3.480             0.000502 ***
    ## mailer_catL        0.0530734  0.0063573     8.348 < 0.0000000000000002 ***
    ## mailer_catP       -0.0235589  0.0499656    -0.472             0.637282    
    ## mailer_catX        0.0625897  0.0064245     9.742 < 0.0000000000000002 ***
    ## mailer_catZ        0.0681313  0.0074886     9.098 < 0.0000000000000002 ***
    ## RETAIL_DISC       -3.6484667  0.0011664 -3127.862 < 0.0000000000000002 ***
    ## COUPON_DISC       -0.4141556  0.0150666   -27.488 < 0.0000000000000002 ***
    ## COUPON_MATCH_DISC -3.5838773  0.0567566   -63.145 < 0.0000000000000002 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.9914 on 5609982 degrees of freedom
    ## Multiple R-squared:  0.683,  Adjusted R-squared:  0.683 
    ## F-statistic: 7.109e+05 on 17 and 5609982 DF,  p-value: < 0.00000000000000022
