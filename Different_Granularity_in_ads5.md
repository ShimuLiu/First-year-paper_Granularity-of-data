Different_Granularity_in_ads3
================
2025-05-05

Run BEERALEMALT LIQUORS product at a time with the top 5 products. - one
x product is feature or display - two x’s feature and display - all the
categories of feature and display - learn how to show multiple models
with stargazer - show three models in one table

1.  Load the dataset

``` r
# Load libraries
library(tidyverse)
```

    ## -- Attaching core tidyverse packages ------------------------ tidyverse 2.0.0 --
    ## v dplyr     1.1.4     v readr     2.1.5
    ## v forcats   1.0.0     v stringr   1.5.1
    ## v ggplot2   3.5.2     v tibble    3.2.1
    ## v lubridate 1.9.4     v tidyr     1.3.1
    ## v purrr     1.0.4     
    ## -- Conflicts ------------------------------------------ tidyverse_conflicts() --
    ## x dplyr::filter() masks stats::filter()
    ## x dplyr::lag()    masks stats::lag()
    ## i Use the conflicted package (<http://conflicted.r-lib.org/>) to force all conflicts to become errors

``` r
# Set data path
data_path <-  "C:\\Users\\moomo\\Desktop\\Drexel Spring 2025\\First-year paper\\archive"

# Load datasets
campaign_desc       <- read_csv(file.path(data_path, "campaign_desc.csv"))
```

    ## Rows: 30 Columns: 4
    ## -- Column specification --------------------------------------------------------
    ## Delimiter: ","
    ## chr (1): DESCRIPTION
    ## dbl (3): CAMPAIGN, START_DAY, END_DAY
    ## 
    ## i Use `spec()` to retrieve the full column specification for this data.
    ## i Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` r
campaign_table      <- read_csv(file.path(data_path, "campaign_table.csv"))
```

    ## Rows: 7208 Columns: 3
    ## -- Column specification --------------------------------------------------------
    ## Delimiter: ","
    ## chr (1): DESCRIPTION
    ## dbl (2): household_key, CAMPAIGN
    ## 
    ## i Use `spec()` to retrieve the full column specification for this data.
    ## i Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` r
causal_data         <- read_csv(file.path(data_path, "causal_data.csv"))
```

    ## Rows: 36786524 Columns: 5
    ## -- Column specification --------------------------------------------------------
    ## Delimiter: ","
    ## chr (2): display, mailer
    ## dbl (3): PRODUCT_ID, STORE_ID, WEEK_NO
    ## 
    ## i Use `spec()` to retrieve the full column specification for this data.
    ## i Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` r
coupon_redempt      <- read_csv(file.path(data_path, "coupon_redempt.csv"))
```

    ## Rows: 2318 Columns: 4
    ## -- Column specification --------------------------------------------------------
    ## Delimiter: ","
    ## dbl (4): household_key, DAY, COUPON_UPC, CAMPAIGN
    ## 
    ## i Use `spec()` to retrieve the full column specification for this data.
    ## i Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` r
coupon              <- read_csv(file.path(data_path, "coupon.csv"))
```

    ## Rows: 124548 Columns: 3
    ## -- Column specification --------------------------------------------------------
    ## Delimiter: ","
    ## dbl (3): COUPON_UPC, PRODUCT_ID, CAMPAIGN
    ## 
    ## i Use `spec()` to retrieve the full column specification for this data.
    ## i Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` r
hh_demographic      <- read_csv(file.path(data_path, "hh_demographic.csv"))
```

    ## Rows: 801 Columns: 8
    ## -- Column specification --------------------------------------------------------
    ## Delimiter: ","
    ## chr (7): AGE_DESC, MARITAL_STATUS_CODE, INCOME_DESC, HOMEOWNER_DESC, HH_COMP...
    ## dbl (1): household_key
    ## 
    ## i Use `spec()` to retrieve the full column specification for this data.
    ## i Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` r
product             <- read_csv(file.path(data_path, "product.csv"))
```

    ## Rows: 92353 Columns: 7
    ## -- Column specification --------------------------------------------------------
    ## Delimiter: ","
    ## chr (5): DEPARTMENT, BRAND, COMMODITY_DESC, SUB_COMMODITY_DESC, CURR_SIZE_OF...
    ## dbl (2): PRODUCT_ID, MANUFACTURER
    ## 
    ## i Use `spec()` to retrieve the full column specification for this data.
    ## i Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` r
transaction_data    <- read_csv(file.path(data_path, "transaction_data.csv"))
```

    ## Rows: 2595732 Columns: 12
    ## -- Column specification --------------------------------------------------------
    ## Delimiter: ","
    ## chr  (1): TRANS_TIME
    ## dbl (11): household_key, BASKET_ID, DAY, PRODUCT_ID, QUANTITY, SALES_VALUE, ...
    ## 
    ## i Use `spec()` to retrieve the full column specification for this data.
    ## i Specify the column types or set `show_col_types = FALSE` to quiet this message.

2.Keep only TOILET TISSUE

``` r
milk_ids <- product %>%
  filter(SUB_COMMODITY_DESC == "TOILET TISSUE") %>%
  pull(PRODUCT_ID)

transaction_data <- transaction_data %>%
  filter(PRODUCT_ID %in% milk_ids)

print(transaction_data)
```

    ## # A tibble: 13,310 x 12
    ##    household_key   BASKET_ID   DAY PRODUCT_ID QUANTITY SALES_VALUE STORE_ID
    ##            <dbl>       <dbl> <dbl>      <dbl>    <dbl>       <dbl>    <dbl>
    ##  1          1172 26985025264     1    1075214        1        5.99      396
    ##  2          1060 26985040735     1    9553288        1        8.49      315
    ##  3           347 27008813029     3    1040145        1        4.99      367
    ##  4          2105 27008940074     2    9420312        1        0.99      446
    ##  5           924 27009222177     3    9420312        1        0.99      400
    ##  6           474 27021262230     4    1065593        1        1         352
    ##  7          1558 27021343894     4    1083733        1        2.19      359
    ##  8          2352 27021363165     4    1070272        1        3.99      321
    ##  9           474 27031281714     5    1065593        1        1         352
    ## 10          2328 27045601594     6     933354        1        7.49      406
    ## # i 13,300 more rows
    ## # i 5 more variables: RETAIL_DISC <dbl>, TRANS_TIME <chr>, WEEK_NO <dbl>,
    ## #   COUPON_DISC <dbl>, COUPON_MATCH_DISC <dbl>

3.Join transaction and product tables

``` r
# Join transaction with product information
transaction_with_product <- transaction_data %>%
  left_join(product, by = "PRODUCT_ID")
```

4.  Top 5 selling products in “TOILET TISSUE”

``` r
top5_milk_products <- transaction_with_product %>%
  group_by(PRODUCT_ID) %>%
  summarise(TOTAL_SALES = sum(SALES_VALUE, na.rm = TRUE)) %>%
  arrange(desc(TOTAL_SALES)) %>%
  slice_head(n = 5)

print(top5_milk_products)
```

    ## # A tibble: 5 x 2
    ##   PRODUCT_ID TOTAL_SALES
    ##        <dbl>       <dbl>
    ## 1    1065593       5032.
    ## 2    1075214       3926.
    ## 3   10149640       3782.
    ## 4    6442594       2968.
    ## 5    9553288       2417.

check for the first product

``` r
check1 <-causal_data %>%
  filter(PRODUCT_ID == 1065593)

print(check1)
```

    ## # A tibble: 1,653 x 5
    ##    PRODUCT_ID STORE_ID WEEK_NO display mailer
    ##         <dbl>    <dbl>   <dbl> <chr>   <chr> 
    ##  1    1065593      286      20 5       A     
    ##  2    1065593      286      35 5       C     
    ##  3    1065593      286      37 5       0     
    ##  4    1065593      286      43 0       D     
    ##  5    1065593      286      49 5       C     
    ##  6    1065593      286      50 5       C     
    ##  7    1065593      286      51 5       0     
    ##  8    1065593      286      52 5       0     
    ##  9    1065593      286      53 9       F     
    ## 10    1065593      286      54 9       C     
    ## # i 1,643 more rows

5.  Generate 5 data panels col1: Product IDs (same in one panel)

col2: Store IDs

col3: Week numbers (9 to 201)

col4: Total sales (from transaction data)

Col5: Price (Sales/Quantity)

col6: Ad exposure level 1 (0, 1 for display, 2 for feature, 3 for both)

col7: Display1 (0,1)

col8: Feature1 (0,1)

col9: Display2 (0, 1:Store Front, 2:Store Rear, 3: Front End Cap,
4:Mid-Aisle End Cap, 5:Rear End Cap, 6:Side-Aisle End Cap, 7:In-Aisle,
9:Secondary Location Display, A:In-Shelf)

col10: Feature2 (0, A:Interior page feature, C:Interior page line item,
D:Front page feature, F:Back page feature, H:Wrap front feature, J:Wrap
interior coupon, L:Wrap back feature, P:Interior page coupon, X:Free on
interior page, Z:Free on front page, back page or wrap

``` r
top5_ids <- c(1065593, 1075214, 10149640, 6442594, 9553288)
#weeks <- tibble(WEEK_NO = 9:59)
weeks <- transaction_data %>%
  distinct(WEEK_NO) %>%
  arrange(WEEK_NO)
product_panel_list <- list()

# Initialize combined panel
combined_panel <- tibble()

# 1. Store-week average price for each product
avg_price_store_week <- transaction_data %>%
  filter(QUANTITY > 0) %>%
  mutate(unit_price = SALES_VALUE / QUANTITY) %>%
  group_by(PRODUCT_ID, STORE_ID, WEEK_NO) %>%
  summarise(avg_price = mean(unit_price, na.rm = TRUE), .groups = "drop")

# 2. Overall store average price per product (fallback)
avg_price_store <- transaction_data %>%
  filter(QUANTITY > 0) %>%
  mutate(unit_price = SALES_VALUE / QUANTITY) %>%
  group_by(PRODUCT_ID, STORE_ID) %>%
  summarise(store_avg_price = mean(unit_price, na.rm = TRUE), .groups = "drop")

for (prod_id in top5_ids) {
  cat("Processing product:", prod_id, "\n")
  
  stores_with_product <- causal_data %>%
    filter(PRODUCT_ID == prod_id) %>%
    distinct(STORE_ID)
  
  panel_base <- crossing(stores_with_product, weeks)

  sales_aggregated <- transaction_data %>%
    filter(PRODUCT_ID == prod_id) %>%
    group_by(STORE_ID, WEEK_NO) %>%
    summarise(TOTAL_SALES = sum(SALES_VALUE, na.rm = TRUE), .groups = "drop")
  
  ads_info <- causal_data %>%
    filter(PRODUCT_ID == prod_id) %>%
    mutate(
      display1 = if_else(display != "0", 1, 0),
      feature1 = if_else(mailer != "0", 1, 0),
      display2 = display,
      feature2 = mailer
    ) %>%
    select(STORE_ID, WEEK_NO, display1, feature1, display2, feature2) %>%
    distinct() %>%
    mutate(
         adtype = if_else(display1 == 1 | feature1 == 1, 1, 0)
      )

  panel_final <- panel_base %>%
    left_join(sales_aggregated, by = c("STORE_ID", "WEEK_NO")) %>%
    left_join(ads_info, by = c("STORE_ID", "WEEK_NO")) %>%
    mutate(
      TOTAL_SALES = replace_na(TOTAL_SALES, 0),
      display1 = replace_na(display1, 0),
      feature1 = replace_na(feature1, 0),
      adtype = replace_na(adtype, 0),
      display2 = replace_na(display2, "0"),
      feature2 = replace_na(feature2, "0")
    ) %>%
    left_join(avg_price_store_week %>% filter(PRODUCT_ID == prod_id), 
              by = c("STORE_ID", "WEEK_NO")) %>%
    left_join(avg_price_store %>% filter(PRODUCT_ID == prod_id), 
              by = "STORE_ID") %>%
    mutate(
      price = coalesce(avg_price, store_avg_price),
      price = round(price, 4),
      PRODUCT_ID = prod_id
    ) %>%
    select(PRODUCT_ID, STORE_ID, WEEK_NO, TOTAL_SALES, price, adtype, display1, feature1, display2, feature2)

  combined_panel <- bind_rows(combined_panel, panel_final)
}
```

    ## Processing product: 1065593 
    ## Processing product: 1075214 
    ## Processing product: 10149640 
    ## Processing product: 6442594 
    ## Processing product: 9553288

``` r
head(combined_panel)
```

    ## # A tibble: 6 x 10
    ##   PRODUCT_ID STORE_ID WEEK_NO TOTAL_SALES price adtype display1 feature1
    ##        <dbl>    <dbl>   <dbl>       <dbl> <dbl>  <dbl>    <dbl>    <dbl>
    ## 1    1065593      286       1           0     1      0        0        0
    ## 2    1065593      286       2           0     1      0        0        0
    ## 3    1065593      286       3           0     1      0        0        0
    ## 4    1065593      286       4           0     1      0        0        0
    ## 5    1065593      286       5           0     1      0        0        0
    ## 6    1065593      286       6           0     1      0        0        0
    ## # i 2 more variables: display2 <chr>, feature2 <chr>

``` r
combined_panel %>%
  distinct(PRODUCT_ID)
```

    ## # A tibble: 5 x 1
    ##   PRODUCT_ID
    ##        <dbl>
    ## 1    1065593
    ## 2    1075214
    ## 3   10149640
    ## 4    6442594
    ## 5    9553288

6.  Run the model Model1: Y = weekly sales at each store X = Product
    IDs, price, ad exposure (0, 1) comparing group 1 (display only)
    vs. group 0 (no ad)

Model2: Y = weekly sales at each store X = price, display1 (0,1),
feature1 (0,1) comparing all observations with display1 = 1 (including
the cases when both are 1) Only differs in the interaction term? if add
a interaction this is equal to model 1?

Model3: Y = weekly sales at each store X = price, display2
(0,1,2,3,4,5,6,7,9,A), feature2 (0,A,C,D,F,H,J,L,P,X,Z)

``` r
# Load necessary package
#install.packages("stargazer")
library(stargazer)
```

    ## 
    ## Please cite as:

    ##  Hlavac, Marek (2022). stargazer: Well-Formatted Regression and Summary Statistics Tables.

    ##  R package version 5.2.3. https://CRAN.R-project.org/package=stargazer

``` r
# Extract the first product panel
df <- combined_panel

# Convert needed columns to factor
df <- df %>%
  mutate(
    PRODUCT_ID = factor(PRODUCT_ID, levels = c(859075, setdiff(unique(df$PRODUCT_ID), 859075))),
    adtype = factor(adtype),
    display1 = factor(display1),
    feature1 = factor(feature1),
    display2 = factor(display2),
    feature2 = factor(feature2)
  )

#split the data 9:59 50 weeks for trains, 60:201 41 weeks for test.
train_df <- df %>% filter(WEEK_NO >= 9, WEEK_NO <= 91)
test_df  <- df %>% filter(WEEK_NO > 91)

# Fit the three models
model1 <- lm(TOTAL_SALES ~ price + adtype + PRODUCT_ID, data = train_df)
model2 <- lm(TOTAL_SALES ~ price + display1 + feature1 + PRODUCT_ID, data = train_df)
model3 <- lm(TOTAL_SALES ~ price +display2 + feature2 + PRODUCT_ID, data = train_df)

# Display models side-by-side
stargazer(model1, model2, model3,
          type = "text",
          title = "Store-Week Sales Models by Ad Exposure (Product ID: 1029743)",
          column.labels = c("AdType", "Binary Flags", "Granular Types"),
          dep.var.labels = "Weekly Sales",
          omit.stat = c("f", "ser"))
```

    ## 
    ## Store-Week Sales Models by Ad Exposure (Product ID: 1029743)
    ## ========================================================
    ##                             Dependent variable:         
    ##                    -------------------------------------
    ##                                Weekly Sales             
    ##                     AdType   Binary Flags Granular Types
    ##                       (1)        (2)           (3)      
    ## --------------------------------------------------------
    ## price              0.210***    0.214***      0.214***   
    ##                     (0.014)    (0.014)       (0.014)    
    ##                                                         
    ## adtype1            0.530***                             
    ##                     (0.020)                             
    ##                                                         
    ## display11                      0.394***                 
    ##                                (0.028)                  
    ##                                                         
    ## feature11                      0.345***                 
    ##                                (0.028)                  
    ##                                                         
    ## display21                                     0.118     
    ##                                              (0.141)    
    ##                                                         
    ## display22                                    0.231***   
    ##                                              (0.076)    
    ##                                                         
    ## display23                                    0.415***   
    ##                                              (0.045)    
    ##                                                         
    ## display24                                    0.679***   
    ##                                              (0.142)    
    ##                                                         
    ## display25                                    0.370***   
    ##                                              (0.043)    
    ##                                                         
    ## display26                                    0.358**    
    ##                                              (0.177)    
    ##                                                         
    ## display27                                    0.496***   
    ##                                              (0.048)    
    ##                                                         
    ## display29                                    0.239**    
    ##                                              (0.098)    
    ##                                                         
    ## display2A                                    0.590***   
    ##                                              (0.199)    
    ##                                                         
    ## feature2A                                    0.351***   
    ##                                              (0.032)    
    ##                                                         
    ## feature2C                                    0.240***   
    ##                                              (0.075)    
    ##                                                         
    ## feature2D                                    0.516***   
    ##                                              (0.089)    
    ##                                                         
    ## feature2F                                     0.195     
    ##                                              (0.147)    
    ##                                                         
    ## feature2H                                     0.239     
    ##                                              (0.162)    
    ##                                                         
    ## feature2J                                    -0.289*    
    ##                                              (0.160)    
    ##                                                         
    ## feature2L                                    0.422***   
    ##                                              (0.114)    
    ##                                                         
    ## PRODUCT_ID1075214  -1.311***  -1.360***     -1.375***   
    ##                     (0.081)    (0.081)       (0.082)    
    ##                                                         
    ## PRODUCT_ID10149640 -1.271***  -1.322***     -1.337***   
    ##                     (0.077)    (0.076)       (0.077)    
    ##                                                         
    ## PRODUCT_ID6442594  -1.115***  -1.159***     -1.166***   
    ##                     (0.070)    (0.070)       (0.070)    
    ##                                                         
    ## PRODUCT_ID9553288  -1.365***  -1.409***     -1.422***   
    ##                     (0.080)    (0.080)       (0.080)    
    ##                                                         
    ## Constant           0.192***    0.204***      0.211***   
    ##                     (0.022)    (0.021)       (0.022)    
    ##                                                         
    ## --------------------------------------------------------
    ## Observations        40,920      40,920        40,920    
    ## R2                   0.024      0.028         0.029     
    ## Adjusted R2          0.024      0.028         0.029     
    ## ========================================================
    ## Note:                        *p<0.1; **p<0.05; ***p<0.01

``` r
#The warnings are due to the product have never had a certain type of ads in model 3.
#But what can we do when test dataset have a new type of ads?
```

Some ads types are strongly correlated with price, thus providing
imprecise estimates.

Visualization of model performance

``` r
library(ggplot2)

# Step 1: Get the levels of feature2 from the training data
feature2_coef_names <- names(coef(model3))[grepl("^feature2", names(coef(model3)))]
all_feature2_levels <- gsub("^feature2", "", feature2_coef_names) # Extract levels (e.g., "A", "D")
all_feature2_levels <- c("0", all_feature2_levels) # Add the base level "0" (assuming "0" is the reference)
print("Feature2 levels from model coefficients:")
```

    ## [1] "Feature2 levels from model coefficients:"

``` r
print(all_feature2_levels)
```

    ## [1] "0" "A" "C" "D" "F" "H" "J" "L"

``` r
# Step 2: Recode unseen levels in test_df as "Unknown" and ensure factor levels match
test_df <- test_df %>%
  mutate(
    feature2 = ifelse(feature2 %in% all_feature2_levels, as.character(feature2), "Unknown"),
    feature2 = factor(feature2, levels = c(all_feature2_levels, "Unknown"))
  )
print(unique(test_df$feature2))
```

    ## [1] 0 A J H
    ## Levels: 0 A C D F H J L Unknown

``` r
# Step 3: Compute average effect of feature2 coefficients from model3
feature2_coefs <- coef(model3)[grepl("^feature2", names(coef(model3)))]
avg_feature2_effect <- mean(feature2_coefs, na.rm = TRUE)

# Step 4: Predict for known levels, and manually adjust for "Unknown"
# First, predict for all rows where feature2 is a known level
test_df_known <- test_df %>%
  filter(feature2 %in% all_feature2_levels)
test_df_unknown <- test_df %>%
  filter(feature2 == "Unknown")


# Predict for known levels
if (nrow(test_df_known) > 0) {
  test_df_known <- test_df_known %>%
    mutate(pred_m3 = predict(model3, newdata = .))
} else {
  test_df_known <- test_df_known %>%
    mutate(pred_m3 = numeric(0))
}

# For "Unknown" levels, use the average effect
if (nrow(test_df_unknown) > 0) {
  # Create a temporary data frame with feature2 set to the base level
  temp_df <- test_df_unknown
  temp_df$feature2 <- factor(temp_df$feature2, levels = all_feature2_levels[0]) # Set to base level (e.g., "0")

  # Get terms contributions for known predictors using predict
  terms_contrib <- predict(model3, newdata = temp_df, type = "terms")
  terms_contrib <- as.data.frame(terms_contrib)

  # Manually compute the prediction using terms from base level plus avg_feature2_effect
  test_df_unknown <- test_df_unknown %>%
    mutate(pred_m3 = coef(model3)["(Intercept)"] +
                     terms_contrib$price +
                     terms_contrib$display2 +
                     terms_contrib$PRODUCT_ID +
                     avg_feature2_effect)
} else {
  test_df_unknown <- test_df_unknown %>%
    mutate(pred_m3 = numeric(0))
}

# Combine the results
test_df <- bind_rows(test_df_known, test_df_unknown)

# Predict for model1 and model2 as before
test_df <- test_df %>%
  mutate(
    pred_m1 = predict(model1, newdata = .),
    pred_m2 = predict(model2, newdata = .)
  )

# 4. Aggregate and plot
plot_data <- test_df %>%
  group_by(WEEK_NO) %>%
  summarise(
    actual = mean(TOTAL_SALES),
    m1 = mean(pred_m1, na.rm = TRUE),
    m2 = mean(pred_m2, na.rm = TRUE),
    m3 = mean(pred_m3, na.rm = TRUE)
  )

plot_long <- plot_data %>%
  pivot_longer(cols = -WEEK_NO, names_to = "series", values_to = "sales")

ggplot(plot_long, aes(x = WEEK_NO, y = sales, color = series)) +
  geom_line(linewidth = 0.5,) + 
scale_color_manual(
  values = c("actual" = "black", "m1" = "red", "m2" = "blue", "m3" = "green"),
  labels = c("actual" = "Actual", "m1" = "Model 1", "m2" = "Model 2", "m3" = "Model 3")) +
  scale_x_continuous(breaks = unique(plot_data$WEEK_NO), labels = as.integer(unique(plot_data$WEEK_NO))) +
  labs(
    title = "Predicted vs Actual Sales: Weeks 81–101",
    x = "Week Number",
    y = "Average Weekly Sales",
    color = "Series"
  ) +
  theme_minimal()
```

![](Different_Granularity_in_ads5_files/figure-gfm/unnamed-chunk-8-1.png)<!-- -->

``` r
# Step 1: Compute raw errors
error_df <- test_df %>%
  mutate(
    err_m1 = pred_m1 - TOTAL_SALES,
    err_m2 = pred_m2 - TOTAL_SALES,
    err_m3 = pred_m3 - TOTAL_SALES
  ) %>%
  group_by(WEEK_NO) %>%
  summarise(
    m1 = mean(err_m1, na.rm = TRUE),
    m2 = mean(err_m2, na.rm = TRUE),
    m3 = mean(err_m3, na.rm = TRUE)
  )

# Step 2: Convert to long format for plotting
error_long <- error_df %>%
  pivot_longer(cols = -WEEK_NO, names_to = "model", values_to = "error")

# Step 3: Plot
ggplot(error_long, aes(x = WEEK_NO, y = error, color = model)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray") +
  geom_line(linewidth = 0.6) +
  scale_color_manual(
    values = c("m1" = "red", "m2" = "blue", "m3" = "green"),
    labels = c("Model 1", "Model 2", "Model 3")
  ) +
  scale_x_continuous(breaks = unique(plot_data$WEEK_NO), labels = as.integer(unique(plot_data$WEEK_NO))) +
  labs(
    title = "Prediction Error by Week",
    x = "Week Number",
    y = "Prediction Error (Predicted - Actual)",
    color = "Model"
  ) +
  theme_minimal()
```

![](Different_Granularity_in_ads5_files/figure-gfm/unnamed-chunk-9-1.png)<!-- -->

Squared Errors

``` r
# Compute squared errors
squared_error_df <- test_df %>%
  mutate(
    sqerr_m1 = (pred_m1 - TOTAL_SALES)^2,
    sqerr_m2 = (pred_m2 - TOTAL_SALES)^2,
    sqerr_m3 = (pred_m3 - TOTAL_SALES)^2
  ) %>%
  group_by(WEEK_NO) %>%
  summarise(
    m1 = mean(sqerr_m1, na.rm = TRUE),
    m2 = mean(sqerr_m2, na.rm = TRUE),
    m3 = mean(sqerr_m3, na.rm = TRUE)
  )

# Total sum of squared error (SSE) for each model
sse_summary <- test_df %>%
  summarise(
    SSE_Model1 = sum((pred_m1 - TOTAL_SALES)^2, na.rm = TRUE),
    SSE_Model2 = sum((pred_m2 - TOTAL_SALES)^2, na.rm = TRUE),
    SSE_Model3 = sum((pred_m3 - TOTAL_SALES)^2, na.rm = TRUE)
  )

# Print SSE results to console
print("Total Sum of Squared Errors (SSE) by Model:")
```

    ## [1] "Total Sum of Squared Errors (SSE) by Model:"

``` r
print(sse_summary)
```

    ## # A tibble: 1 x 3
    ##   SSE_Model1 SSE_Model2 SSE_Model3
    ##        <dbl>      <dbl>      <dbl>
    ## 1     11118.     11017.     11073.

``` r
# Convert to long format for plotting
squared_error_long <- squared_error_df %>%
  pivot_longer(cols = -WEEK_NO, names_to = "model", values_to = "squared_error")

# Plot squared errors
ggplot(squared_error_long, aes(x = WEEK_NO, y = squared_error, color = model)) +
  geom_line(linewidth = 0.6) +
  scale_color_manual(
    values = c("m1" = "red", "m2" = "blue", "m3" = "green"),
    labels = c("Model 1", "Model 2", "Model 3")
  ) +
  scale_x_continuous(breaks = unique(plot_data$WEEK_NO), labels = as.integer(unique(plot_data$WEEK_NO))) +
  labs(
    title = "Mean Squared Error by Week",
    x = "Week Number",
    y = "Mean Squared Error",
    color = "Model"
  ) +
  theme_minimal()
```

![](Different_Granularity_in_ads5_files/figure-gfm/unnamed-chunk-10-1.png)<!-- -->
