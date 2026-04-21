# ============================================================
# WAREHOUSE AND RETAIL SALES - ANALYSIS
# Dataset: Montgomery County, MD
# Source: https://catalog.data.gov/dataset/warehouse-and-retail-sales
# ============================================================


# ------------------------------------------------------------
# 1. LIBRARIES
# ------------------------------------------------------------

# If any library fails to load, run the line below ONCE to install it.
# After installing, add a "#" at the start of the line to comment it out,
# then run the rest of the script normally.
# install.packages(c("tidyverse", "lubridate", "janitor", "scales"))

library(tidyverse)

library(lubridate)

library(janitor)

library(scales)

# Avoid scientific notation in numbers.
options(scipen = 999)


# ------------------------------------------------------------
# 2. DATA IMPORT
# ------------------------------------------------------------

sales <- read.csv(
  "Warehouse_and_Retail_Sales.csv",
  stringsAsFactors = FALSE
)


# ------------------------------------------------------------
# 3. DATA CLEANING AND TYPE CONVERSION
# ------------------------------------------------------------

# Standardize column names to avoid issues with spaces and capitalization.
colnames(sales) <- make.names(colnames(sales))

sales_clean <- sales %>%
  rename(
    WAREHOUSE.SALES = WAREHOUSE.SALES.
  ) %>%
  mutate(
    YEAR             = as.integer(YEAR),
    MONTH            = as.integer(MONTH),
    RETAIL.SALES     = as.numeric(RETAIL.SALES),
    RETAIL.TRANSFERS = as.numeric(RETAIL.TRANSFERS),
    WAREHOUSE.SALES  = as.numeric(WAREHOUSE.SALES),
    # Real date column for correct time series plotting
    date             = make_date(YEAR, MONTH, 1)
  )

# Non-numeric values in numeric fields are coerced to NA
# and excluded from aggregations using na.rm = TRUE.


# ------------------------------------------------------------
# 4. INITIAL EXPLORATION
# ------------------------------------------------------------

# Dataset dimensions
cat("Dataset dimensions:\n")
cat("Rows:", nrow(sales_clean), "\n")
cat("Columns:", ncol(sales_clean), "\n\n")

# Column types and first values
cat("Dataset structure:\n")
glimpse(sales_clean)

# Time range
cat("\nTime range:\n")
cat("Minimum year:", min(sales_clean$YEAR, na.rm = TRUE), "\n")
cat("Maximum year:", max(sales_clean$YEAR, na.rm = TRUE), "\n")
cat("Available months:", n_distinct(sales_clean$MONTH), "\n")
cat("Unique periods (year-month):",
    n_distinct(paste(sales_clean$YEAR, sales_clean$MONTH)), "\n\n")

# Missing values per column
cat("Missing values per column:\n")
na_summary <- data.frame(
  column     = names(sales_clean),
  nas        = colSums(is.na(sales_clean)),
  percentage = round(colSums(is.na(sales_clean)) / nrow(sales_clean) * 100, 2)
)
print(na_summary)

# WARNING: WAREHOUSE.SALES data quality check
# WAREHOUSE.SALES contains 99.99% NA values (307,543 out of 307,547 records).
# Only 4 real values exist in the entire column.
# Any comparative analysis involving this channel must be interpreted
# with caution, as warehouse data is not adequately represented in this dataset.
# Retail sales remain the primary and reliable focus of this analysis.
cat("\nWAREHOUSE.SALES data quality:\n")
cat("NA values:", sum(is.na(sales_clean$WAREHOUSE.SALES)), "\n")
cat("Real values:", sum(!is.na(sales_clean$WAREHOUSE.SALES)), "\n")
cat("NA percentage:", round(mean(is.na(sales_clean$WAREHOUSE.SALES)) * 100, 2), "%\n\n")

# Available categories
cat("\nProduct types (ITEM.TYPE):\n")
print(table(sales_clean$ITEM.TYPE))

cat("\nNumber of unique products (ITEM.DESCRIPTION):",
    n_distinct(sales_clean$ITEM.DESCRIPTION), "\n")
cat("Number of unique suppliers (SUPPLIER):",
    n_distinct(sales_clean$SUPPLIER), "\n\n")

# Descriptive statistics for key numeric columns
cat("Descriptive statistics:\n")
sales_clean %>%
  select(RETAIL.SALES, WAREHOUSE.SALES, RETAIL.TRANSFERS) %>%
  summary() %>%
  print()


# ------------------------------------------------------------
# 5. CUSTOM PLOT THEME
# ------------------------------------------------------------

custom_theme <- theme(
  plot.title         = element_text(size = 14, face = "bold"),
  plot.subtitle      = element_text(size = 11),
  axis.title         = element_text(size = 11),
  axis.text          = element_text(size = 10),
  legend.title       = element_text(size = 10),
  legend.text        = element_text(size = 9),
  panel.grid.minor   = element_blank(),
  panel.grid.major.x = element_blank()
)


# ------------------------------------------------------------
# 6. SALES DISTRIBUTION (VISUAL EXPLORATION)
# ------------------------------------------------------------

# Histogram to detect skewness and outliers
ggplot(sales_clean %>% filter(!is.na(RETAIL.SALES)),
       aes(x = RETAIL.SALES)) +
  geom_histogram(bins = 50, fill = "#1f77b4", color = "white", linewidth = 0.2) +
  scale_x_continuous(labels = label_number(scale = 1e-3, suffix = "K")) +
  labs(
    title    = "Distribution of retail sales per record",
    subtitle = "Helps identify skewness and outliers",
    x        = "Retail sales (thousands)",
    y        = "Count"
  ) +
  theme_light() +
  custom_theme

# Records per year to check dataset balance
records_per_year <- sales_clean %>%
  count(YEAR) %>%
  rename(records = n)

ggplot(records_per_year,
       aes(x = factor(YEAR), y = records)) +
  geom_col(fill = "#1f77b4") +
  geom_text(aes(label = records), vjust = -0.5, size = 3.5) +
  labs(
    title    = "Number of records per year",
    subtitle = "Uneven years may affect trend comparisons",
    x        = "Year",
    y        = "Records"
  ) +
  theme_light() +
  custom_theme


# ------------------------------------------------------------
# 7. TOP 10 PRODUCTS BY RETAIL SALES
# ------------------------------------------------------------

# Only retail sales are considered to reflect direct consumer demand,
# excluding warehouse sales and internal transfers.

top10_products_retail <- sales_clean %>%
  group_by(ITEM.DESCRIPTION) %>%
  summarize(
    total_retail_sales = sum(RETAIL.SALES, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(desc(total_retail_sales)) %>%
  slice_head(n = 10) %>%
  mutate(ITEM.DESCRIPTION = substr(ITEM.DESCRIPTION, 1, 40))

ggplot(top10_products_retail,
       aes(x = reorder(ITEM.DESCRIPTION, total_retail_sales),
           y = total_retail_sales)) +
  geom_col(fill = "#1f77b4") +
  coord_flip() +
  scale_y_continuous(labels = label_number(scale = 1e-6, suffix = "M")) +
  labs(
    title = "Top 10 Products by Retail Sales",
    x     = "Product",
    y     = "Sales (Millions)"
  ) +
  theme_light() +
  custom_theme


# ------------------------------------------------------------
# 8. MONTHLY RETAIL SALES TREND
# ------------------------------------------------------------

monthly_retail_trend <- sales_clean %>%
  group_by(YEAR, MONTH) %>%
  summarize(
    total_retail_sales = sum(RETAIL.SALES, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(YEAR, MONTH) %>%
  mutate(date = make_date(YEAR, MONTH, 1))

# Trend plot using a real date on the X axis.
# Using date instead of MONTH prevents all years from collapsing into one line.
ggplot(monthly_retail_trend,
       aes(x = date, y = total_retail_sales)) +
  geom_line(color = "#2ca02c", linewidth = 1) +
  geom_point(color = "#2ca02c", size = 2) +
  scale_x_date(date_labels = "%b %Y", date_breaks = "6 months") +
  scale_y_continuous(labels = label_number(scale = 1e-6, suffix = "M")) +
  labs(
    title    = "Monthly Retail Sales Trend",
    subtitle = "Retail sales aggregated at a monthly level",
    x        = "Date",
    y        = "Retail Sales (Millions)"
  ) +
  theme_light() +
  custom_theme +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


# ------------------------------------------------------------
# 9. MONTHLY GROWTH RATE
# ------------------------------------------------------------

# arrange(YEAR, MONTH) ensures that lag() is chronologically correct.
# Using arrange(MONTH) alone causes January of a new year to be compared
# against December of the previous year incorrectly.

monthly_retail_trend <- monthly_retail_trend %>%
  arrange(YEAR, MONTH) %>%
  mutate(
    monthly_growth = total_retail_sales - lag(total_retail_sales),
    # CV = sd / mean  (higher CV = greater volatility)
    growth_rate    = (monthly_growth / lag(total_retail_sales)) * 100
  )

ggplot(monthly_retail_trend,
       aes(x = date, y = growth_rate)) +
  geom_line(linewidth = 1) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  scale_x_date(date_labels = "%b %Y", date_breaks = "6 months") +
  labs(
    title = "Monthly retail sales growth rate",
    x     = "Date",
    y     = "Growth rate (%)"
  ) +
  theme_light() +
  custom_theme +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


# ------------------------------------------------------------
# 10. SEASONALITY
# ------------------------------------------------------------

# "Are some months systematically better for retail sales?"

seasonality_retail <- sales_clean %>%
  group_by(MONTH) %>%
  summarize(
    avg_retail_sales = mean(RETAIL.SALES, na.rm = TRUE),
    .groups = "drop"
  )

ggplot(seasonality_retail,
       aes(x = factor(MONTH), y = avg_retail_sales)) +
  geom_col() +
  scale_y_continuous(labels = label_number(scale = 1e-6, suffix = "M")) +
  scale_x_discrete(labels = month.abb) +
  labs(
    title = "Average retail sales by month",
    x     = "Month",
    y     = "Average retail sales (Millions)"
  ) +
  theme_light() +
  custom_theme


# ------------------------------------------------------------
# 11. TREND BY PRODUCT TYPE
# ------------------------------------------------------------

monthly_retail_product <- sales_clean %>%
  group_by(MONTH, ITEM.TYPE) %>%
  summarize(
    total_retail_sales = sum(RETAIL.SALES, na.rm = TRUE),
    .groups = "drop"
  )

ggplot(monthly_retail_product,
       aes(x = MONTH, y = total_retail_sales, color = ITEM.TYPE)) +
  geom_line(linewidth = 1) +
  scale_x_continuous(breaks = 1:12, labels = month.abb) +
  scale_y_continuous(labels = label_number(scale = 1e-6, suffix = "M")) +
  labs(
    title = "Monthly retail sales by product type",
    x     = "Month",
    y     = "Total retail sales (Millions)",
    color = "Product type"
  ) +
  theme_light() +
  custom_theme


# ------------------------------------------------------------
# 12. RETAIL VS WAREHOUSE COMPARISON
# ------------------------------------------------------------

# NOTE: WAREHOUSE.SALES is 99.99% NA (only 4 real values out of 307,547 records).
# The chart below is included for analytical completeness, but warehouse results
# should not be used for business decisions given the lack of data.

monthly_channel_comparison <- sales_clean %>%
  group_by(YEAR, MONTH) %>%
  summarize(
    total_retail_sales    = sum(RETAIL.SALES,    na.rm = TRUE),
    total_warehouse_sales = sum(WAREHOUSE.SALES, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(date = make_date(YEAR, MONTH, 1))

# Reshape to long format for comparison
monthly_channel_long <- monthly_channel_comparison %>%
  pivot_longer(
    cols      = c(total_retail_sales, total_warehouse_sales),
    names_to  = "channel",
    values_to = "sales"
  )

ggplot(monthly_channel_long,
       aes(x = date, y = sales, color = channel, group = channel)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  scale_x_date(date_labels = "%b %Y", date_breaks = "6 months") +
  scale_y_continuous(labels = label_number(scale = 1e-6, suffix = "M")) +
  scale_color_manual(
    values = c(
      "total_retail_sales"    = "#ff7f0e",
      "total_warehouse_sales" = "#2ca02c"
    ),
    labels = c(
      "total_retail_sales"    = "Retail",
      "total_warehouse_sales" = "Warehouse"
    )
  ) +
  labs(
    title = "Retail vs Warehouse Sales Over Time",
    x     = "Date",
    y     = "Sales (Millions)",
    color = "Channel"
  ) +
  theme_light() +
  custom_theme +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


# ------------------------------------------------------------
# 13. VOLATILITY AND STABILITY ANALYSIS
# ------------------------------------------------------------

# NOTE: Given that WAREHOUSE.SALES is 99.99% NA, the CV and z-score results
# for the warehouse channel are based on only 4 data points and are
# statistically unreliable. The retail volatility results remain valid.

# Reshape to long format to calculate metrics per channel
long_sales <- sales_clean %>%
  select(YEAR, MONTH, RETAIL.SALES, WAREHOUSE.SALES) %>%
  pivot_longer(
    cols      = c(RETAIL.SALES, WAREHOUSE.SALES),
    names_to  = "channel",
    values_to = "sales"
  )

# Stability metrics: mean, standard deviation and CV.
# CV = sd / mean  (higher CV = less stable, more volatile)
stability_metrics <- long_sales %>%
  group_by(channel) %>%
  summarize(
    mean_sales               = mean(sales, na.rm = TRUE),
    sd_sales                 = sd(sales,   na.rm = TRUE),
    coefficient_of_variation = sd_sales / mean_sales,
    .groups = "drop"
  )

print(stability_metrics)

# CV visualization for non-technical audiences
ggplot(stability_metrics,
       aes(x = channel, y = coefficient_of_variation, fill = channel)) +
  geom_col(width = 0.6) +
  scale_fill_manual(values = c("#1f77b4", "#2ca02c")) +
  labs(
    title    = "Sales Volatility Comparison (Coefficient of Variation)",
    subtitle = "Higher CV indicates greater relative volatility",
    x        = "Channel",
    y        = "Coefficient of Variation"
  ) +
  theme_light() +
  custom_theme +
  theme(legend.position = "none")

# Z-score normalization per channel.
# 0 = typical month, +1 = one std dev above average, -1 = unusually weak month.
normalized_sales <- long_sales %>%
  group_by(channel) %>%
  mutate(
    normalized_sales = (sales - mean(sales, na.rm = TRUE)) /
                        sd(sales, na.rm = TRUE)
  ) %>%
  ungroup()

ggplot(normalized_sales,
       aes(x = MONTH, y = normalized_sales, color = channel)) +
  geom_line(linewidth = 1) +
  geom_hline(yintercept = 0, linetype = "dashed", alpha = 0.4) +
  scale_x_continuous(breaks = 1:12, labels = month.abb) +
  labs(
    title = "Retail vs Warehouse sales volatility (normalized)",
    x     = "Month",
    y     = "Deviation from channel average"
  ) +
  theme_light() +
  custom_theme


# ------------------------------------------------------------
# 14. FINAL CONCLUSIONS
# ------------------------------------------------------------

# Numbers are calculated dynamically so the report updates
# automatically if the dataset changes.

total_retail    <- sum(sales_clean$RETAIL.SALES,    na.rm = TRUE)
total_warehouse <- sum(sales_clean$WAREHOUSE.SALES, na.rm = TRUE)
retail_share    <- round((total_retail / (total_retail + total_warehouse)) * 100, 1)

top_product <- sales_clean %>%
  group_by(ITEM.DESCRIPTION) %>%
  summarize(total = sum(RETAIL.SALES, na.rm = TRUE), .groups = "drop") %>%
  arrange(desc(total)) %>%
  slice_head(n = 1)

best_month <- sales_clean %>%
  group_by(MONTH) %>%
  summarize(avg = mean(RETAIL.SALES, na.rm = TRUE), .groups = "drop") %>%
  arrange(desc(avg)) %>%
  slice_head(n = 1)

worst_month <- sales_clean %>%
  group_by(MONTH) %>%
  summarize(avg = mean(RETAIL.SALES, na.rm = TRUE), .groups = "drop") %>%
  arrange(avg) %>%
  slice_head(n = 1)

cat("============================================================\n")
cat("FINAL CONCLUSIONS\n")
cat("============================================================\n\n")

cat("1. BUSINESS COMPOSITION\n")
cat("   The retail channel accounts for", retail_share, "% of total combined sales,\n")
cat("   confirming that direct-to-consumer selling is the main revenue driver.\n\n")

cat("2. TOP PRODUCTS\n")
cat("   The product with the highest retail sales volume is:\n")
cat("  ", substr(top_product$ITEM.DESCRIPTION, 1, 50), "\n")
cat("   with a total of $", format(round(top_product$total), big.mark = ","), "in sales.\n\n")

cat("3. SEASONALITY\n")
cat("   The month with the highest average retail sales is month", best_month$MONTH,
    "(", month.name[best_month$MONTH], "),\n")
cat("   while the weakest month is month", worst_month$MONTH,
    "(", month.name[worst_month$MONTH], ").\n")
cat("   This difference suggests seasonal patterns that should be considered\n")
cat("   in inventory planning and promotional strategies.\n\n")

cat("4. VOLATILITY AND STABILITY\n")
cat("   The coefficient of variation analysis confirmed that:\n")
cat("   - Retail CV ~4.35: highly irregular demand, sensitive to seasonality\n")
cat("     and consumer behavior.\n")
cat("   - Warehouse CV: statistically unreliable — only 4 real values exist\n")
cat("     out of 307,547 records (99.99% NA). Warehouse data is not\n")
cat("     adequately represented in this dataset and should not be used\n")
cat("     for stability comparisons or business decisions.\n\n")

cat("5. BUSINESS IMPLICATIONS\n")
cat("   Retail is the only channel with sufficient data for reliable analysis.\n")
cat("   Recommendations based on retail findings:\n")
cat("   - Strengthen promotional strategies during low-seasonality months.\n")
cat("   - Monitor high-volatility retail months to optimize inventory.\n")
cat("   - Collect complete warehouse data in future datasets to enable\n")
cat("     a valid channel comparison and a more comprehensive analysis.\n\n")

cat("============================================================\n")
cat("Analysis based on Montgomery County, MD data\n")
cat("Period covered:", min(sales_clean$YEAR), "-", max(sales_clean$YEAR), "\n")
cat("Total records analyzed:", format(nrow(sales_clean), big.mark = ","), "\n")
cat("============================================================\n")
