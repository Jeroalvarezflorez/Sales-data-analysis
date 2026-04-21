# Sales-data-analysis

# Analysis of Warehouse and Retail Sales

R analysis, SQL queries, and a Power BI dashboard are used in an exploratory data study of Montgomery County, Maryland warehouse and retail sales data.

## Dataset, The source is [data.gov].(https://catalog.data.gov/dataset/warehouse-and-retail-sales)307,547 records
Time Frame: 2017–2020 

## Structure of the Repository:

Sales-data-analysis/
│
├── README.md
│
├── R/
│   ├── warehouse_retail_analysis.R
│   └── plots/
│       ├── 01_top10_products.png
│       ├── 02_monthly_trend.png
│       ├── 03_growth_rate.png
│       ├── 04_seasonality.png
│       ├── 05_sales_by_product_type.png
│       ├── 06_retail_vs_warehouse.png
│       ├── 07_volatility_cv.png
│       └── 08_normalized_sales.png
│
└── SQL/
└── warehouse_retail_queries.sql

## R Analysis
- Cleaning and converting data types
- The top ten items in terms of retail sales
- The trend of monthly retail sales
- Analysis of seasonality
- A comparison of the retail and warehouse channels, Coefficient of Variation and Z-score normalization are used for volatility analysis

## SQL Analysis
- Basic queries: SELECT, WHERE, ORDER BY
- Aggregations: GROUP BY, HAVING
- Table joins: INNER JOIN, LEFT JOIN

## Power BI Dashboard
- Retail sales trend over time
- Top 10 products by retail sales
- Average retail sales by month
- Sales distribution by product type
- Top 10 suppliers by retail sales

## Tools
- R — tidyverse, lubridate, janitor, scales
- SQL — SQLite via DBeaver
- Power BI Desktop

## Key Finding
WAREHOUSE_SALES contains 99.99% NA values (307,543 out of 307,547 records),
making retail the only reliable channel for analysis in this dataset.
This limitation is documented throughout the analysis
