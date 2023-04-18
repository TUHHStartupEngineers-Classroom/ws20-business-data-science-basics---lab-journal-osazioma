library(tidyverse)
library(readr)
diamonds2 <- readRDS("diamonds2.rds")
diamonds2 %>% head(n = 5)
diamonds2 %>% 
  pivot_longer(cols      = c("2008", "2009"), 
               names_to  = 'year', 
               values_to = 'price') %>% 
  head(n = 5)

model <- lm(price ~ ., data = diamonds2_long)
model <- lm(price ~ ., data = diamonds2_long)
model
#⁠#⁠ Call:
lm(formula = price ~ ., data = diamonds2_long)
diamonds3 <- readRDS("diamonds3.rds")
diamonds3 %>% head(n = 5)  
diamonds3 %>% 
  pivot_wider(names_from  = "dimension",
              values_from = "measurement") %>% 
  head(n = 5)
diamonds4 <- readRDS("diamonds4.rds")
diamonds4
diamonds4 %>% 
  separate(col = dim,
           into = c("x", "y", "z"),
           sep = "/",
           convert = T)
diamonds5
diamonds5 %>% 
  unite(clarity, clarity_prefix, clarity_suffix, sep = '')
library(ggplot2)
library(dplyr)
diamonds %>% 
  filter(cut == 'Ideal' | cut == 'Premium', carat >= 0.23) %>% 
  head(5)
diamonds %>% 
  filter(cut == 'Ideal' | cut == 'Premium', carat >= 0.23) %>% 
  slice(3:4)
diamonds %>% 
  arrange(cut, carat, desc(price))
diamonds %>% 
  select(color, clarity, x:z) %>% 
  head(n = 5)
diamonds %>% 
  select(-(x:z)) %>% 
  head(n = 5)
diamonds %>% 
  mutate(p = x + z, q = p + y) %>% 
  select(-(depth:price)) %>% 
  head(n = 5)
diamonds %>% 
  transmute(carat, cut, sum = x + y + z) %>% 
  head(n = 5)
diamonds %>% 
  group_by(cut) %>% 
  summarize(max_price  = max(price),
            mean_price = mean(price),
            min_price  = min(price))
glimpse(diamonds)
## Rows: 53,940
## Columns: 10
glimpse(diamonds)
library(lubridate)
ymd(20101215)
mdy("4/1/17")
bday <- dmy("14/10/1979")
#1.0 Load libraries ----
#Load libraries ---- 

library(tidyverse)

#load data
bikes_tbl      <- read_excel(path = "00_data/01_bike_sales/01_raw_data/bikes.xlsx")
orderlines_tbl <- read_excel("00_data/01_bike_sales/01_raw_data/orderlines.xlsx")
bikeshops_tbl  <- read_excel("00_data/01_bike_sales/01_raw_data/bikeshops.xlsx")
orderlines_tbl
glimpse(orderlines_tbl)
left_join(orderlines_tbl, bikes_tbl)
left_join(orderlines_tbl, bikes_tbl, by = c("product.id" = "bike.id"))
bike_orderlines_joined_tbl <- orderlines_tbl %>%
  left_join(bikes_tbl, by = c("product.id" = "bike.id")) %>%
  left_join(bikeshops_tbl, by = c("customer.id" = "bikeshop.id"))
bike_orderlines_joined_tbl %>% glimpse()
bike_orderlines_joined_tbl %>% 
  select(category) %>%
  filter(str_detect(category, "^Mountain")) %>% 
  unique()
bike_orderlines_wrangled_tbl <- bike_orderlines_joined_tbl %>% 
  separate(col    = category,
           into   = c("category.1", "category.2", "category.3"),
           sep    = " - ") %>%
mutate(total.price = price * quantity) %>%
select(-...1, -gender) %>% 
select(-ends_with(".id")) %>%
bind_cols(bike_orderlines_joined_tbl %>% select(order.id)) %>%
  select(order.id, contains("order"), contains("model"), contains("category"),
         price, quantity, total.price,
         everything()) %>%
  rename(bikeshop = name) %>%
  set_names(names(.) %>% str_replace_all("\\.", "_"))  
library(lubridate)
sales_by_year_tbl <- bike_orderlines_wrangled_tbl %>% 
  select(order_date, total_price) %>%
  mutate(year = year(order_date)) %>%
  group_by(year) %>% 
  summarize(sales = sum(total_price)) %>%
  mutate(sales_text = scales::dollar(sales, big.mark = ".", 
                                     decimal.mark = ",", 
                                     prefix = "", 
                                     suffix = " €"))
sales_by_year_tbl

#graph 1
sales_by_year_tbl %>%
  ggplot(aes(x = year, y = sales)) +
  geom_col(fill = "#2DC6D6") +
  geom_label(aes(label = sales_text)) +
  geom_smooth(method = "lm", se = FALSE) +
  scale_y_continuous(labels = scales::dollar_format(big.mark = ".", 
                                                    decimal.mark = ",", 
                                                    prefix = "", 
                                                    suffix = " €")) +
  labs(
    title    = "Revenue by year",
    subtitle = "Upward Trend",
    x = "", # Override defaults for x and y
    y = "Revenue"
  )
#graph 2
sales_by_year_cat_1_tbl <- bike_orderlines_wrangled_tbl %>%
  select(order_date, total_price, category_1) %>%
  mutate(year = year(order_date)) %>%
  group_by(year, category_1) %>%
  summarise(sales = sum(total_price)) %>%
  ungroup() %>%
  mutate(sales_text = scales::dollar(sales, big.mark = ".", 
                                     decimal.mark = ",", 
                                     prefix = "", 
                                     suffix = " €"))
sales_by_year_cat_1_tbl
ggplot(aes(x = year, y = sales, fill = category_1)) +
  geom_col() +
  facet_wrap(~ category_1) +
  scale_y_continuous(labels = scales::dollar_format(big.mark = ".", 
                                                    decimal.mark = ",", 
                                                    prefix = "", 
                                                    suffix = " €")) +
  labs(
    title = "Revenue by year and main category",
    subtitle = "Each product category has an upward trend",
    fill = "Main category" # Changes the legend name
  )

# Step 2 - Visualize
sales_by_year_cat_1_tbl %>%
  
  # Set up x, y, fill
  ggplot(aes(x = year, y = sales, fill = category_1)) +
  
  # Geometries
  geom_col() + # Run up to here to get a stacked bar plot
  
  # Facet
  facet_wrap(~ category_1) +
  
  # Formatting
  scale_y_continuous(labels = scales::dollar_format(big.mark = ".", 
                                                    decimal.mark = ",", 
                                                    prefix = "", 
                                                    suffix = " €")) +
  labs(
    title = "Revenue by year and main category",
    subtitle = "Each product category has an upward trend",
    fill = "Main category" # Changes the legend name
  )


