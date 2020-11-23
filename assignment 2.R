#Run libraries

library(tidyverse)
library(ggplot2)
library(dplyr)
library(readxl)
library(readr)
library(lubridate)
library("writexl")
#Importing the files

bikes_tbl <- read_excel(path = "DS_101/00_data/01_bike_sales/01_raw_data/bikes.xlsx")
orderlines_tbl <- read_excel(path = "DS_101/00_data/01_bike_sales/01_raw_data/orderlines.xlsx")
bikeshops_tbl <- read_excel(path = "DS_101/00_data/01_bike_sales/01_raw_data/bikeshops.xlsx")
#run data wrangling 
glimpse(bike_orderlines_joined_tbl)
bike_orderlines_joined_tbl <- orderlines_tbl %>%
  left_join(bikes_tbl, by = c("product.id" = "bike.id")) %>%
  left_join(bikeshops_tbl, by = c("customer.id" = "bikeshop.id"))

bike_orderlines_joined_tbl %>% 
  select(category) %>%
  filter(str_detect(category, "^road")) %>% 
  unique()

bike_orderlines_wrangled_tbl <- bike_orderlines_joined_tbl %>%
  separate(col = category,
           into = c("category.1", "category.2", "category.3"),
           sep = " - ")%>%
  mutate(total.price = price * quantity) %>%
  select(-...1, -gender) %>%
  select(-ends_with(".id")) %>%
  bind_cols(bike_orderlines_joined_tbl %>% select(order.id)) %>%
  select(order.id, contains("order"), contains("model"), contains("category"),
         price, quantity, total.price,
         everything()) %>%
  rename(bikeshop = name) %>%
  set_names(names(.) %>% str_replace_all("\\.", "_"))

bike_orderlines_wrangled_2_tbl <- bike_orderlines_wrangled_tbl %>%
  separate(col = location,
           into = c("city", "state"),
           sep = ", ")%>%
  select(order_id, contains("order"), contains("model"), contains("category"),
         price, quantity, total_price,
         everything())

#first solution for state 

sales_by_state_tbl <- bike_orderlines_wrangled_2_tbl %>%
  select(state, total_price) %>%
  group_by(state) %>%
  summarize(sales = sum(total_price)) %>%
  mutate(sales_text = scales::dollar(sales, big.mark = ".",
                                     decimal.mark = ",",
                                     prefix = "",
                                     suffix = " Є"))
sales_by_state_tbl %>%
  ggplot(aes(x=state, y=sales))+
  geom_col(fill = "#FFDB6D")+
  geom_label(aes(label=sales_text))+
  geom_smooth(method="lm", se=FALSE)+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  scale_y_continuous(labels = scales::dollar_format(big.mark = ".",
                                                    decimal.mark = ",",
                                                    prefix = "",
                                                    suffix = " Є"))+
  labs(
    title = "Revenue by state",
    subtitle = "Upward Trend",
    x = "",
    y = "Revenue"
  )
#second solution price 
glimpse(bike_orderlines_wrangled_2_tbl)

sales_by_state_year_tbl <- bike_orderlines_wrangled_2_tbl %>%
  select(order_date, total_price, state) %>%
  mutate(year = year(order_date)) %>%
  group_by(year, state) %>%
  summarize(sales = sum(total_price)) %>%
  #theme(axis.text.x = element_text(angle = 45, hjust = 1))%>%
  ungroup()%>%
  mutate(sales_text = scales::dollar(sales, big.mark = ".",
                                     decimal.mark = ",",
                                     prefix = "",
                                     suffix = " Є"))

sales_by_state_year_tbl %>%
  ggplot(aes(x=year, y=sales, fill = state))+
  geom_col()+
  facet_wrap(~ state)+
  scale_y_continuous(labels = scales::dollar_format(big.mark = ".",
                                                    decimal.mark = ",",
                                                    prefix = "",
                                                    suffix = " Є"))+
  labs(
    title = "Revenue by Year and  the State",
    subtitle = "upward trend",
    fill = "States"
  )
#save the files
bike_orderlines_wrangled_tbl %>%
  write_xlsx("DS_101/00_data/01_bike_sales/02_wrangled_data/bike_orderlines.xlsx")
bike_orderlines_wrangled_tbl %>%
  write_xlsx("DS_101/00_data/01_bike_sales/02_wrangled_data/bike_orderlines.csv")
bike_orderlines_wrangled_tbl %>%
  write_xlsx("DS_101/00_data/01_bike_sales/02_wrangled_data/bike_orderlines.rds")
