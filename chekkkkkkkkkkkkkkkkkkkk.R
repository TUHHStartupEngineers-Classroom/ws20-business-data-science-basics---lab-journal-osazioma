#libraries
library(tidyverse)
library(ggplot2)
library(dplyr)
library(readxl)
library(readr)
library(lubridate)
library("writexl")
library(genius)
library(spotifyr)
library(plotly)
library(ggplot2)
library(rvest)
library(xopen)
library(jsonlite)
library(glue)
library(stringi)
library(furrr)
library(RSQLite)
library(httr)
library(keyring)
library(rstudioapi)
library(stringr)
library(purrr)
library(RColorBrewer)
library(data.table)
library(ggthemes)
library(tidyr)
library(scales)
library(forcats)
library(maps)
library(spdep)
library(ggmap)


url_home <- "https://www.rosebikes.com/bikes"
bikes_home <- url_home %>%
  read_html()

bike_family_tbl <- bikes_home %>%
  html_nodes(css = ".catalog-navigation__link")%>%
  html_attr('title') %>%
  discard(.p = ~stringr::str_detect(.x, "Sale"))%>%
  enframe(name = "position", value = "family_class") %>%
  mutate(family_id = str_glue("#{family_class}"))%>%
  mutate(url = glue("https://www.rosebikes.com/bikes/{family_class}"))

bike_family_tbl
