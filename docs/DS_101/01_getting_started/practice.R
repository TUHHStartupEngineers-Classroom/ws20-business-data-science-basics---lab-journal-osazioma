# data science at NIT -----------------------------------------------------
# installation of packages

# CRAN Packages ----
pkgs_cran <- c(
  # File System
  "fs",         # working with the file system
  
  # Import
  "readxl",     # reading excel files
  "writexl",    # saving data as excel files
  
  # Tidy, Transform, & Visualize
  "tidyverse",  # dplyr, ggplot2, tibble, tidyr, readr, purrr, stringr, forcats
  "lubridate",  # working with dates and times
  
  # Other
  "devtools"    # used to install non-CRAN packages
)

install.packages("fs")       # Install single package
install.packages(pkgs_cran)  # Install many packages
library("tidyverse")
as_tibble()
tibble(
  x = 1:50,
  y = runif(50), 
  z = x + y^2,
  outcome = rnorm(50)
)
class(cars)
cars_tbl <- as_tibble(cars)
class(cars_tbl)
vehicles <- as_tibble(cars[1:5,])
vehicles[['speed']]
vehicles[[1]]
vehicles$speed
vehicles %>% .$dist
vehicles %>% .[['dist']]
vehicles %>% .[[2]]
dataset_tbl <- read_csv("data.csv")
readr::problems(dataset_tbl)
dataset_tbl <- read_csv("data.csv")
the_data <-
  read.csv('/path/to/data/file.csv') %>%
  subset(variable_a > x) %>%
  transform(variable_c = variable_a/variable_b) %>%
  head(100)
the_data <-
  read.csv('/path/to/data/file.csv') %>%
  subset(variable_a > x) %>%
  transform(variable_c = variable_a/variable_b) %>%
  head(100)
data <- iris
data <- head(data, n=3)
library("tidyverse")
iris %>% head(n=3)
read.csv('/path/to/data/file.csv') %>%
  subset(variable_a > x) %>%
  transform(variable_c = variable_a/variable_b) %>%
  head(100)
read.csv('/path/to/data/file.csv')
library(tidyverse)
diamonds2 <- readRDS("diamonds2.rds")
