libraries <- function(){
  suppressPackageStartupMessages({
    if (!require("tidyquant")) install.packages("tidyquant"); library(tidyquant)
    if (!require("tidyverse")) install.packages("tidyverse"); library(tidyverse)
    if (!require("derivmkts")) install.packages("derivmkts"); library(derivmkts)
    if (!require("scales")) install.packages("scales"); library(scales)
    if (!require("broom")) install.packages("broom"); library(broom)
    if (!require("tictoc")) install.packages("tictoc"); library(tictoc)
    if (!require("viridis")) install.packages("viridis"); library(viridis)
    if (!require("RMySQL")) install.packages("RMySQL"); library(RMySQL)
    if (!require("DBI")) install.packages("DBI"); library(DBI)
    if (!require("zoo")) install.packages("zoo"); library(zoo)
    if (!require("greeks")) install.packages("greeks"); library(greeks)
    if (!require("Quandl")) install.packages("Quandl"); library(Quandl)
  })
}

libraries()

# Folder Creation ----
if(dir.exists("00_scripts/")){
  dump(list = c("libraries"), file = "00_scripts/Libraries.R", append = FALSE,
       control = "all", envir = parent.frame(), evaluate = TRUE)
}else{
  fs::dir_create("00_scripts/") 
  dump(list = c("libraries"), file = "00_scripts/Libraries.R", append = FALSE)
}