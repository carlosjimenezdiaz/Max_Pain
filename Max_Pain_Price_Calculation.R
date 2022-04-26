# Starting the Timer
tictoc::tic()

# Global Variables
options(scipen = 999)
options(dplyr.summarise.inform = FALSE)

# Getting the functions
source(file = "00_scripts/Libraries.R")

# Loading the functions
libraries()

# Local Variavles
Ticker              <- "^SPX" # Defining the ticker
Ticker_Multi        <- 100   # Multiplier of the Option Contract
environment_r       <- "EU"  # Where is your computer located? EU or US
Date_MP_Calculation <- "2022-04-29" # Expiration that you want to analyze
Ticker_Label        <- str_replace_all(Ticker, "[^[:alnum:]]", " ") %>% str_replace_all(.,"[ ]+", "")

# Getting future option chain
db_option_chain <- getOptionChain(Ticker, src = "yahoo", Exp = str_glue("{lubridate::year(Sys.Date())}/{lubridate::year(Sys.Date()) + 1}")) %>%
  unlist(recursive = FALSE) %>%
  enframe() %>%
  unnest(cols = c(value))

# Addaping the structure of the option chain to US or EU machines
if(environment_r == "US"){
  # Fixing some formats
  db_option_chain <- db_option_chain %>%
    dplyr::mutate(data_month = substr(name, 1, 3),  # Extracting just the portion related to the month
                  data_day   = substr(name, 5, 6),  # Extracting just the portion related to the day
                  data_year  = substr(name, 8, 11), # Extracting just the portion related to the year
                  type       = substr(name, 13, length(name)),
                  data_month = case_when(data_month == "Jan" ~ "01",
                                         data_month == "Feb" ~ "02",
                                         data_month == "Mar" ~ "03",
                                         data_month == "Apr" ~ "04",
                                         data_month == "May" ~ "05",
                                         data_month == "Jun" ~ "06",
                                         data_month == "Jul" ~ "07",
                                         data_month == "Aug" ~ "08",
                                         data_month == "Sep" ~ "09",
                                         data_month == "Oct" ~ "10",
                                         data_month == "Nov" ~ "11",
                                         data_month == "Dec" ~ "12",
                                         TRUE ~ data_month),
                  expiration = as.Date(str_glue("{data_year}-{data_month}-{data_day}"), format = "%Y-%m-%d")) %>%
    dplyr::select(expiration, type, Strike, OI) %>% 
    replace(is.na(.), 0)
}else{
  # Fixing some formats
  db_option_chain <- db_option_chain %>%
    dplyr::mutate(data_month = substr(name, 1, 3),  # Extracting just the portion related to the month
                  data_day   = substr(name, 6, 7),  # Extracting just the portion related to the day
                  data_year  = substr(name, 9, 12), # Extracting just the portion related to the year
                  type       = substr(name, 14, length(name)),
                  data_month = case_when(data_month == "ene" ~ "01",
                                         data_month == "feb" ~ "02",
                                         data_month == "mar" ~ "03",
                                         data_month == "abr" ~ "04",
                                         data_month == "may" ~ "05",
                                         data_month == "jun" ~ "06",
                                         data_month == "jul" ~ "07",
                                         data_month == "ago" ~ "08",
                                         data_month == "sep" ~ "09",
                                         data_month == "oct" ~ "10",
                                         data_month == "nov" ~ "11",
                                         data_month == "dic" ~ "12",
                                         TRUE ~ data_month),
                  expiration = as.Date(str_glue("{data_year}-{data_month}-{data_day}"), format = "%Y-%m-%d")) %>%
    dplyr::select(expiration, type, Strike, OI) %>%  
    replace(is.na(.), 0)
}

################################################
# Calculating the Strike where the Max Pain is #
################################################

# Option Interest distribution
p <- db_option_chain %>%
  dplyr::filter(expiration == Date_MP_Calculation) %>%
  ggplot(aes(x = Strike, y = OI)) +
  geom_bar(stat = "identity") +
  scale_y_continuous(labels = scales::dollar_format(prefix = "")) +
  labs(title    = str_glue("Option interest on all strikes ({Date_MP_Calculation} expiration date)."),
       subtitle = str_glue("Analysis performed on {Ticker_Label}."),
       caption  = "By: Carlos Jimenez",
       x = "Strike",
       y = "Open Interest") +
  theme(legend.title = element_blank()) +
  facet_free(type ~ .)

p

ggsave("OI_Distribution_Option_Type.png", plot = p, device = "png", path = "Plots/", width = 25, height = 15, units = "cm")

p <- db_option_chain %>%
  dplyr::filter(expiration == Date_MP_Calculation) %>%
  ggplot(aes(x = Strike, y = OI, colour = type)) +
  geom_line() +
  scale_y_continuous(labels = scales::dollar_format(prefix = "")) +
  labs(title    = str_glue("Option interest on all strikes ({Date_MP_Calculation} expiration date)."),
       subtitle = str_glue("Analysis performed on {Ticker_Label}."),
       caption  = "By: Carlos Jimenez",
       x = "Strike",
       y = "Open Interest") +
  theme(legend.title = element_blank())

p

ggsave("OI_Distribution_Option_Type_V2.png", plot = p, device = "png", path = "Plots/", width = 25, height = 15, units = "cm")

# Analyzing the Calls
db_Call_Cash <- NULL
db_Calls_MP  <- db_option_chain %>%
  dplyr::filter(expiration == Date_MP_Calculation) %>% 
  dplyr::filter(type == "calls")

for(closing_price in db_Calls_MP$Strike){ # closing_price <- 195
  
  # Intrinsic Value calculation (because at expiration extrinsic value is ZERO)
  IV_Calculation <- db_Calls_MP %>%
    dplyr::transmute(status          = ifelse(closing_price >= Strike, "ITM", "OTM"),
                     Intrinsic_Value = case_when(status == "ITM" ~ closing_price - Strike, TRUE ~ 0),
                     Cash_Value      = Intrinsic_Value*OI*Ticker_Multi)
  
  # Collecting the results
  if(is.null(db_Call_Cash)){
    db_Call_Cash <- data.frame(Price      = closing_price,
                               Cash_Value = sum(IV_Calculation$Cash_Value))
  }else{
    db_Call_Cash <- db_Call_Cash %>%
      bind_rows(data.frame(Price      = closing_price,
                           Cash_Value = sum(IV_Calculation$Cash_Value)))
  }
}

# Analyzing the Puts
db_Put_Cash <- NULL
db_Puts_MP  <- db_option_chain %>%
  dplyr::filter(expiration == Date_MP_Calculation) %>% 
  dplyr::filter(type == "puts")

for(closing_price in db_Puts_MP$Strike){ # closing_price <- 175
  
  # Intrinsic Value calculation (because at expiration extrinsic value is ZERO)
  IV_Calculation <- data.frame(Strike = db_Puts_MP$Strike) %>%
    dplyr::transmute(status          = ifelse(closing_price <= Strike, "ITM", "OTM"),
                     Intrinsic_Value = case_when(status == "ITM" ~ Strike - closing_price, TRUE ~ 0),
                     Cash_Value      = Intrinsic_Value*db_Puts_MP$OI*Ticker_Multi)
  
  # Collecting the results
  if(is.null(db_Put_Cash)){
    db_Put_Cash <- data.frame(Price      = closing_price,
                              Cash_Value = sum(IV_Calculation$Cash_Value))
  }else{
    db_Put_Cash <- db_Put_Cash %>%
      bind_rows(data.frame(Price      = closing_price,
                           Cash_Value = sum(IV_Calculation$Cash_Value)))
  }
}

# Combining all the information
db_Max_Pain <- db_Call_Cash %>%
  dplyr::mutate(Option_Type = "Call") %>%
  bind_rows(db_Put_Cash %>%
              dplyr::mutate(Option_Type = "Put")) %>%
  dplyr::group_by(Price) %>%
  dplyr::summarise(Total_Cash_Value = sum(Cash_Value)) %>%
  dplyr::mutate(Total_Cash_Value = (lowess(Price, Total_Cash_Value, f = 1/10) %>% pluck(2))) # Removing the Noice

Price_Max_Pain <- db_Max_Pain %>%
  dplyr::filter(Total_Cash_Value == min(Total_Cash_Value)) %>%
  dplyr::select(Price) %>%
  pull()

# Generating the Chart
p <- db_Max_Pain %>%
  dplyr::filter(Price >= Price_Max_Pain*0.5 & Price_Max_Pain <= Price_Max_Pain*1.5) %>%
  ggplot(aes(x = Price, y = Total_Cash_Value)) +
  geom_line() +
  scale_y_continuous(labels = scales::dollar_format()) +
  labs(title    = str_glue("Max Pain Calculations - Expected Pin Price at expiration: {Price_Max_Pain}"),
       subtitle = str_glue("Analysis performed on {Ticker_Label} - Expiration Date: {Date_MP_Calculation}"),
       caption  = "By: Carlos Jimenez",
       x = "Underlying Price",
       y = "Cash Value of Options") +
  theme(legend.title = element_blank()) + 
  geom_vline(xintercept = Price_Max_Pain, 
             linetype   = "dotted", 
             color      = "blue",  
             size       = 1.5)

p

ggsave("Pain_Price_Calculation.png", plot = p, device = "png", path = "Plots/", width = 25, height = 15, units = "cm")

# Stoping the Timer
tictoc::toc()
