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
Date_MP_Calculation <- "2022-05-13" # Expiration that you want to analyze
Ticker_Label        <- str_replace_all(Ticker, "[^[:alnum:]]", " ") %>% str_replace_all(.,"[ ]+", "")

# Getting Current Price
Current_Price <- getQuote(Ticker)$Last

# Getting future option chain
db_option_chain <- getOptionChain(Ticker, src = "yahoo", Exp = str_glue("{lubridate::year(Sys.Date())}/{lubridate::year(Sys.Date()) + 1}")) %>%
  unlist(recursive = FALSE) %>%
  enframe() %>%
  unnest(cols = c(value))

  # Extracting the type of Option
  opt_type <- c()
  for(i in 1:nrow(db_option_chain)){ # i <- 1
    
    # Selecting the name column
    name_tag <- db_option_chain$name[i]
    
    # Getting the length of that name
    name_length <- nchar(name_tag)
    
    # Getting the option type
    opt_type <- c(opt_type, substr(name_tag, 14, name_length))
  }
  
# Adding the Option Type
  db_option_chain <- db_option_chain %>%
  dplyr::mutate(type = opt_type)
  
################################################
# Calculating the Strike where the Max Pain is #
################################################

# Option Interest distribution
p <- db_option_chain %>%
  dplyr::filter((Expiration %>% as.Date()) == (Date_MP_Calculation %>% as.Date())) %>%
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
  dplyr::filter((Expiration %>% as.Date()) == (Date_MP_Calculation %>% as.Date())) %>%
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
  dplyr::filter((Expiration %>% as.Date()) == (Date_MP_Calculation %>% as.Date())) %>%
  dplyr::filter(type == "calls")

for(closing_price in db_Calls_MP$Strike){ # closing_price <- 3900
  
  # Intrinsic Value calculation (because at expiration extrinsic value is ZERO)
  IV_Calculation <- db_Calls_MP %>%
    dplyr::transmute(status          = ifelse(closing_price >= Strike, "ITM", "OTM"),
                     Intrinsic_Value = case_when(status == "ITM" ~ closing_price - Strike, TRUE ~ 0),
                     Cash_Value      = Intrinsic_Value*OI*Ticker_Multi) %>%
    replace(is.na(.), 0)
  
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
  dplyr::filter((Expiration %>% as.Date()) == (Date_MP_Calculation %>% as.Date())) %>%
  dplyr::filter(type == "puts")

for(closing_price in db_Puts_MP$Strike){ # closing_price <- 175
  
  # Intrinsic Value calculation (because at expiration extrinsic value is ZERO)
  IV_Calculation <- db_Puts_MP %>%
    dplyr::transmute(status          = ifelse(closing_price <= Strike, "ITM", "OTM"),
                     Intrinsic_Value = case_when(status == "ITM" ~ Strike - closing_price, TRUE ~ 0),
                     Cash_Value      = Intrinsic_Value*OI*Ticker_Multi) %>%
    replace(is.na(.), 0)
  
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
  labs(title    = str_glue("Max Pain Calculations - Expected Pin Price at expiration: {Price_Max_Pain}. Currently Diff {round((Price_Max_Pain/Current_Price - 1)*100, digits = 2)}%."),
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
