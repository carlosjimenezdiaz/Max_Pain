# Starting the Timer
tictoc::tic()

# Global Variables
options(scipen = 999)
options(dplyr.summarise.inform = FALSE)

# Getting the functions
source(file = "00_scripts/Option_Chain_SQL.R")
source(file = "00_scripts/Libraries.R")

# Loading the functions
libraries()

# Getting historical Option Chain from our DB (and filtering for the SPY)
if(file.exists("db_credentials/mysql_financial_data.yml")){
  db_Option_Chain <- Option_Chain_SQL("db_credentials/mysql_financial_data.yml") %>%
    dplyr::filter(Symbol == "SPY")  
}else{
  db_Option_Chain <- readRDS("db_provisional/db_subset_option_chain_SPY.rds")
}

# Getting the expiration dates
Expiration_Dates <- db_Option_Chain$final_date %>% unique() %>%  as.Date()

# Subsetting all our DB just to show SPY AT EXPIRATION
db_Option_Chain_SS <- db_Option_Chain %>%
  dplyr::mutate(Date_Process = as.Date(Time_Process)) %>%
  left_join(tq_get("SPY",
                   from = (Expiration_Dates %>% min()) - 1, # First day in our data base
                   to   = Sys.Date() + 1,
                   get  = "stock.prices",
                   complete_cases = TRUE) %>%
              dplyr::select(date, adjusted), by = c("Date_Process" = "date")) %>%
  dplyr::filter(Date_Process == final_date)

# First Analysis: manually input one date from the Expiration_Dates variable

  # Let see the dates
  Expiration_Dates
  
  # Selecting the date
  date_analysis <- "2021-11-15"
  
  # Subsetting the DB
  chart_data <- db_Option_Chain_SS %>%
    dplyr::filter(final_date == date_analysis)
  
  # Selecting the adjusted close of the underlying asset that had at that expiration date
  Closing_Price_At_Expiration <- chart_data$adjusted[1]
  
  # Creating the chart
  chart_data %>%
    ggplot(aes(x = Strike, y = OI, fill = final_date)) +
    geom_bar(stat = "identity") +
    facet_free(. ~ type) + 
    geom_vline(xintercept = Closing_Price_At_Expiration, linetype = "dotted", color = "black", size=0.8) +
    scale_y_continuous(labels = scales::dollar_format(prefix = "")) +
    labs(title    = str_glue("Max Pain Theory analysis on the SPY - {date_analysis}"),
         subtitle = "How many strikes ended OTM at Expiration (black dotted line was the closing price at expiration date).",
         caption  = "Data source: proprietary database (historically compiled from CBOE and Yahoo Finance).",
         x = "Strike Price",
         y = "Open Interest") + 
    theme(legend.position = "none")

# Calculating how many options ended OTM at expiration (full analysis)
db_Max_Pain <- NULL
for(expirations in db_Option_Chain_SS$final_date %>% unique()){ # expirations <- "2021-11-15"
  
  # Subsetting the DB
  db_Max_Pain_SS <- db_Option_Chain_SS %>%
    dplyr::filter(final_date == expirations)

  # Analyzing Calls
  db_MP_Calls <- db_Max_Pain_SS %>% 
    dplyr::filter(type == "calls") %>%
    dplyr::mutate(status = case_when(Strike > adjusted ~ "OTM",
                                     Strike == adjusted ~ "ATM",
                                     TRUE ~ "ITM"))  %>%
    dplyr::group_by(status) %>%
    dplyr::summarise(Total_OI = sum(OI)) %>%
    dplyr::mutate(Total_PCT  = Total_OI/sum(Total_OI),
                  Expiration = expirations) %>%
    dplyr::select(status, Total_PCT, Expiration) %>%
    spread(status, Total_PCT) %>%
    dplyr::mutate(Option_Type = "Call")
  
  db_MP_Puts <- db_Max_Pain_SS %>% 
    dplyr::filter(type == "puts") %>%
    dplyr::mutate(status = case_when(Strike > adjusted ~ "ITM",
                                     Strike == adjusted ~ "ATM",
                                     TRUE ~ "OTM"))  %>%
    dplyr::group_by(status) %>%
    dplyr::summarise(Total_OI = sum(OI)) %>%
    dplyr::mutate(Total_PCT  = Total_OI/sum(Total_OI),
                  Expiration = expirations) %>%
    dplyr::select(status, Total_PCT, Expiration) %>%
    spread(status, Total_PCT) %>%
    dplyr::mutate(Option_Type = "Put")
  
  # Saving the information
  if(is.null(db_Max_Pain)){
    db_Max_Pain <- db_MP_Calls %>% bind_rows(db_MP_Puts)
  }else{
    db_Max_Pain <- db_Max_Pain %>% bind_rows(db_MP_Calls) %>% bind_rows(db_MP_Puts)
  }
}

# Analyzing OTM and ITM Open Interest
db_MP_Calls <- db_Max_Pain %>%
  dplyr::filter(Option_Type == "Call") %>%
  dplyr::mutate(Expiration = Expiration %>% as.Date()) %>%
  dplyr::select(-Option_Type) %>%
  pivot_longer(names_to = "Status", values_to = "Pct", -Expiration) %>%
  dplyr::mutate(Type = "Calls")
  
db_MP_Puts <- db_Max_Pain %>%
  dplyr::filter(Option_Type == "Put") %>%
  dplyr::mutate(Expiration = Expiration %>% as.Date()) %>%
  dplyr::select(-Option_Type) %>%
  pivot_longer(names_to = "Status", values_to = "Pct", -Expiration) %>%
  dplyr::mutate(Type = "Puts")

db_MP_Calls %>%
  bind_rows(db_MP_Puts) %>%
  ggplot(aes(x = Expiration, y = Pct, fill = Status)) +
  geom_area(alpha = 0.4) +
  scale_fill_manual(values = c("green","red"))+ 
  scale_y_continuous(labels = scales::percent, limits = c(0, 1)) +
  labs(title    = "Percentage of total OI that ended, at expiration, ITM or OTM.",
       subtitle = "Analysis performed on the SPY.",
       caption  = str_glue("By: Carlos Jimenez"),
       x = "Expiration Date",
       y = "Open Interest") +
  theme(legend.title = element_blank()) +
  facet_free(Type ~ .)

# Stoping the Timer
tictoc::toc()