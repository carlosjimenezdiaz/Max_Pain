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

# Local Dataframes
db_Tickers <- data.frame(Symbol         = c("SPX", "SPY"),
                         Price_Ratios   = c(10, 1),
                         Multiplier     = c(100, 100),
                         div_yield      = c(1.3, 1.3),
                         risk_free_rate = c(1.9, 1.9))

# Getting historical data of the SPY
hist_data_SPY <- tq_get("SPY",
                        from = "2021-11-14", # First day in our data base
                        to   = Sys.Date() + 1,
                        get  = "stock.prices",
                        complete_cases = TRUE) %>%
  dplyr::select(date, adjusted)

# Getting historical Option Chain
db_Option_Chain <- Option_Chain_SQL("db_credentials/mysql_financial_data.yml") 

# Feature Eng - enhancing the Option Chain DB
db_Option_Chain_Enhanced <- db_Option_Chain %>%
  dplyr::mutate(Date_Process = as.Date(Time_Process)) %>%
  left_join(hist_data_SPY, by = c("Date_Process" = "date")) %>%
  left_join(db_Tickers, by = "Symbol") %>%
  dplyr::mutate(time_expiration_years = (((final_date %>% as.Date()) - Date_Process) %>% as.numeric())/365) %>%
  dplyr::mutate(Spot_Price = adjusted * Price_Ratios)

# Calculating Historical GEX and Gamma Flip
db_GEX_Historical_Performance <- NULL
db_GEX_Profile <- NULL
DB_Gamma_Flip  <- NULL

for(Date_Analysis in db_Option_Chain_Enhanced$Date_Process %>% unique()){ # Date_Analysis <- "2021-11-15"
  
  # Historical GEX Calculations
    # Filtering DB of Option Chain per Date
    db_Option_Chain_Subset <- db_Option_Chain_Enhanced %>% 
      dplyr::filter(Date_Process == Date_Analysis)
    
    # CALLS
    db_option_chain_GEX <- db_Option_Chain_Subset %>% dplyr::filter(type == "calls")
    
    GEX_Calls <- greeks(bscall(s  = db_option_chain_GEX$Spot_Price,
                               k  = db_option_chain_GEX$Strike,
                               v  = db_option_chain_GEX$IV,
                               r  = db_option_chain_GEX$risk_free_rate/100,
                               tt = db_option_chain_GEX$time_expiration_years,
                               d  = db_option_chain_GEX$div_yield/100), complete = TRUE) %>%
      dplyr::select(k, s, Gamma) %>%
      dplyr::transmute(Date = db_option_chain_GEX$final_date,
                       type = "calls",
                       GEX  = Gamma * db_option_chain_GEX$Multiplier * db_option_chain_GEX$OI * s^2 * 0.01,
                       k    = k * db_option_chain_GEX$Price_Ratios)
    
    # PUTS
    db_option_chain_GEX <- db_Option_Chain_Subset %>% dplyr::filter(type == "puts")
    
    GEX_Puts <- greeks(bsput(s  = db_option_chain_GEX$Spot_Price,
                             k  = db_option_chain_GEX$Strike,
                             v  = db_option_chain_GEX$IV,
                             r  = db_option_chain_GEX$risk_free_rate/100,
                             tt = db_option_chain_GEX$time_expiration_years,
                             d  = db_option_chain_GEX$div_yield/100), complete = TRUE) %>%
      dplyr::select(k, s, Gamma) %>%
      dplyr::transmute(Date = db_option_chain_GEX$final_date,
                       type = "puts",
                       GEX  = -1 * Gamma * db_option_chain_GEX$Multiplier * db_option_chain_GEX$OI * s^2 * 0.01,
                       k    = k * db_option_chain_GEX$Price_Ratios)
    
    # Merging both GEX DB
    db_GEX <- GEX_Calls %>% rbind(GEX_Puts)
    
    # Saving the info related to the GEX calculation during Date_Analysis
    hist_GEX <- data.frame(Date = Date_Analysis %>% as.Date(),
                           GEX  = db_GEX$GEX %>% sum())
    
    # Accumulating
    db_GEX_Historical_Performance <- db_GEX_Historical_Performance %>% rbind(hist_GEX)
    
  # Calculating the Gamma Flip
    # Setting the Delta in price
    Spot_Steps <- seq(-20, 20, 0.1)
    
    for(new_step in Spot_Steps){ # new_step <- 0
      
      # GEX for the Calls
      db_option_chain_enhance_Calls <- db_Option_Chain_Subset %>% dplyr::filter(type == "calls")
      
      GEX_Calls <- greeks(bscall(s  = db_option_chain_enhance_Calls$Spot_Price*(1 + new_step/100),
                                 k  = db_option_chain_enhance_Calls$Strike,
                                 v  = db_option_chain_enhance_Calls$IV,
                                 r  = db_option_chain_enhance_Calls$risk_free_rate/100,
                                 tt = db_option_chain_enhance_Calls$time_expiration_years,
                                 d  = db_option_chain_enhance_Calls$div_yield/100), complete = TRUE) %>%
        dplyr::select(k, s, Gamma) %>%
        dplyr::mutate(Date = db_option_chain_enhance_Calls$final_date,
                      OI   = db_option_chain_enhance_Calls %>% dplyr::filter(type == "Calls") %>% dplyr::select(OI) %>% pluck(1), 
                      type = "Calls",
                      GEX  = Gamma * db_option_chain_enhance_Calls$Multiplier * db_option_chain_enhance_Calls$OI * s^2 * 0.01)
      
      # GEX for the Puts
      db_option_chain_enhance_Puts <- db_Option_Chain_Subset %>% dplyr::filter(type == "puts")
      
      GEX_Puts <- greeks(bsput(s  = db_option_chain_enhance_Puts$Spot_Price*(1 + new_step/100),
                               k  = db_option_chain_enhance_Puts$Strike,
                               v  = db_option_chain_enhance_Puts$IV,
                               r  = db_option_chain_enhance_Puts$risk_free_rate/100,
                               tt = db_option_chain_enhance_Puts$time_expiration_years,
                               d  = db_option_chain_enhance_Puts$div_yield/100), complete = TRUE) %>%
        dplyr::select(k, s, Gamma) %>%
        dplyr::mutate(Date = db_option_chain_enhance_Puts$final_date,
                      OI   = db_option_chain_enhance_Puts %>% dplyr::filter(type == "Puts") %>% dplyr::select(OI) %>% pluck(1), 
                      type = "Puts",
                      GEX  = Gamma * db_option_chain_enhance_Puts$Multiplier * db_option_chain_enhance_Puts$OI * s^2 * 0.01 * -1)
      
      # Combining all the GEX DB
      db_GEX_Spot <- GEX_Puts %>% rbind(GEX_Calls)
      
      # Saving the calculations in a provisional Dataframe
      db_GEX_Profile_prov <- data.frame(Spot_Change = new_step,
                                        GEX         = sum(db_GEX_Spot$GEX))
      
      # Accumulating 
      db_GEX_Profile <- db_GEX_Profile %>% rbind(db_GEX_Profile_prov)
    }
    
    # Extracting the Gamma Flip level
    Gamma_Flip <- db_GEX_Profile %>%
      dplyr::mutate(Spot_Change = lowess(Spot_Change, GEX, f = 1/10) %>% pluck(1),
                    GEX         = (lowess(Spot_Change, GEX, f = 1/10) %>% pluck(2))/1000000000,
                    Flip = ifelse(dplyr::lead(GEX > 0) & GEX < 0, "Yes", "No")) %>%
      dplyr::filter(Flip == "Yes") %>%
      dplyr::select(Spot_Change) %>%
      pull(1) %>% first()
    
    # Keeping the Recors
    DB_Gamma_Flip <- c(DB_Gamma_Flip, Gamma_Flip)
}

# Plotting the Historical GEX (and interpolating the missing values with linear approximation)
db_GEX_Historical_Performance %>%
  dplyr::mutate(GEX = na.approx(GEX)) %>%
  ggplot() +
  geom_line(aes(x = Date, y = GEX/1000000000)) +
  geom_rect(inherit.aes = FALSE,
            aes(xmin = db_GEX_Historical_Performance$Date %>% min(), 
                xmax = db_GEX_Historical_Performance$Date %>% max(), 
                ymin = -Inf, 
                ymax = 0), 
            fill ='red', alpha = 0.002) +
  geom_rect(inherit.aes = FALSE,
            aes(xmin = db_GEX_Historical_Performance$Date %>% min(), 
                xmax = db_GEX_Historical_Performance$Date %>% max(), 
                ymin = 0, 
                ymax = +Inf), 
            fill ='green', alpha = 0.002) +
  scale_y_continuous(labels = scales::dollar_format()) +
  labs(title    = "Historical behavior of GEX in the SP500.",
       subtitle = "Assuming dealers are long calls and short puts (considering SPX and SPY).",
       caption  = str_glue("By: Carlos Jimenez - {Sys.Date()}"),
       x = "Date",
       y = "Gamma Exposure (Billions)")

# Relationship between Returns in the SP500 and GEX
db_GEX_Historical_Performance %>%
  dplyr::mutate(GEX = na.approx(GEX)) %>%
  left_join(historical_prices <- tq_get("SPY",
                                        from = "2021-11-14", # First day in our data base
                                        to   = Sys.Date() + 1,
                                        get  = "stock.prices",
                                        complete_cases = TRUE) %>%
              dplyr::select(date, adjusted), by = c("Date" = "date")) %>%
  dplyr::mutate(adjusted = na.approx(adjusted),
                Ret      = TTR::ROC(adjusted, n = 1)) %>%
  replace(is.na(.), 0) %>%
  ggplot(aes(x = GEX/1000000000, y = Ret)) + 
  geom_point(shape = 18, 
             color = "red")+
  geom_smooth(method   = glm,  
              linetype = "dashed",
              color    = "darkblue", 
              fill     = "orange") +
  geom_vline(xintercept = 0, linetype = "dotted", color = "black", size = 0.9) +
  geom_hline(yintercept = 0, linetype = "dotted", color = "black", size = 0.9) +
  scale_x_continuous(labels = scales::dollar_format()) +
  labs(title    = "Relationship between Gamma Exposure and Returns in the SP500 - daily",
       subtitle = "Assuming dealers are long calls and short puts (considering SPX and SPY).",
       caption  = str_glue("By: Carlos Jimenez - {Sys.Date()}"),
       x = "Gamma Exposure (Billions)",
       y = "Change in Price") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  theme(legend.title = element_blank())  
