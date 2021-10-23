# Este script sirve para ejecutar los stop
# loss y take profit establecidos. Se tiene 
# ejecutar antes del script principal. 

# ----1.Librerias----
library(reticulate)
library(tidyverse)
library(timetk)
library(lubridate)
library(anytime)
library(TTR)
library(glue)

np <- import("numpy", convert = F)
pd <- import("pandas", convert = F)
binance <- import("binance", convert = F)

ruta_carpeta <- "C:/Users/llore/OneDrive/Documentos/github/Tecnologies/Binance - Bot - Free"
setwd(ruta_carpeta)
set.seed(2021)
options(scipen = 999)

# ----2.Credenciales----
requierements <- read_delim("requierements_example.txt", delim = ",", col_names = F)
client <- binance$Client(as.character(requierements[1,]), as.character(requierements[2,]))
stop_loss <- 0.015              # stop loss 
take_profit <- 0.03             # take profit

# Si hay alguna operacion en marcha
if((file.exists('trade_actual.rds')|file.exists('order_market_buy_r.rds'))){
  
  # ----3.Obtener datos-----
  trade_actual <- read_rds('trade_actual.rds')
  order_market_buy_r <- read_rds('order_market_buy_r.rds')
  tickers <- client$get_all_tickers() %>% py_to_r() %>% map(pluck(1)) %>% unlist()
  criptos <- tickers[str_detect(tickers, 'EUR')] %>% .[.!= 'EURUSDT']
  
  get_data <- function(simbolo, intervalo){
    
    # Nombres variables
    column_names <- c('dateTime', 'open', 'high', 'low', 'close', 'volume', 
                      'closeTime', 'quoteAssetVolume', 'numberOfTrades', 
                      'takerBuyBaseVol', 'takerBuyQuoteVol', 'ignore')
    
    # Obtener datos
    data <- pd$DataFrame(client$get_klines(symbol = simbolo, interval = intervalo)) %>% py_to_r()
    names(data) <- column_names
    
    data <- data %>%
      mutate(
        dateTime = anytime(dateTime/1000),
        closeTime = anytime(closeTime/1000)
      ) %>%
      mutate(across(where(is.character), ~ as.numeric(.)))
    
    return(data)
  }
  
  criptos_info <- client$get_exchange_info()["symbols"] %>% 
    py_to_r() %>% 
    keep(.p = function(x){
      x %>% 
        pluck(1) %in% criptos
    }) %>% set_names(criptos)
  
  data <- get_data(trade_actual$Activo, '30m')
  precio <- data$close %>% tail(1) 
  print(precio)
  
  wallet_func <- function(){
    
    account_info <- client$get_account()
    account_balance <- account_info$balances %>% py_to_r() 
    
    return(account_balance)
  }
  portfolio <- wallet_func()
  
  # -----4.Orden SL TP------
  profits_limits_func <- function(df, action){
    
    if(action == 1){
      
      sl <- df$close %>% tail(1) * (1 - stop_loss) 
      tp <- df$close %>% tail(1) * (1 + take_profit)
      
    } else if(action == -1){
      
      sl <- df$close %>% tail(1) * (1 + stop_loss)
      tp <- df$close %>% tail(1) * (1 - take_profit)
      
    }
    
    return(c(sl, tp))
  }
  
  order_market_sell_func <- function(symbol, quantity){ 
    
    order <- client$order_market_sell(
      symbol = symbol,
      quantity = quantity
    )
    
    return(order)
  }
  
  price_sl_tp <- profits_limits_func(trade_actual,1) %>% as.character()
  print(price_sl_tp)
  
  symbol <- order_market_buy_r$symbol
  print(symbol)
  
  max_decimals_func <- function(trade){
    info <- criptos_info %>% 
      keep(.p = function(x){x %>% 
          pluck(1) == trade$Activo}) %>% 
      .[[trade$Activo]] %>% 
      .[["filters"]] %>% 
      .[[3]] %>%
      .$minQty %>%
      str_locate(pattern = '1')
    info <- info - 2
    info <- info[1]
    
    return(info)
  }
  max_decimals <- max_decimals_func(trade_actual)
  max_decimals <- ifelse(max_decimals < 0, 0, max_decimals)
  
  quantity <- portfolio %>% 
    keep(.p = function(x){
      x %>% 
        pluck(1) == str_sub(trade_actual$Activo, end = -4)
      }) %>% 
    .[[1]] %>% 
    .$free %>% 
    as.numeric() 
  quantity <- quantity * ( 10 ^ max_decimals )
  quantity <- floor(quantity)
  quantity <- quantity / 10 ^ max_decimals
  print(quantity)
  
  # Si el precio de cierre del activo de la operacion
  # en cuestion rebasa algun umbral, se envia una orden
  # de venta de mercado y finalmente se borran los archivos 
  # de la operacion.
  if(precio > price_sl_tp[2]){
    
    order_market_sell <- order_market_sell_func(symbol, quantity)
    print('Take profit')
    print(order_market_sell)
    file.remove('order_market_buy_r.rds')
    file.remove('trade_actual.rds')
    
  } else if(precio < price_sl_tp[1]){
    
    order_market_sell <- order_market_sell_func(symbol, quantity)
    print('stop loss')
    print(order_market_sell)
    file.remove('order_market_buy_r.rds')
    file.remove('trade_actual.rds')
    
  }
  
}




