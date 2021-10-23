# CRIPTO BOT BINANCE
#
# contacto: llorencnoguera@gmail.com
#
# El bot opera con las principales criptos. 
# Las principales criptos las defino como aquellas
# que tienen par con el euro. Una vez detectadas,
# se formaran nuevos pares respecto al BTC. De esta
# manera se tiene todo el capital de la cuenta (que está en 
# euros) invertido en algun activo, ya sea BTC cuando no
# se tiene ninguna posicion abierta o de lo contrario con alguna
# otra de las principales criptos. 
#
# El bot aplica una estrategia que combina 3 indicadores:
# el estocastico, el rsi y el macd. Una vez se suceden las
# pertinentes señales, el bot aplica una orden de compra. 
# La operacion se cerrara cuando el precio de cierre sobrepase
# los umbrales del stop loss o de take profit establecidos.
#
# La cantidad de cada operacion sera la maxima disponible
# de la cuenta.
# 
# No me hago responable del uso de este bot. Esta creado
# con una mera intencion recreacional y de aprendizaje. 
# No garantiza resultados, invierta su capital asumiendo
# su propio riesgo.

ruta_carpeta <- "C:/Users/llore/OneDrive/Documentos/github/Tecnologies/Binance - Bot - Free"
setwd(ruta_carpeta)
set.seed(2021)
options(scipen = 999)

# ----1.SL y TP-----

# Cuando el bot ya esta activado se debe ejecutar primero 
# el script de Stop Loss y Take Profit en caso que haya 
# alguna operacion en marcha.

source('script_sl_tp.R')
rm(list = ls())

# ----2.Librerias----

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

# ----3.Credenciales----

requierements <- read_delim("requierements.txt", delim = ",", col_names = F)
client <- binance$Client(as.character(requierements[1,]), as.character(requierements[2,]))

# ----4.Parametros bot----

freq <- "30m"           # frecuencia observaciones
n_rsi <- 14             # numero de periodos para el calculo del rsi
n_fast_k_stoch <- 14    # numero de periodos para el calculo del stoch señal rapida
n_fast_d_stoch <- 3     # numero de periodos para suavizar fastK señal lenta
n_slow_d_stoch <- 3     # numero de periodos para suavizar fastd 
n_fast_macd <- 12       # numero de periodos para la media movil rapida
n_slow_macd <- 26       # numero de periodos para la media movil lenta
n_sig_macd <- 9         # numero de periodos para la señal
sl <- 0.015             # stop loss 
tp <- 0.03              # take profit

# ----5.Obtener datos-----

# Todas las criptos de binance
tickers <- client$get_all_tickers() %>% py_to_r() %>% map(pluck(1)) %>% unlist()

# Filtramos las que nos interesan
criptos <- tickers[str_detect(tickers, 'EUR')] %>% .[.!= 'EURUSDT'] 
criptos_symbol <- criptos %>% str_sub(end = -4)
criptos_symbol <- criptos_symbol[!str_detect(criptos_symbol, 'EURB|BTC')]
criptos <- criptos_symbol %>% str_c('BTC', sep = '')
criptos <- tickers[tickers %in% criptos]

# Informacion criptos
criptos_info <- client$get_exchange_info()["symbols"] %>% 
  py_to_r() %>% 
  keep(.p = function(x){
    x %>% 
      pluck(1) %in% criptos
  }) %>% set_names(criptos)


# Funcion obtener datos
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

# Datos criptos
data <- map(criptos, get_data, freq) %>% set_names(criptos) 

# ----6.Estrategia----

# Creamos funcion donde aplicamos los indicadores
indicadors_func <- function(df){
  df <- df %>%
    select(dateTime, open, high, low, close, volume) %>%
    mutate(
      rsi = TTR::RSI(close, n = n_rsi)
    )
  
  stoch <- TTR::stoch(HLC = as.matrix(df %>% select(high, low, close), 
                                      nFastK = n_fast_k_stoch, 
                                      nFastD = n_fast_d_stoch, 
                                      nSlowD = n_slow_d_stoch)) %>% as.data.frame()
  macd <- TTR::MACD(df$close, nFast = n_fast_macd, nSlow = n_slow_macd, nSig = n_sig_macd, maType = "EMA") %>% as.data.frame()
  
  df <- bind_cols(
      df, stoch, macd
    )
  
  return(df)
}

# Aplicamos aviso de posibles errores
indicadors_func_safe <- possibly(.f = indicadors_func, otherwise = 'Error_indicadors', quiet = TRUE)

# Creamos funcion aplicando estrategia de los indicadores y sus umbrales
strategy_func <- function(df){
  
  df <- df %>%
    mutate(
      stoch_signal = case_when(
        fastK < 0.20 & fastD < 0.20 ~ 1,
        fastK > 0.80 & fastD > 0.80 ~ -1,
        TRUE ~ 0
      ),
      rsi_signal = case_when(
        lag(rsi,1) < 50 & rsi > 50 ~ 1,
        lag(rsi,1) > 50 & rsi < 50 ~ -1,
        TRUE ~ 0
      ),
      macd_signal = case_when(
        lag(macd,1) < lag(signal,1) & macd > signal ~ 1,
        lag(macd,1) > lag(signal,1) & macd < signal ~ -1,
        TRUE ~ 0
      )
    ) %>% 
    na.omit() %>% 
  mutate(
    action = case_when(
      (lag(stoch_signal, 2) == 1 | lag(stoch_signal, 1) == 1 | stoch_signal == 1) &
        (lag(rsi_signal, 2) == 1 | lag(rsi_signal, 1) == 1 | rsi_signal == 1) & 
        macd_signal == 1 & (fastK < 0.80 & fastD < 0.80) ~ 1,
      
      # (lag(stoch_signal, 2) == -1 | lag(stoch_signal, 1) == -1 | stoch_signal == -1) &
      #   (lag(rsi_signal, 2) == -1 | lag(rsi_signal, 1) == -1 | rsi_signal == -1) & 
      #   macd_signal == -1 & (fastK > 0.20 & fastD > 0.20) ~ -1,
      TRUE ~ 0
    )
  )
  
  return(df)
}

# Aplicamos aviso de posibles errores
strategy_func_safe <- possibly(.f = strategy_func, otherwise = 'Error', quiet = TRUE)

# Cremos funcion de backtest
backtest_func <- function(df){
  
  df <- df %>%
    mutate(
      sl = case_when(
        action == 1 ~ close * (1 - sl),
        action == -1 ~ close * (1 + sl),
        TRUE ~ 0
      ),
      tp = case_when(
        action == 1 ~ close * (1 + tp),
        action == -1 ~ close * (1 - tp),
        TRUE ~ 0
      )
    )
  
  trades <- row.names(df[df$action != 0,])
  
  limits <- list()
  
  for (i in trades){
    for(j in rownames(df)[as.numeric(rownames(df))>as.numeric(i)]){
      
      if(limits[[i]] %>% length() == 0){
        
        if(df[i,'action'] == 1 & df[i, 'sl'] > df[as.character(j), 'close']){
          
          limits[[i]][[as.character(j)]] <- 'sl_buy'
          
        } else if(df[i, 'action'] == 1 & df[i, 'tp'] < df[as.character(j), 'close']){
          
          limits[[i]][[as.character(j)]] <- 'tp_buy'
          
        } else if(df[i, 'action'] == -1 & df[i, 'sl'] < df[as.character(j), 'close']){
          
          limits[[i]][[as.character(j)]] <- 'sl_sell'
          
        } else if(df[i, 'action'] == -1 & df[i, 'tp'] > df[as.character(j), 'close']){
          
          limits[[i]][[as.character(j)]] <- 'tp_sell'
          
        }
        
      }
      
    }
  }
    return(limits)
}

# Aplicamos aviso de posibles errores
backtest_func_safe <- possibly(.f = backtest_func, otherwise = 'Error', quiet = TRUE)

# Nuevo conjunto de datos con indicadores y estrategia
data1 <- data %>% map( ~ indicadors_func_safe(.)) %>% map( ~ strategy_func_safe(.))

# Obtenemos todas las operaciones realizadas según los indicadores y la estrategia
trades <- map(data1, backtest_func_safe) %>% 
  discard(is_empty) %>% 
  unlist() %>% 
  as.data.frame() %>% 
  tibble::rownames_to_column() %>%
  rename(result = ".") %>%
  separate(col = rowname, into = c('activo', 'entra', 'sale'), sep = '\\.') %>% 
  rowwise() %>%
  mutate(tiempo_trade = as.numeric(sale) - as.numeric(entra)) %>% 
  ungroup() %>% 
  arrange(desc(tiempo_trade))

# Las variables 'entra' y 'sale' indican el número de observación del conjunto de datos
print(trades)

# Obtenemos el recuento de operaciones ganadoras y perdedoras
trades_sumario <- map(data1, backtest_func_safe) %>% 
  discard(is_empty) %>% 
  unlist() %>% 
  as.data.frame() %>% 
  tibble::rownames_to_column() %>%
  rename(result = ".") %>%
  group_by(result) %>%
  count()
print(trades_sumario)

# Buscamos si en este preciso momento hay alguna señal de compra
trade_actual <- data1 %>% 
  keep(~ is.data.frame(.)) %>%
  map_df(.f = function(x){
  x %>% 
    tail(1) %>%
    select(dateTime, action, close)
},.id = 'Activo')
trade_actual <- trade_actual %>% filter(action == 1)
trade_actual <- trade_actual[1,]
print(trade_actual)

# ----7.Portfolio----

# Funcion para obtener info de la cartera  
wallet_func <- function(){
  
  account_info <- client$get_account()
  account_balance <- account_info$balances %>% py_to_r() 
  
  return(account_balance)
}

# Obtenemos los activos de nuestra cartera
portfolio <- wallet_func()
eur_disp <- portfolio %>% 
  keep(.p = function(x){x %>% pluck(1) == 'BTC'}) %>% 
  flatten() %>% 
  .$free

print(eur_disp)

portfolio %>% keep(.p = function(x){x %>% pluck(2) %>% as.numeric() != 0}) %>% list2DF() %>% print()

# ----8.Orden-----

# Si existe una senyal de compra en este momento, 
# se envia una orden de mercado de compra
if(!anyNA(trade_actual)){
  
  symbol <- trade_actual$Activo
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
  max_decimals
  quantity_func <- function(trade, eur, max_decimal){
    quantity <- (eur %>% as.numeric() / trade$close %>% as.numeric()) %>% round(digits = max_decimal) %>% as.character()
    quantity <- as.character(as.numeric(quantity) - (0.1^max_decimal))
    return(quantity)
  }
  quantity <- quantity_func(trade_actual, eur_disp, symbol, max_decimals)
  print(quantity)
  
  order_market_buy_func <- function(symbol, quantity){
    
    order <- client$order_market_buy(
      symbol = symbol,
      quantity = quantity
    )
    
    return(order)
  }
  
  # Una vez obtenidos el par y cantidad, se envia la orden
  order_market_buy <- order_market_buy_func(symbol, quantity)
  order_market_buy_r <- order_market_buy %>% py_to_r()
  print(order_market_buy_r)
  
  # Si no ha habido errores se guardan los objetos de la operacion
  # ya que se utilizan en el script de ejecucion del stop loss y take profit
  if((!anyNA(trade_actual) & exists('order_market_buy_r'))){write_rds(trade_actual, 'trade_actual.rds')}
  if(exists('order_market_buy_r')){write_rds(order_market_buy_r, 'order_market_buy_r.rds')}
  
}

# # -----Visss------
# # OPCIONAL.
# # Se pueden visualizar las operaciones pasadas para mas clarificacion.
# 
# library(patchwork)
# library(plotly)
# 
# i <- "LTCBTC" # definir par
# x <- data1[[i]]
# x <- x %>% mutate(action = factor(action))
# ggclose <- ggplot(x, aes(x = dateTime, y = close))+
#   geom_line()+
#   geom_line(aes(y = high), color = 'yellow', alpha = 0.3)+
#   geom_line(aes(y = low), color = 'green', alpha = 0.3)+
#   geom_point(data = x[x$action != 0,], aes(color = action))+
#   ggtitle(glue(names(data1)[i]))
# ggstoch <- ggplot(x, aes(x = dateTime))+
#   geom_line(aes(y = fastK), color = 'red')+
#   geom_line(aes(y = fastD), color = 'blue')+
#   geom_hline(yintercept = 0.2)+
#   geom_hline(yintercept = 0.8)
# ggrsi <- ggplot(x, aes(x = dateTime))+
#   geom_line(aes(y = rsi))+
#   geom_hline(yintercept = 50)
# ggmacd <- ggplot(x, aes(x = dateTime))+
#   geom_line(aes(y = macd), color = 'red')+
#   geom_line(aes(y = signal), color = 'blue')+
#   geom_hline(yintercept = 0)
# gg <- (ggclose/ggstoch/ggrsi/ggmacd)
# gg
# ggplotly(p = ggclose)

q(save = 'no')
