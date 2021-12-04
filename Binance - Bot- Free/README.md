## README

Repositorio para el programa de un Bot integrado con Binance. 

Se usa Reticulate para la interacci�n con la Api de python de Binance.

El bot ejecuta ordenes de compra de mercado cuando existen 
unas se�ales basadas en los indicadores Estoc�stico, Rsi y el MACD.

Cuando el activo de la operaci�n rebasa los umbrales de Stop 
Loss o Take profit ejecuta ordenes de venta de mercado.

El bot solo necesita establecer el directorio donde se va a guardar,
las claves de la api de binance y programarlo con el administrador de 
tareas de windows o crontab de mac que se ejecute cada 30 min o seg�n 
la frecuencia deseada. 15 minutos es �ptima.

Se puede ajustar la frecuencia, stop loss, take profit, los indicadores y
los dem�s par�metros establecidos.

Para m�s info:

llorencnoguera@gmail.com

No me hago responsable del uso de este programa. Esta hecho, con la 
mera intenci�n de aprendizaje y no garantiza beneficios. Asuma su propio
riesgo cuando lo use. 

--------------

Repository for bot integrated with the Bianance API.

Uses Reticulate for the interaction with the python Binance API.

The bot executes buy market orders when there is a signal, which is based in
a combination of the Stochastic, RSI and MACD.

When the asset broke either the Stop Loss limit or Take profit limit
executes the sell market order.

The bot only needs to know the directory where it will work, 
the Binance API key and set it up with the task scheduler or crontab 
to start the job with the desired frecuency. 15 min is fine.

You can edit all the parameters and customize it like the stop loss,
take profit, indicators, ...

More info:
llorencnoguera@gmail.com

Disclaimer: I am not responsible of the use of this bot. It's for 
educational purproses and doesn't guarantee profits. Asume your own risk.


