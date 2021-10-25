## README

Repositorio para el programa de un Bot integrado con Binance. 

Se usa Reticulate para la interacción con la Api de python de Binance.

El bot ejecuta ordenes de compra de mercado cuando existen 
unas señales basadas en los indicadores Estocástico, Rsi y el MACD.

Cuando el activo de la operación rebasa los umbrales de Stop 
Loss o Take profit ejecuta ordenes de venta de mercado.

El bot solo necesita establecer el directorio donde se va a guardar,
las claves de la api de binance y programarlo con el administrador de 
tareas de windows o crontab de mac que se ejecute cada 30 min o según 
la frecuencia deseada. 15 minutos es óptima.

Se puede ajustar la frecuencia, stop loss, take profit, los indicadores y
los demás parámetros establecidos.

Para más info:

llorencnoguera@gmail.com

No me hago responsable del uso de este programa. Esta hecho, con la 
mera intención de aprendizaje y no garantiza beneficios. Asuma su propio
riesgo cuando lo use. 


