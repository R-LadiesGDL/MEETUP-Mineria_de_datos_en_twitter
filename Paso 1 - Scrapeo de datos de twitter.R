#### Minería de datos de Twitter para analizar eventos políticos y
#### movimientos sociales

#### DANIELA DE LOS SANTOS 
#### Meetup: RLadies Guadalajara 
#### 6 de julio de 2021 

###################################################################################

# El primer paso es el scrapeo de datos de twitter

# Para esto es necesario
# 1) Tener una cuenta de twitter
# 2) Con los datos de su cuenta de twitter, setear una cuenta de 'developer'
# https://developer.twitter.com/en/apply-for-access en este link pueden aplicar para 
# una cuenta de developer. 
# Generar una app y obtener keys y token. Ver presentación para más detalles!

install.packages("rtweet")
library(rtweet)

## Tenemos que configurar todas estas cosas: key, secret, appname, access token y access secret.
## En mi presentación pueden ubicar exactamente los lugares de donde obtenerlos.
## Aquí pongo unos míos de ejemplo para utilizar en el taller, pero que voy a resetear 
## después de terminar.

key= "7e1wDsU90EZyTsCSoNjqcGMNg"
secret= "EVJgLkYugIcSdKJEEu40T3fmrnC5jkpPmDyqpHheYJxGk6uMfu"
appname <- "Ejemplo_RladiesGua"

access_token = "155965570-Y1UlDtE7A1cgl3vTGX2XbOwPEBIZmxPGZyQXmVpB"
access_secret= "RBuvJsUTDYFpl2iw6qafO4Fk9xkOrRcLSpd5PXixWdgEB"

# Creamos un token con todos estos datos, que utiliza el paquete rtweet.
twitter_token <- create_token(
  app = appname,
  consumer_key = key,
  consumer_secret = secret,
  access_token = access_token,
  access_secret = access_secret)

# ¡Vamos a probarlo!

## Podemos buscar los datos de un usuarie:
prueba_user <- search_users("danidlsa")
dplyr::glimpse(prueba_user)
View(prueba_user)

## Podemos buscar un hashtag:
rstats_tweets <- search_tweets(q = "#rstats",
                               n = 100)
dplyr::glimpse(rstats_tweets) #Noten que tenemos distintos tipos de datos
View(rstats_tweets)

rm(list=ls())
# Como ven, las bases de datos son super completas.

# Pueden consultar la documentación de rtweet para ver otro tipo de scrapeos que pueden hacer.
help(rtweet)

# Otro paquete que cumple funciones similares es "twitteR".
# https://cran.r-project.org/web/packages/twitteR/twitteR.pdf


# Veamos, ahora, qué tipo de análisis podemos hacer con esto...
# Para eso, vamos al script "Paso 2".