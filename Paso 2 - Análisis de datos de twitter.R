#### Minería de datos de Twitter para analizar eventos políticos y
#### movimientos sociales

#### DANIELA DE LOS SANTOS 
#### Meetup: RLadies Guadalajara 
#### 6 de julio de 2021 

###################################################################################

# Hagamos algunas pruebas de tipos de análisis que podemos hacer con datos scrapeados de Twitter.

# Para esto, vamos a usar unas bases de datos de tweets que yo he descargado en el pasado
# Para descargarlas simplemente usé el paquete rtweet, como muestra el código "Paso 1".

# Vamos a analizar un movimiento social, y un evento.

library(tidyverse)
library(tm)
library(ggwordcloud)
library(igraph)
library(networkD3)
library(zoo)

################# ANÁLISIS DE UN MOVIMIENTO SOCIAL ###################
# Vamos a descargar una base de datos que contiene todos los tuits del colectivo Ni una Menos Argentina
# desde la creación de su cuenta de twitter, hasta abril de 2021.

num <- readr::read_csv2("https://raw.githubusercontent.com/danidlsa/presentacion_rladies_GDL/main/ColectivoNUM.csv")

# Esta base es una versión simplificada de lo que podemos descargar con rtweet.

# Veamos cómo se distribuyen sus tuits en el tiempo

num <- num %>% mutate(fecha=as.Date(fecha, "%d/%m/%Y"))
num <- num %>% mutate(ym=as.yearmon(fecha))

num %>% group_by(ym) %>%
  count() %>%
  ggplot(aes(x=ym, y=n)) +
  geom_line(col="darkgreen") +
  theme_bw() +
  labs(x="Mes y año", y="N° de tuits",
       title="N° de tuits mensuales",
       subtitle="Período 2016-2021",
       caption="Fuente: elaboración propia") +
  theme(legend.position = "",
        axis.title=element_text(size=12),
        plot.title=element_text(face="bold", size=14),
        strip.text=element_text(size=12), 
        axis.text.x=element_text(angle=90),
        plot.subtitle=element_text(size=13, face="italic"))

# ¿Cuáles son sus hashtags más difundidos?

## Primero extraemos los hashtags
## Ojo, con el paquete rtweet ya tenemos una columna con los hashtags extraidos y listados
## Este ejercicio lo hacemos porque no tenemos esa columna (los datos fueron extraidos con otro método)
hashtags = str_extract_all(num$tuit, "#\\S+") 
hashtag_word <- unlist(hashtags)
hashtag_word <- tolower(hashtag_word)
hashtag_word <- gsub("[[:punct:]ー]", "", hashtag_word)

hashtag_count <- as.data.frame(table(hashtag_word))
hashtag_count <- hashtag_count[order(-hashtag_count$Freq),]

hashtag_count_20 <- head(hashtag_count, 20) # Tomemos los 20 más frecuentes

ggplot(hashtag_count_20, aes(x=reorder(hashtag_word, Freq), y=Freq)) +
  geom_bar(stat="identity") +
  coord_flip() +
  labs(y="N° tuits incluyendo el hashtag",
       x="Hashtag",
       title="20 hashtags más difundidos por el colectivo Ni Una Menos") +
  geom_text(aes(label=Freq), size=3, hjust=-.5) +
  theme_bw() +
  scale_color_brewer(palette="Dark2") +
  theme(legend.position = "",
        axis.title=element_text(size=12),
        plot.title=element_text(face="bold", size=14),
        strip.text=element_text(size=12), 
        axis.text.x=element_text(angle=90),
        plot.subtitle=element_text(size=13, face="italic"))


# Ahora cambiemos el tipo de datos que estamos usando.

# En lugar de analizar un usuario, analizaremos un hashtag.

rm(list=ls())


############## ANÁLISIS DE UN "EVENTO POLÍTICO" ##################################

# Los siguientes datos fueron descargados el 30 de marzo de 2020.
# Se trata de 2416 tuits que usaron el hashtag #SindicatoDelMiedo en los días anteriores.
# #SindicatoDelMiedo fue tendencia en Uruguay durante varios días, como parte de una campaña
# de desprestigio al sindicato médico, que pedía parar las actividades por el aumento de casos de COVID.

sindicato_miedo <- readRDS(url("https://github.com/danidlsa/presentacion_rladies_GDL/blob/main/Tuits%20Sindicato%20del%20miedo.rds?raw=true"))

# Veamos primero cuándo aparecen los primeros tuits, y cómo crecen de un día para otro.

sindicato_miedo <- sindicato_miedo %>% mutate(dia=as.Date(created_at))

resum <- sindicato_miedo %>% group_by(dia) %>%
  count() %>%
  ungroup()

ggplot(resum, aes(x=dia, y=n)) +
  geom_line(stat="identity", color="#1da1f2", size=1) +
  labs(x="Fecha", y="N° de tuits", title="Tuits con el hashtag #SindicatoDelMiedo",
       caption="@danidlsa") +
  ggdark::dark_theme_bw() +
  theme(plot.title=element_text(color="#1da1f2"))

# ¿Quién empezó con la tendencia?

primeros_tuits <- sindicato_miedo[order(sindicato_miedo$created_at),]
primeros_tuits <- head(primeros_tuits, 5)

primeros_tuits %>% select(screen_name,
                                    created_at,
                                    text,
                                    favorite_count,
                                    retweet_count) %>%
  flextable::flextable()

# Muchos de esos tuits son de hecho, retuits. Podemos filtrar los originales.

originales <- sindicato_miedo %>% filter(is_retweet==F)
originales <- originales[order(-originales$retweet_count),]

# Tuiteros 

tuiteros <- sindicato_miedo %>% group_by(screen_name) %>%
  count() # 1018 usuarios usaron el hashtag


# Pero algunos lo usaron mucho más que otros. 
# Veamos quiénes son los usuarios que usaron el hashtag más de 10 veces.


tuiteros <- tuiteros[order(-tuiteros$n),]
militantes <- tuiteros %>% filter(n>=10) # Podemos llamarlos "militantes"
head(militantes, 1) # El militante más activo registra 70 tuits en menos de 24 horas.



######### Podemos trazar redes e identificar cómo se relacionan las personas usando el hashtag

red <- sindicato_miedo %>%
  filter(is_retweet == TRUE)%>%
  select(screen_name,retweet_screen_name) #columnas que integrarán la red

t <- red %>% group_by(screen_name) %>%
  count() %>%
  ungroup() # calculamos el tamaño de los nodos

# Vamos además a ver cuáles de los usuarios más retuiteados son nuestros usuarios "militantes", a ver si los 
# podemos visualizar mejor en la red
militantes <- militantes %>% select(-n) %>% mutate(militante=1)
t <- t %>% left_join(militantes, by="screen_name")
t <- t %>% mutate(militante=ifelse(is.na(militante)==F, 1, 0))

red <- red %>% left_join(t, by="screen_name")
colnames(red)[3] <- "size"
red <- graph_from_data_frame(red, directed = TRUE) #Creo el el objeto igraph para darle formato red 
class(red)
V(red)
E(red)

# ploteo con NetworkD3

red3D <- igraph_to_networkD3(red)

vinculos <- red3D[["links"]]
nodos <- red3D[["nodes"]]
colnames(t)[1] <- "name"
nodos <- nodos %>% left_join(t, by="name")
nodos <- nodos %>% mutate(militante=ifelse(militante==1, 1, 
                                           ifelse(is.na(militante), 0, 0))) # recodificamos missing values
my_color <- 'd3.scaleOrdinal() .range(["lightblue", "green"])' # seteamos los colores de la red

forceNetwork(Links = vinculos, Nodes = nodos, 
             Source = 'source', Target = "target", 
             NodeID = 'name', Group = 'militante', opacity = 1.5, 
             fontSize = 20, linkDistance = 100, zoom = TRUE, 
             Nodesize = "n", colourScale = my_color
            )

# Llegamos al final! Gracias! 

