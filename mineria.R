
#https://www.cienciadedatos.net/documentos/38_text_minig_con_r_ejemplo_practico_twitter#longitud_media_de_los_tweets_por_usuario
#https://www.cienciadedatos.net/

library(tidyverse)

setwd("C:/Users/Armando/Desktop/tweets")

tweets_elon       <- read_csv(file = "datos_tweets_@elonmusk.csv",
                              col_names = TRUE)
tweets_BillGates  <- read_csv(file = "datos_tweets_@BillGates.csv",
                              col_names = TRUE)
tweets_mayoredlee <- read_csv(file = "datos_tweets_@mayoredlee.csv",
                              col_names = TRUE)

# Se unen todos los tweets en un único dataframe
tweets <- bind_rows(tweets_elon, tweets_BillGates, tweets_mayoredlee)

# ver cantidad de tweets por grupo
tweets %>% group_by(screen_name) %>% summarise(numero_tweets = n()) 


# Selección de variables
tweets <- tweets %>% select(screen_name, created_at, status_id, text)

# Se renombran las variables con nombres más prácticos
tweets <- tweets %>% rename(autor = screen_name, fecha = created_at,
                            texto = text, tweet_id = status_id)
head(tweets)

########################################## funcion tokenizar
limpiar_tokenizar <- function(texto){
  # El orden de la limpieza no es arbitrario
  # Se convierte todo el texto a minúsculas
  nuevo_texto <- tolower(texto)
  # Eliminación de páginas web (palabras que empiezan por "http." seguidas 
  # de cualquier cosa que no sea un espacio)
  nuevo_texto <- str_replace_all(nuevo_texto,"http\\S*", "")
  # Eliminación de signos de puntuación
  nuevo_texto <- str_replace_all(nuevo_texto,"[[:punct:]]", " ")
  # Eliminación de números
  nuevo_texto <- str_replace_all(nuevo_texto,"[[:digit:]]", " ")
  # Eliminación de espacios en blanco múltiples
  nuevo_texto <- str_replace_all(nuevo_texto,"[\\s]+", " ")
  # Tokenización por palabras individuales
  nuevo_texto <- str_split(nuevo_texto, " ")[[1]]
  # Eliminación de tokens con una longitud < 2
  nuevo_texto <- keep(.x = nuevo_texto, .p = function(x){str_length(x) > 1})
  return(nuevo_texto)
}

test = "Esto es 1 ejemplo de l'limpieza de6 TEXTO  https://t.co/rnHPgyhx4Z @JoaquinAmatRodrigo #textmining"
limpiar_tokenizar(texto = test)

###################################################################

# Se aplica la función de limpieza y tokenización a cada tweet
tweets <- tweets %>% mutate(texto_tokenizado = map(.x = texto,
                                                   .f = limpiar_tokenizar))
tweets %>% select(texto_tokenizado) %>% head()

######################## Análisis exploratorio
#expansion vertical
tweets_tidy <- tweets %>% select(-texto) %>% unnest()
tweets_tidy <- tweets_tidy %>% rename(token = texto_tokenizado)
head(tweets_tidy) 

#distribucion temporal de los tweets
library(lubridate)

ggplot(tweets, aes(x = as.Date(fecha), fill = autor)) +
  geom_histogram(position = "identity", bins = 20, show.legend = FALSE) +
  scale_x_date(date_labels = "%m-%Y", date_breaks = "5 month") +
  labs(x = "fecha de publicación", y = "número de tweets") +
  facet_wrap(~ autor, ncol = 1) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90))

# distribucion temnporal 2
tweets_mes_anyo <- tweets %>% mutate(mes_anyo = format(fecha, "%Y-%m"))
tweets_mes_anyo %>% group_by(autor, mes_anyo) %>% summarise(n = n()) %>%
  ggplot(aes(x = mes_anyo, y = n, color = autor)) +
  geom_line(aes(group = autor)) +
  labs(title = "Número de tweets publicados", x = "fecha de publicación",
       y = "número de tweets") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, size = 6),
        legend.position = "bottom")

#palabras por usuario
tweets_tidy %>% group_by(autor) %>% summarise(n = n()) 
tweets_tidy %>%  ggplot(aes(x = autor)) + geom_bar() + coord_flip() + theme_bw() 

#palabras distintas por usuario
tweets_tidy %>% select(autor, token) %>% distinct() %>%  group_by(autor) %>%
  summarise(palabras_distintas = n()) 
tweets_tidy %>% select(autor, token) %>% distinct() %>%
  ggplot(aes(x = autor)) + geom_bar() + coord_flip() + theme_bw()

#longitud media por usuario
tweets_tidy %>% group_by(autor, tweet_id) %>% summarise(longitud = n()) %>% 
  group_by(autor) %>% summarise(media_longitud = mean(longitud),
                                sd_longitud = sd(longitud))


tweets_tidy %>% group_by(autor, tweet_id) %>% summarise(longitud = n()) %>%                      group_by(autor) %>%
  summarise(media_longitud = mean(longitud),
            sd_longitud = sd(longitud)) %>%
  ggplot(aes(x = autor, y = media_longitud)) +
  geom_col() +
  geom_errorbar(aes(ymin = media_longitud - sd_longitud,
                    ymax = media_longitud + sd_longitud)) +
  coord_flip() + theme_bw()

#Palabras más usadas por usuario
tweets_tidy %>% group_by(autor, token) %>% count(token) %>% group_by(autor) %>%
  top_n(10, n) %>% arrange(autor, desc(n)) %>% print(n=30)

########################## QUitar STOP WORDS
lista_stopwords <- c('me', 'my', 'myself', 'we', 'our', 'ours', 'ourselves',
                     'you','your', 'yours', 'yourself', 'yourselves', 'he', 'him','his',
                     'himself', 'she', 'her', 'hers', 'herself', 'it', 'its', 'itself',
                     'they', 'them', 'their', 'theirs', 'themselves', 'what', 'which',
                     'who', 'whom', 'this', 'that', 'these', 'those', 'am', 'is', 'are',
                     'was', 'were', 'be', 'been', 'being', 'have', 'has', 'had',
                     'having', 'do', 'does', 'did', 'doing', 'a', 'an', 'the', 'and',
                     'but', 'if', 'or', 'because', 'as', 'until', 'while', 'of', 'at',
                     'by', 'for', 'with', 'about', 'against', 'between', 'into',
                     'through', 'during', 'before', 'after', 'above', 'below', 'to',
                     'from', 'up', 'down', 'in', 'out', 'on', 'off', 'over', 'under',
                     'again', 'further', 'then', 'once', 'here', 'there', 'when',
                     'where', 'why', 'how', 'all', 'any', 'both', 'each', 'few', 'more',
                     'most', 'other', 'some', 'such', 'no', 'nor', 'not', 'only', 'own',
                     'same', 'so', 'than', 'too', 'very', 's', 't', 'can', 'will',
                     'just', 'don', 'should', 'now', 'd', 'll', 'm', 'o', 're', 've',
                     'y', 'ain', 'aren', 'couldn', 'didn', 'doesn', 'hadn', 'hasn',
                     'haven', 'isn', 'ma', 'mightn', 'mustn', 'needn', 'shan',
                     'shouldn', 'wasn', 'weren', 'won', 'wouldn','i')
# Se añade el término amp al listado de stopwords
lista_stopwords <- c(lista_stopwords, "amp")

# Se filtran las stopwords
tweets_tidy <- tweets_tidy %>% filter(!(token %in% lista_stopwords))



###################frecuencia palabras mas frecuentes sin stop
tweets_tidy %>% group_by(autor, token) %>% count(token) %>% group_by(autor) %>%
  top_n(10, n) %>% arrange(autor, desc(n)) %>%
  ggplot(aes(x = reorder(token,n), y = n, fill = autor)) +
  geom_col() +
  theme_bw() +
  labs(y = "", x = "") +
  theme(legend.position = "none") +
  coord_flip() +
  facet_wrap(~autor,scales = "free", ncol = 1, drop = TRUE)

###################### grafico en nube de palabras
library(wordcloud)
library(RColorBrewer)

wordcloud_custom <- function(grupo, df){
  print(grupo)
  wordcloud(words = df$token, freq = df$frecuencia,
            max.words = 400, random.order = FALSE, rot.per = 0.35,
            colors = brewer.pal(8, "Dark2"))
}

df_grouped <- tweets_tidy %>% group_by(autor, token) %>% count(token) %>%
  group_by(autor) %>% mutate(frecuencia = n / n()) %>%
  arrange(autor, desc(frecuencia)) %>% nest() 

walk2(.x = df_grouped$autor, .y = df_grouped$data, .f = wordcloud_custom)

############### CORRELACION ENTRE USUARIOS
library(gridExtra)
library(scales)

tweets_spread <- tweets_tidy %>% group_by(autor, token) %>% count(token) %>%
  spread(key = autor, value = n, fill = NA, drop = TRUE)

cor.test(~ mayoredlee + elonmusk, method = "pearson", data = tweets_spread)


#OTRA CORRELACION
cor.test(~ BillGates + elonmusk, data = tweets_spread)

#####GRAFICAR CVORRELACION
p1 <- ggplot(tweets_spread, aes(elonmusk, mayoredlee)) +
  geom_jitter(alpha = 0.1, size = 2.5, width = 0.25, height = 0.25) +
  geom_text(aes(label = token), check_overlap = TRUE, vjust = 1.5) +
  scale_x_log10(labels = percent_format()) +
  scale_y_log10(labels = percent_format()) +
  geom_abline(color = "red") +
  theme_bw() +
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank())

p2 <- ggplot(tweets_spread, aes(elonmusk, BillGates)) +
  geom_jitter(alpha = 0.1, size = 2.5, width = 0.25, height = 0.25) +
  geom_text(aes(label = token), check_overlap = TRUE, vjust = 1.5) +
  scale_x_log10(labels = percent_format()) +
  scale_y_log10(labels = percent_format()) +
  geom_abline(color = "red") +
  theme_bw() +
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank())

grid.arrange(p1, p2, nrow = 1)
########################### pALABRAS COMUNES
palabras_comunes <- dplyr::intersect(tweets_tidy %>% filter(autor=="elonmusk") %>%
                                       select(token), tweets_tidy %>% filter(autor=="mayoredlee") %>%
                                       select(token)) %>% nrow()
paste("Número de palabras comunes entre Elon Musk y Ed Lee", palabras_comunes)



palabras_comunes <- dplyr::intersect(tweets_tidy %>% filter(autor=="elonmusk") %>%
                                       select(token), tweets_tidy %>% filter(autor=="BillGates") %>%
                                       select(token)) %>% nrow()
paste("Número de palabras comunes entre Elon Musk y Bill Gates", palabras_comunes)

#########################Comparación en el uso de palabras
# Pivotaje y despivotaje
tweets_spread <- tweets_tidy %>% group_by(autor, token) %>% count(token) %>%
  spread(key = autor, value = n, fill = 0, drop = TRUE)
tweets_unpivot <- tweets_spread %>% gather(key = "autor", value = "n", -token)

# Selección de los autores elonmusk y mayoredlee
tweets_unpivot <- tweets_unpivot %>% filter(autor %in% c("elonmusk",
                                                         "mayoredlee"))
# Se añade el total de palabras de cada autor
tweets_unpivot <- tweets_unpivot %>% left_join(tweets_tidy %>%
                                                 group_by(autor) %>%
                                                 summarise(N = n()),
                                               by = "autor")
# Cálculo de odds y log of odds de cada palabra
tweets_logOdds <- tweets_unpivot %>%  mutate(odds = (n + 1) / (N + 1))
tweets_logOdds <- tweets_logOdds %>% select(autor, token, odds) %>% 
  spread(key = autor, value = odds)
tweets_logOdds <- tweets_logOdds %>%  mutate(log_odds = log(elonmusk/mayoredlee),
                                             abs_log_odds = abs(log_odds))
# Si el logaritmo de odds es mayor que cero, significa que es una palabra con
# mayor probabilidad de ser de Elon Musk. Esto es así porque el ratio sea ha
# calculado como elonmusk/mayoredlee.
tweets_logOdds <- tweets_logOdds %>%
  mutate(autor_frecuente = if_else(log_odds > 0,
                                   "Elon",
                                   "Mayor"))
tweets_logOdds %>% arrange(desc(abs_log_odds)) %>% head() 

###### Representación de las 30 palabras más diferenciadas 
tweets_logOdds %>% group_by(autor_frecuente) %>% top_n(15, abs_log_odds) %>%
  ggplot(aes(x = reorder(token, log_odds), y = log_odds, fill = autor_frecuente)) +
  geom_col() +
  labs(x = "palabra", y = "log odds ratio (@elonmusk / mayoredlee)") +
  coord_flip() + 
  theme_bw()

########### Relación entre palabras
library(tidytext)
limpiar <- function(texto){
  # El orden de la limpieza no es arbitrario
  # Se convierte todo el texto a minúsculas
  nuevo_texto <- tolower(texto)
  # Eliminación de páginas web (palabras que empiezan por "http." seguidas 
  # de cualquier cosa que no sea un espacio)
  nuevo_texto <- str_replace_all(nuevo_texto,"http\\S*", "")
  # Eliminación de signos de puntuación
  nuevo_texto <- str_replace_all(nuevo_texto,"[[:punct:]]", " ")
  # Eliminación de números
  nuevo_texto <- str_replace_all(nuevo_texto,"[[:digit:]]", " ")
  # Eliminación de espacios en blanco múltiples
  nuevo_texto <- str_replace_all(nuevo_texto,"[\\s]+", " ")
  return(nuevo_texto)
}

bigramas <- tweets %>% mutate(texto = limpiar(texto)) %>%
  select(texto) %>%
  unnest_tokens(input = texto, output = "bigrama",
                token = "ngrams",n = 2, drop = TRUE)

# Contaje de ocurrencias de cada bigrama
bigramas  %>% count(bigrama, sort = TRUE)

######################## Separar los bigramas por stop words

# Separación de los bigramas 
bigrams_separados <- bigramas %>% separate(bigrama, c("palabra1", "palabra2"),
                                           sep = " ")
head(bigrams_separados)

# Filtrado de los bigramas que contienen alguna stopword
bigrams_separados <- bigrams_separados  %>%
  filter(!palabra1 %in% lista_stopwords) %>%
  filter(!palabra2 %in% lista_stopwords)

# Unión de las palabras para formar de nuevo los bigramas
bigramas <- bigrams_separados %>%
  unite(bigrama, palabra1, palabra2, sep = " ")

# Nuevo contaje para identificar los bigramas más frecuentes
bigramas  %>% count(bigrama, sort = TRUE) %>% print(n = 20)


##########################NUEVO############################
library(igraph)
library(ggraph)

bigram_counts <- bigrams_separados %>% 
  dplyr::count(palabra1, palabra2, sort = TRUE) # contamos la cantidad de words por bigrama

#quitar na

bigram_counts <-na.omit(bigram_counts)

#exportar como csv

library(rio)
export(bigram_counts, "cuenta_bigramas.xlsx")

#Uniendolos de nuevo en la columna bigrams
# bigrams_united <- bigrams_separados %>%
#   unite(bigram, palabra1, palabra2, sep = " ") # count bigrams cleaning
# bigrams_united %>%
#   dplyr::count(bigram, sort = TRUE)

bigram_counts %>%
  filter(n >= 10) %>% #filtro para grafico mas de 20 interacciones
  graph_from_data_frame() %>%
  ggraph(layout = "fr") +
  geom_edge_link(aes(edge_alpha = n, edge_width = n), edge_colour = "green") +
  geom_node_point(size = 2) +
  geom_node_text(aes(label = name), repel = TRUE, 
                 point.padding = unit(0.2, "lines")) +
  ggtitle('Bigramas')

##################### cuantificar la temática de un texto ################################

# Número de veces que aparece cada término por tweet
tweets_tf <- tweets_tidy %>% group_by(tweet_id, token) %>% summarise(n = n())

# Se añade una columna con el total de términos por tweet
tweets_tf <- tweets_tf %>% mutate(total_n = sum(n))

# Se calcula el tf
tweets_tf <- tweets_tf %>% mutate(tf = n / total_n )
head(tweets_tf)


total_documentos = tweets_tidy$tweet_id %>% unique() %>% length()
total_documentos

# Número de documentos en los que aparece cada término
tweets_idf <- tweets_tidy %>% distinct(token, tweet_id) %>% group_by(token) %>%
  summarise(n_documentos = n())

# Cálculo del idf
tweets_idf <- tweets_idf %>% mutate(idf = n_documentos/ total_documentos) %>%
  arrange(desc(idf))
head(tweets_idf)

library(kableExtra)
tweets_tf_idf <- left_join(x = tweets_tf, y = tweets_idf, by = "token") %>% ungroup()
tweets_tf_idf <- tweets_tf_idf %>% mutate(tf_idf = tf * idf)
tweets_tf_idf %>% select(-tweet_id) %>% head() %>% kable()


View(tweets_tf_idf)

########################### Clasificación de tweets

