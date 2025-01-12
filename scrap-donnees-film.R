""" Principes 
1) Récupération depuis les pages wikipedia des films français par année, la liste des films des années voulues
 Format de l'URL https://fr.wikipedia.org/wiki/Liste_de_films_fran%C3%A7ais_sortis_en_2015
 années disponibles  1903 à 2027
 
2) deux types de films ceux qui existent dans wikipedia (lien écrit en bleu) => ce sont les seuls à récupérer

3) Pour chaque film récupéré, lien vers wikipedia, récupération des liens allocine et imdb

4) Pour chaque film aller sur allocine et IMDB et récupérer les données
""" 
### Library utilisées

library(rvest)
library(tidyverse)
library(dplyr)

###Récupération de la liste de films de l'année 2000


url_film <- "https://fr.wikipedia.org/wiki/Liste_de_films_fran%C3%A7ais_sortis_en_2000"
data_html <- url_film %>% read_html()

# Extraire la table contenant les films, elle s'appelle wikitable
films_table_html <- data_html %>% html_node("table.wikitable") # Identifier la classe de la table

# Extraire les lignes (rows) du tableau
rows <- films_table_html %>% html_nodes("tr")

# Extraire les informations des titres et des liens
films_data <- rows %>%
  lapply(function(row) {
    titre_node <- row %>% html_node("td:nth-child(1) a") # Première colonne avec le titre et le lien
    titre <- titre_node %>% html_text(trim = TRUE)       # Texte du titre
    lien <- titre_node %>% html_attr("href")             # Lien relatif
    
    # Combiner les données
    list(Titre = titre, Lien = ifelse(!is.na(lien), paste0("https://fr.wikipedia.org", lien), NA))
  }) %>%
  bind_rows() # Convertir en data frame

# Nettoyer les données pour retirer les lignes vides
films_data <- films_data %>% filter(!is.na(Titre))

# Afficher un aperçu des résultats
print(films_data)
# ***************************************************************

# ---- Liste films : Récupération depuis les pages wikipedia des films de l'année courante(-1) à l'année minimum ----

# ***************************************************************

#Année courante 
year <- format(Sys.Date(), "%Y")

annee_max <- as.integer(year)-1 ## année en cours -1
annee_min <- 2000 ## année minimale

periode <- annee_min:annee_max
films <- tibble
for (annee in periode) {

  url_base <-"https://fr.wikipedia.org/wiki/Liste_de_films_fran%C3%A7ais_sortis_en_"
  url_film <- paste0(url_base, annee)
  print(url_film)
  
  data_html <- url_film %>% read_html()
  
  # Extraire la table contenant les films, elle s'appelle wikitable
  films_table_html <- data_html %>% html_node("table.wikitable") # Identifier la classe de la table
  
  # Extraire les lignes (rows) du tableau
  rows <- films_table_html %>% html_nodes("tr")
  
  # Extraire les informations des titres et des liens
  films_data_temp <- rows %>%
    lapply(function(row) {
      titre_node <- row %>% html_node("td:nth-child(1) a") # Première colonne avec le titre et le lien
      titre <- titre_node %>% html_text(trim = TRUE)       # Texte du titre
      lien_wikipedia <- titre_node %>% html_attr("href")             # Lien relatif
      
      # Combiner les données
      list(Titre = titre, lien_wikipedia = ifelse(!is.na(lien_wikipedia), paste0("https://fr.wikipedia.org", lien_wikipedia), NA))
    }) %>%
    bind_rows() # Convertir en data frame
  
  # Nettoyer les données pour retirer les lignes vides
  films_data_temp <- films_data_temp %>% filter(!is.na(Titre))
  if (annee == annee_min) {
    films <-films_data_temp
  }
    else{
  # Concaténation avec le tableau global
    films <- bind_rows(films, films_data_temp)
  }
    }

# ***************************************************************

# ---- Récup Liens IMDB et allociné depuis la page du film wikipedia ----
 
# ***************************************************************

films$url_imdb<-NA
films$url_allocine <-NA

#### Recherche des liens IMDB et allocine depuis les pages wikipedia de la table films
for (f in 1:nrow(films)){
for (f in 1:20){
  url <-as.character(films[f,"lien_wikipedia"])
  html_film<-read_html(url)
  print(url)
  #recherche du lien vers IMDB dans la page
  liens_type <- html_film %>% html_nodes("span.liste-horizontale ul li a.external") %>% html_text() 
  #récupération des attributs 
  urls <- html_film %>% html_nodes("span.liste-horizontale ul li a.external") %>% html_attrs() 
  
  ## On cherche dans la liste des types de liens celui de type IMDB et on récupère le href associé dans la variable url
  for (i in seq_along(liens_type)){
    
    type <-liens_type[i]
    if (type=="IMDb") {
      url<-urls[[i]]["href"]
      ## modification de l'url sur IMDB (on garde les 9 derniers caractères et on ajoute la racone)
      url<- paste0("https://www.imdb.com/title/",substr(url, nchar(url) - 8, nchar(url)))
  #    films[f,"url_imdb"] <-url
  #    films[f,"url_imdb_cast"] <-paste0(url,"/fullcredits/?ref_=tt_cst_sm")
      print(url)
      films[f,"url_imdb"]<-url
      
    }
    if (type=="Allociné") {
      url<-urls[[i]]["href"]
      print(url)
      films[f,"url_allocine"]<-url
      
    }
  }
}

summary(data)

html_allocine <- read_html("https://www.allocine.fr/film/fichefilm_gen_cfilm=24536.html")

# Afficher un aperçu des résultats
print(films_data)




film_selector <- "#mw-content-text div ul:nth-of-type(3) li i a"
film_nodes <- data_html %>% html_nodes(film_selector) %>% html_attrs()
films <- tibble()
for(k in seq_along(film_nodes)) {
  film_node <- film_nodes[[k]]
  if("class" %in% names(film_node)) next # Absence de page dédiée
  if(film_node["title"] == "Galadriel") next # Mauvais lien
  films <- rbind(
    films,
    list(titre=film_node["title"], url=film_node["href"])
  )
}