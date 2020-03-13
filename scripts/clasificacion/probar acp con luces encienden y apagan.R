# Probar con análisis de componentes otro ejercicio
recorders = data.frame("X"=c(0,0,1,1), "Y" = c(0,1,1,0),
                       row.names=c("A", "B","C","D"))

recorders

locs = data.frame("X"=c(.3,.5),"Y"=c(.8,.2))
intensities = data.frame("sine"=sin(0:99*(pi/10))+1.2,
                         "cosine"= .7*cos(0:99*(pi/15))+.9)

dists = matrix(nrow=dim(locs)[1], ncol=dim(recorders)[1],
               dimnames=list(NULL, row.names(recorders)))
for (i in 1:dim(dists)[2]){
  dists[,i]=sqrt((locs$X-recorders$X[i])^2
                 + (locs$Y-recorders$Y[i])^2)}
set.seed(500)
recorded.data = data.frame(jitter(as.matrix(intensities)%*%
                                    as.matrix(exp(-2*dists)),amount=0))

plot(recorded.data)

round(cor(recorded.data),2)

plot.ts(recorded.data)


# Otro ejemplo porque no entiendo mucho hasta aqu[i.
install.packages("tidiverse")
install.packages("psych")

library(tidyverse)
library(psych)


dc_marvel <-
  read_csv("data/superheores/superhero-set/heroes_information.csv") %>%
  select(name, Publisher) %>%
  filter(Publisher %in% c("DC Comics", "Marvel Comics"))


heroe_poderes <- read_csv("data/superheores/superhero-set/super_hero_powers.csv")

heroe_poderes <-
  heroe_poderes %>%
  filter(hero_names %in% dc_marvel$name)


# Quitamos espcios y caracteres raros en nombres y reemplzamos por guines bajos
names(heroe_poderes) <-
  names(heroe_poderes) %>%
  tolower() %>%
  gsub("\\W+", "_", .)


# Creamos dos data frames diferentes, una con los nombres de los personajes y otra con los poderes.
# Personajes
# Personajes
heroe <- select(heroe_poderes, hero_names)
# Poderes
poderes <- select(heroe_poderes, -hero_names)

# Modificamos los valores TRUE yFALSE 1 y 0
poderes <-
  map_df(poderes, ~ifelse(. == "TRUE", 1, 0))

# CUANTOS superheroes TIENE grito sónic
sum(poderes$sonic_scream)

# CUANTOS superherores TIENEn agilidad
sum(poderes$agility)


# omitimos estos poderes al realizar PCA obtendremos mejores resultados, pues los este método es sensible a valores extremos.
index_poderes <- map_dbl(poderes, sum)


# ver la distribución de los poderes
plot(density(index_poderes), main = "Frecuencia de poderes")

# Ahora veamos los cinco poderes más y menos comunes, con ayuda de sort() y head().
# Más comunes
head(sort(index_poderes, decreasing = TRUE), 5)

# Menos comunes
head(sort(index_poderes), 5)


# quinar poderes que nadie tiene
poderes <- poderes[(index_poderes > 4 & index_poderes < 150)]
# Tamaño nuevo de poderes
dim(poderes)

# Gráfico nuevo de poderes
map_dbl(poderes, sum) %>% 
  density() %>% 
  plot(main = "Frecuencia de poderes")


# También quitaremos a los personajes que no tiene ningún poder y a aquellos que tienen un número muy alto de ellos. Estos datos también pueden ser considerados como outliers.
index_heroe <- rowSums(poderes)
# Resultado
index_heroe

# Demos un vistazo a cómo se distribuyen estos valores.
plot(density(index_heroe), "Distribución de héroe")


heroe <- heroe[index_heroe > 0 & index_heroe < 30, ]
# Tambien lo tenemos que hacer con poderes para que coincidan los tamaños de las tablas
poderes <- poderes[index_heroe > 0 & index_heroe < 30, ]
# Nueva distribución
rowSums(poderes) %>% 
  density() %>% 
  plot(main = "Distribución de héroe - Nuevo")


# Ya estamos listos para para realizar PCA.
# Análisis de Componentes Principales (PCA)

#  Llamamos a VSS usando la función vss() de psych.
poderes_vss <- vss(poderes)
# Nuestro resultado
poderes_vss

# Ejecutamos el PCA
#Usamos la función pca() de psych, aplicada a nuestro objeto poderes y con el argumento nfactors = 8 para ejecutar este procedimiento.
poderes_pca <- pca(r = poderes, nfactors = 8)

poderes_pca

# Generamos cor correlation
cor(poderes_pca$weights) %>% round(2)


# Ver cargas

poderes_loadings <-
  poderes_pca$weights %>%
  data.frame() %>%
  rownames_to_column("poder") %>%
  tbl_df()
# Nuestro resultado
poderes_loadings


# Para ilustrar esto, veamos los ocho componentes con los poderes 
# que tienen las cargas más altas con ellos. 
# Deberíamos observar poderes relacionados entre sí.
names(poderes_loadings[-1]) %>%
  map(function(x){
    poderes_loadings %>%
      select(poder, factor = x) %>%
      arrange(desc(factor))
  })


# Guardemos de una vez un vector con los nombres posibles para cada uno de estos componentes y los asignamos 
# al objeto poderes_loading.
poderes_nombres <- c("super_ojos", "divino", "psiquico", 
                     "spider_man", "energia", "ladrillo", "vigilante", "Otro")
# Asignamos nombres
names(poderes_loadings) <- c("poder", poderes_nombres)


# Puntuaciones de cada componente
poderes_scores <-
  ((poderes_pca$scores * 100) + 500) %>%
  tbl_df() %>%
  bind_cols(heroe, .)
# Asignación de nombre
names(poderes_scores) <- c("heroe", poderes_nombres)

# Resultado
poderes_scores



# De manera similar a como lo hicimos con los las cargas, 
# podemos ver los personajes con las puntuaciones más altas 
# en cada uno de los componentes usando sus nombres.
#
names(poderes_scores[-1]) %>%
  map(function(x){
    poderes_scores %>%
      select(heroe, Score = x) %>%
      arrange(desc(Score))
  }) %>% {
    names(.) <- poderes_nombres
    .
  }


# Cambiar poderes con nombres mas populares
poderes_nombres <- c("superman", "omnipotente", "psiquico", 
                     "spiderman", "animal", "energia", "titan", "vigilante")
# Renombramos las columnas de poderes_scores
names(poderes_scores) <- c("heroe", poderes_nombres)




# Ver a Colos
filter(poderes_scores, heroe == "Colossus")

filter(poderes_scores, heroe == "Hulk")



# Una función para saber a que grupo pertenece esta personaje
obten_tipo <- function(nombre, cuantos = 3) {
  poderes_scores %>% 
    filter(heroe == nombre) %>% 
    gather(componente, score, superman:vigilante) %>% 
    arrange(desc(score)) %>% 
    top_n(wt = score, n = cuantos) %>% 
    mutate(diferencia = score - first(score))
}


# ejemplo
c("X-23", "Punisher", "Stargirl", "Swamp Thing", "Shazan") %>% 
  map(obten_tipo)
