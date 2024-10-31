###########################################
########## SPATIAL ANALYSYS IN R ##########
###########################################

##############
## Sesion 3 ##
##############
## Home Range ##

rm(list=ls()) # Elimina todos los objetos creados previamente en el entorno de trabajo de R.
gc() # Liberar memoria no utilizada.

setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) # Establece el directorio de trabajo en el que se encuentra el script actual.
getwd() # Muestra el directorio de trabajo actual para confirmarlo.

# Lista de librerías necesarias
libraries <- c("reshape2", "ggplot2", "lubridate", "dplyr", "sf", 
               "maps", "tidyr", "adehabitatHR", "leaflet", 
               "ggmap", "rnaturalearth", "rnaturalearthhires", 
               'RColorBrewer', 'zoo','purrr', 'viridis','gridExtra', 'grid', 'ggridges', 'plotmo')

remotes::install_github("ropensci/rnaturalearthhires")

install.packages("rnaturalearthhires", repos = "https://ropensci.r-universe.dev", type = "source")

# Función para instalar y cargar librerías
install_and_load <- function(libs) {
  for (lib in libs) {
    if (!require(lib, character.only = TRUE)) { # Verifica si la librería ya está instalada
      install.packages(lib, dependencies = TRUE) # Si no está instalada, la instala
      library(lib, character.only = TRUE) # Carga la librería una vez instalada
    } else {
      library(lib, character.only = TRUE) # Si ya está instalada, solo la carga
    }
  }
}

# Ejecuta la función para instalar y cargar las librerías
install_and_load(libraries)


#Carga tus datos
data <- read.csv("data_session_03_practica.csv", sep=';')  # Leer el archivo CSV separado por punto y coma
names(data)  # Mostrar nombres de columnas
summary(data)  # Resumen de datos
str(data)  # Estructura de los datos
dev.off()  # Cerrar cualquier dispositivo gráfico abierto

# Visualizar puntos de ubicación
# Crear el gráfico usando ggplot
ggplot(data, aes(x = Longitude, y = Latitude)) +
  geom_point() +
  theme_minimal() +
  coord_fixed()

# Cargar los datos del mundo en alta resolución con el paquete rnaturalearth
world = ne_countries(scale = "large", returnclass = "sf")



# Crear el gráfico con el mundo de fondo en la localización de las aves
ggplot(world) +
  geom_sf() +
  geom_point(data = data, aes(x = Longitude, y = Latitude), color = "red") +
  theme_minimal() +
  coord_sf()

##########################################
##### Mínimo polígono convexo (MCP) ######
##########################################

# Usa el paquete adehabitatHR
library(adehabitatHR)

data_sf <- st_as_sf(data, coords = c("Longitude", "Latitude"), crs = 4326)
# Calcula las coordenadas en UTM
data_sf <- st_transform(data_sf, crs = 32630)
# Crea un SpatialPoints usando las coordenads UTM
data$UTMX <- st_coordinates(data_sf)[,1]
data$UTMY <- st_coordinates(data_sf)[,2]
names(data)
# Asignar el sistema de referencia de coordenadas (CRS)
proj4string(data) <- CRS("+proj=utm +zone=30 +ellps=WGS84 +datum=WGS84 +units=m +no_defs")
# Plotealo
plot(data)

par(mfrow = c(1, 2))
plot(data$Longitude, data$Latitude)
plot(data$UTMX, data$UTMY)

# Reset the plotting layout to a single plot per figure
par(mfrow = c(1, 1))
# Close any open graphics devices
dev.off()

# Crea un MCP con el 95%
data_sp <- SpatialPoints(data[,c("UTMX", "UTMY")])
mcp <- mcp(data_sp, percent = 95)
# Plotealo con lo puntos sobre un mapa simple
plot(data_sp$UTMX, data_sp$UTMY, add=FALSE)
plot(mcp, add=TRUE, col="red", alpha = 0.5)

ggplot() +
  geom_sf(data = st_as_sf(mcp), fill = "red", alpha = 0.5) +
  geom_point(data = data, aes(x = Longitude, y = Latitude), color = "blue") +
  theme_minimal() +
  coord_sf()
# ¿Cuanto mide el MCP?
mcp
# Guardalo como jpg
# Guarda ese polígono a shapefile 


####################################
##### Utilisation Distribution #####
#### Kernel Denstity Estimation ####
####################################


# Veíamos en el ppt que estos análisis están condicionado por el número de localizaciones de los animales en un determinado tiempo.
# Por lo tanto tenemos que ver si todos los animales de la población tienen un número similar de localizaciones.
# Mira cuantas localizaciones tienes por día (puedes usar dcast, que es un pivot table)
# Mira cuantas localizaciones tienes por hora (puedes usar dcast, que es un pivot table)
# Ojo, convierte el datetime en formato correcto
# Veíamos que no hay el mismo número de localizaciones por día ni por hora.
# Vamos a crear un dato cada 30 min
# Paso 1: Crear columna con intervalos redondeados a los 30 minutos anteriores.
# Paso 2: Calcular promedios de latitud y longitud por intervalo de 30 minutos.
# Paso 3: Generar una secuencia de todos los intervalos de 30 minutos dentro del rango temporal.
# Paso 4: Crear un grid con todas las combinaciones posibles de ID e intervalos de tiempo.
# Paso 5: Unir los datos promediados con los tiempos completos agregando las coordenadas promedio donde existan datos.
# Paso 6: Ordenar los datos por ID, fecha y tiempo para aplicar la interpolación de forma ordenada.
# Paso 7: Crear función de interpolación para manejar los NA, es decir, donde no hay lat y lon
# Paso 8: Aplicar la interpolación a las columnas de latitud y longitud para cada ID
# Ojo, tenemos NAs!!!
# Estos NAs los tenemos en aquellas lat y lon donde ya no hay datos de algunos emisores.
# Eliminar filas donde Latitude o Longitude son NA


####################################
####### Analisis de densidad #######
####################################

# Cálcula el KEY AREA PARA TODA LA POBLACIÓN Y PARA CADA INDIVIDUO. 
# Con los datos originales y con los datos interpolados

# Bonus (si hay tiempo,  hacedlo para cada ID)


