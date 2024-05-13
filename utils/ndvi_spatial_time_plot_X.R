library(rgdal)
library(raster)
library(terra)
library(sp)
library(sf)
library(tidyverse)
library(lubridate)
library(data.table)
library(tabularaster)
library(ggplot2)
library(ggforce)
library(RColorBrewer)
library(xlsx)


#cargar stack
#cargar shape

# smap

smap_kayra <- raster::stack("./animacion_raster/stack/humedad_kayra.grd") 

# shape 

kayra_shape <- sf::st_read("./animacion_raster/shape/kayra.shp")
# kayra_shape <- st_transform(alay, crs(covs)) # no  es necesario cambiar el CRS
kayra_shape_sp <- as(kayra_shape$geometry, "Spatial") # solamente shape, sin fields/campos

# fechas
fecha_modelos_ <- xlsx::read.xlsx("./animacion_raster/fechas.xlsx", sheetName = "Sheet1", rowIndex = TRUE)
fecha_modelos_ <- fecha_modelos_$x # x lleva la fecha en el dataframe

fechas_chr <- fecha_modelos_  %>% as.character() #"2021-05-14" - "2022-07-02"


# Tibble para graficar

dws_tib <- tabularaster::as_tibble(smap_kayra, cell = FALSE, dim = TRUE ,xy = TRUE)  %>%
  na.omit()  %>% dplyr::rename(SM = cellvalue)# stack to dataframe longer

dws_tib$dimindex <- as.factor(dws_tib$dimindex) # dimmindex rerpesetna la fecha de cada raster 
levels(dws_tib$dimindex) <- fechas_chr # cmambiar los niveles 1 , 2 , 3 por las fechas "2021-05-14" "2021-05-15" "2021-05-16"


# PLOT PARA TODAS LA FECHAS EN DIFERENTES PAGINAS
# facet_wrap_paginate

ggplot(dws_tib, aes(x, y)) +
  facet_wrap_paginate(dimindex ~ ., nrow = 5 , ncol = 6 , page = 14) + # pagina para plotear 
  geom_path(data=kayra_shape_sp, aes(long, lat, group=group), color = 'black') + # a?ade la la cuenca a cada plot
  coord_equal() +
  theme_void() +
  geom_raster(aes(fill = SM)) +
  scale_fill_distiller(palette = "Spectral", direction=1) + # direccion 1 mapea azul a valores de humedad mas altos y rojos ahumedades bajas
  
  ggtitle("Humedad del suelo desagregada a 100 metros. Micro-cuenca  K?ayra \n") +
  theme(plot.title = element_text(hjust = 0.5,  size = 11)) 

# animacion

install.packages("animation")
library(animation)

raster::animate(smap_kayra) # no tiene formato

# gganim <- gg + transition_time(as.numeric(year))


library(gganimate)
install.packages("gganimate")
