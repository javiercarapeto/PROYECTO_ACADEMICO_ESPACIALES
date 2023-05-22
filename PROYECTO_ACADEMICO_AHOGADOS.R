################################### AHOGADOS EN ESPAÑA #########################

# Librerías----

library(pacman)
p_load(readxl, readr, osmdata, ggmap, dplyr, ggplot2, colorspace, sf, sp, gstat, spData, spdep, mapSpain, tmap, gamm4) # esta función instala y carga los paquetes que pongamos 


# Dataframes ----
# España con coordenadas por ccaa y por provincias:
spain <- mapSpain::esp_get_ccaa()

# Dataframe primero, que es por CCAA ----
muertes_ccaa <- read_excel("PROYECTO_ACADÉMICO/ahogados_spain/muertes_comunidades_autonomas_excel.xlsx", 
                           col_types = c("text", "numeric", 
                                         "numeric", "numeric", "numeric", "numeric","numeric",
                                         "numeric", "numeric", "numeric", "numeric", "numeric", "numeric",
                                         "numeric", "numeric", "numeric", "numeric", "numeric"))
# Coinciden los nombres
sum(spain$ine.ccaa.name != muertes_ccaa$ccaa)

# Añadimos los datos de muertes_ccaa al mapa de spain, que es por ccaa
spain$anual <- as.numeric(muertes_ccaa$anual)
spain$num_piscinas <- as.numeric(muertes_ccaa$num_piscinas)
spain$habitantes <- as.numeric(muertes_ccaa$habitantes)
spain$pisc_hab <- as.numeric(muertes_ccaa$pisc_hab)
spain$pisc_100hab <- as.numeric(muertes_ccaa$pisc_100hab)
spain$num_rios <- as.numeric(muertes_ccaa$num_rios) 
spain$num_playas <- as.numeric(muertes_ccaa$num_playas)
spain$turistas <- as.numeric(muertes_ccaa$turistas)
spain$muertes_100k_hab <- as.numeric(muertes_ccaa$Ahogados_100mil_hab)
spain$muertes_100k_turistas <- as.numeric(muertes_ccaa$Ahogados_Por_100mil_turist)
spain$horas_sol <- as.numeric(muertes_ccaa$Horas_sol)
spain$esp_acuatico <- as.numeric(muertes_ccaa$Espacio_acuaticos)
spain$tasa_esp_acuatico_100k_hab <- as.numeric(muertes_ccaa$Tasa_esp_acut_100k_hab)
spain$socos <- as.numeric(muertes_ccaa$Socorristas)
spain$socos_por_tasa_esp_acuatico_100k_hab <- as.numeric(muertes_ccaa$Soco_tasa_esp_acuat_100k_hab)
spain$alcohol <- as.numeric(muertes_ccaa$Alcohol_ult_12_meses)
spain$socos_100k_hab <- as.numeric(muertes_ccaa$Socos_100k_hab)

spain$codauto <- as.numeric(spain$codauto)
summary(spain)

# Estos datos sin las Islas ni ciudades autónomas
spain_solo_peninsula <- spain[c(1,2,3,6,7,8,9,10,11,12,13,14,15,16,17),]
summary(spain_solo_peninsula)

# Dataframe de municipios ----
mapa_interactivo <- read_csv("PROYECTO_ACADÉMICO/ahogados_spain/MAPA_INTERACTIVO.csv", 
                             col_types = cols(Piscinas = col_integer(), 
                                              Habitantes = col_integer(), 
                                              `Piscinas por cada 100 habitantes` = col_number()))

spain2 <- mapSpain::esp_get_munic()
mapa_interactivo$name <- gsub("\\s*\\([^\\(\\)]+\\)", "", mapa_interactivo$name)
# En esta expresión regular, \\s* identifica cualquier espacio en blanco antes del paréntesis de apertura. 
# \\( y \\) identifican los paréntesis de apertura y cierre, respectivamente. [^\\(\\)]+ identifica cualquier 
# cadena de caracteres que no contenga paréntesis de apertura o cierre. Finalmente, todo el patrón se envuelve 
# en \\s* para eliminar cualquier espacio en blanco que pueda quedar después de la eliminación de las palabras 
# entre paréntesis.

combinado <- left_join(spain2, mapa_interactivo, by = c("name" = "name"))



# Análisis descriptivo/exploratorio de los datos espaciales ----

# TASA 100k HAB 
plot(density(spain_solo_peninsula$muertes_100k_hab), main ="Gráfico 1. Distribución de los ahogados por 100k habitantes")

summary(spain_solo_peninsula$muertes_100k_hab) 


# TASA 100K TURISTAS ----

plot(density(spain_solo_peninsula$muertes_100k_turistas), main ="Gráfico 2. Distribución de los ahogados por 100k turistas")

summary(spain_solo_peninsula$muertes_100k_turistas) 


# Test de Moran ----
class(spain_solo_peninsula)
(wr <- spdep::poly2nb(spain_solo_peninsula, queen = FALSE)) 
summary(wr) 

xy <- st_centroid(spain_solo_peninsula$geometry) 

plot(spain_solo_peninsula$geometry, main ="Gráfico 3. Número de vecinos de cada Comunidad Autónoma",border="blue")
plot(wr, xy, col="grey", add=TRUE)

wm <- nb2mat(wr, style='B')
dim(wm)

n <- nrow(spain_solo_peninsula)
y <- spain_solo_peninsula$muertes_100k_hab
ybar <- mean(y)


# Método 1
dy <- y - ybar
g <- expand.grid(dy, dy)
yiyj <- g[,1] * g[,2]

# Método 2
yi <- rep(dy, each=n)
yj <- rep(dy, n)
yiyj <- yi * yj

pm <- matrix(yiyj, ncol=n)
round(pm[1:6, 1:9])

pmw <- pm * wm
wm[1:6, 1:9]

# Sumamos los pesos y luego dividiremos
(spmw <- sum(pmw))
smw <- sum(wm)
sw  <- spmw / smw

vr <- n / sum(dy^2)

(MI <- vr * sw)


# Existe una función para calcular el índice de Moran

ww <- nb2listw(wr, style="B")

moran(spain_solo_peninsula$muertes_100k_hab, ww, n=length(ww$neighbours), S0=Szero(ww))

# Test de Moran ----
moran.test(spain_solo_peninsula$muertes_100k_hab, listw=ww)

# Tasa muertes 100k turistas
moran.test(spain_solo_peninsula$muertes_100k_turistas, listw=ww)


# Hotspot/puntos calientes 
# Gráfico de dispersión de Moran
moran.plot(spain_solo_peninsula$muertes_100k_hab, listw=nb2listw(wr, style="B"), col="blue", main = "Gráfico 4. Gráfico de Moran de Ahogados por 100k Habitantes", xlab = "", ylab="valores retardados")
# CCAA atípicas, 9 Cataluña y la 3 Asturias

localmoran(spain_solo_peninsula$muertes_100k_hab, listw = ww)

LISA <- function(x) {
  P = localmoran(spain_solo_peninsula$muertes_100k_hab, listw = ww)
  #head(P) # Se muestran los primeros 6 datos.
  #dim(P) # La dimension de P
  dif = spain_solo_peninsula$muertes_100k_hab - mean(spain_solo_peninsula$muertes_100k_hab)
  lag = lag.listw(ww, spain_solo_peninsula$muertes_100k_hab) # Calcula el retardo (promedios)
  clag = dif - mean(lag) # Retardo - Media(Retardo)
  p = P[,5] # Toma la columna: Pr(z > 0) de P
  
  # Se inicializa vector numerico de longitud filas de P
  quadrant = vector(mode="numeric",length=nrow(P))+5
  quadrant[dif>0 & clag>0 & p<= 0.05] = 1 # Alto-Alto
  quadrant[dif<0 & clag<0 & p<= 0.05] = 2 # Bajo-Bajo
  quadrant[dif<0 & clag>0 & p<= 0.05] = 3 # Bajo-Alto
  quadrant[dif>0 & clag<0 & p<= 0.05] = 4 # Alto-Bajo
  
  # Grafico  
  brks = c(1,2,3,4,5)
  colors = c("red", "blue", "light blue", "pink", "white")
  plot(spain_solo_peninsula$geometry, border ="lightgray", col=colors[findInterval(quadrant,brks,all.inside=FALSE)])
  legend("bottomright", legend = c("High-High", "Low-Low", "Low-High", "High-Low", "Insignificant"),fill = colors, bty="n", cex=0.7, y.intersp=1, x.intersp=1)
  box()
  title("LISA Cluster Map")
}

LISA(ww)
# No se ve nada

# Tasa 100k turistas
moran.plot(spain_solo_peninsula$muertes_100k_turistas, listw=nb2listw(wr, style="B"), col="blue", main = "Gráfico 5. Gráfico de Moran de Ahogados por 100k Turistas",xlab = "",ylab="valores retardados")
# 9 Cataluña, 3 Asturias y 1 Andalucía
localmoran(spain_solo_peninsula$muertes_100k_turistas, listw = ww)

LISA <- function(x) {
  P = localmoran(spain_solo_peninsula$muertes_100k_turistas, listw = ww)
  #head(P) # Se muestran los primeros 6 datos.
  #dim(P) # La dimension de P
  dif = spain_solo_peninsula$muertes_100k_turistas - mean(spain_solo_peninsula$muertes_100k_turistas)
  lag = lag.listw(ww, spain_solo_peninsula$muertes_100k_turistas) # Calcula el retardo (promedios)
  clag = dif - mean(lag) # Retardo - Media(Retardo)
  p = P[,5] # Toma la columna: Pr(z > 0) de P
  
  # Se inicializa vector numerico de longitud filas de P
  quadrant = vector(mode="numeric",length=nrow(P))+5
  quadrant[dif>0 & clag>0 & p<= 0.05] = 1 # Alto-Alto
  quadrant[dif<0 & clag<0 & p<= 0.05] = 2 # Bajo-Bajo
  quadrant[dif<0 & clag>0 & p<= 0.05] = 3 # Bajo-Alto
  quadrant[dif>0 & clag<0 & p<= 0.05] = 4 # Alto-Bajo
  
  # Grafico  
  brks = c(1,2,3,4,5)
  colors = c("red", "blue", "light blue", "pink", "white")
  plot(spain_solo_peninsula$geometry, border ="lightgray", col=colors[findInterval(quadrant,brks,all.inside=FALSE)])
  legend("bottomright", legend = c("High-High", "Low-Low", "Low-High", "High-Low", "Insignificant"),fill = colors, bty="n", cex=0.7, y.intersp=1, x.intersp=1)
  box()
  title("LISA Cluster Map")
}

LISA(ww)




# GAM TASA 100K HAB ----

adj_list <- st_touches(spain_solo_peninsula, spain_solo_peninsula)
names(adj_list) <- as.factor(spain_solo_peninsula$ccaa.shortname.es)

ahog_model <- gamm4(formula = muertes_100k_hab ~ horas_sol + tasa_esp_acuatico_100k_hab + 
                      socos_por_tasa_esp_acuatico_100k_hab + alcohol + s(as.factor(ccaa.shortname.es), 
                                                                         xt=list(nb=adj_list), bs='mrf'), 
                    data=spain_solo_peninsula, REML = T)

summary(ahog_model$gam)


ahog_model <- gamm4(formula = muertes_100k_hab ~ horas_sol + tasa_esp_acuatico_100k_hab + 
                      + alcohol + s(as.factor(ccaa.shortname.es), 
                                    xt=list(nb=adj_list), bs='mrf'), data=spain_solo_peninsula, REML = T)

summary(ahog_model$gam)


spain_solo_peninsula$fitted <- predict(ahog_model$gam)

moran.test(spain_solo_peninsula$fitted,list=ww)

spain_solo_peninsula$residuos_anual <- spain_solo_peninsula$muertes_100k_hab - spain_solo_peninsula$fitted


ahog_model <- gamm4(formula = muertes_100k_hab ~ residuos_anual + 
                      s(as.factor(ccaa.shortname.es), xt=list(nb=adj_list), bs='mrf'), 
                    data=spain_solo_peninsula, REML = T)
summary(ahog_model$gam)


plot(density(spain_solo_peninsula$residuos_anual))


ggplot(spain_solo_peninsula, aes(fill=fitted)) + geom_sf(lwd=0.2) +
  scale_fill_viridis_c(name='Valores\nPropios',direction=-1) +
  labs(title = paste(
    "Gráfico 6. Mapa de España para los valores ajustados"),
    subtitle = paste("Tasa de ahogados por cada 100 mil habitantes"),
    caption = "Fuente: Elaboración propia")

# Comparar este gráfico con el que tendría que dar
ggplot(spain_solo_peninsula, aes(fill=muertes_100k_hab)) + geom_sf(lwd=0.2) +
  scale_fill_viridis_c(name='Tasa\nAhogados',direction=-1) +
  labs(title = paste(
    "Gráfico 7. Mapa de España"),
    subtitle = paste("Tasa de ahogados por cada 100 mil habitantes"),
    caption = "Fuente: Elaboración propia")




# GAM 100k TURISTAS -----

ahog_model2 <- gamm4(formula = muertes_100k_turistas ~ horas_sol + tasa_esp_acuatico_100k_hab + 
                       socos_por_tasa_esp_acuatico_100k_hab + alcohol + s(as.factor(ccaa.shortname.es), 
                                                                          xt=list(nb=adj_list), bs='mrf'), 
                     data=spain_solo_peninsula, REML = T)
summary(ahog_model2$gam) 


# Eliminamos alcohol
ahog_model2 <- gamm4(formula = muertes_100k_turistas ~ horas_sol + tasa_esp_acuatico_100k_hab + 
                       socos_por_tasa_esp_acuatico_100k_hab + s(as.factor(ccaa.shortname.es), 
                                                                xt=list(nb=adj_list), bs='mrf'), 
                     data=spain_solo_peninsula, REML = T)
summary(ahog_model2$gam)


spain_solo_peninsula$fitted2 <- predict(ahog_model2$gam)
spain_solo_peninsula$residuos_anual <- spain_solo_peninsula$muertes_100k_turistas - spain_solo_peninsula$fitted2

moran.test(spain_solo_peninsula$residuos_anual,list=ww)


ahog_model2 <- gamm4(formula = muertes_100k_turistas ~ residuos_anual + 
                       s(as.factor(ccaa.shortname.es), xt=list(nb=adj_list), bs='mrf'), 
                     data=spain_solo_peninsula, REML = T)
summary(ahog_model2$gam)

plot(density(spain_solo_peninsula$residuos_anual))
ggplot(spain_solo_peninsula, aes(fill=fitted)) + geom_sf(lwd=0.2) +
  scale_fill_viridis_c(name='Valores\nAjustados',direction=-1) +
  labs(title = paste(
    "Gráfico 8. Mapa de España para los valores ajustados"),
    subtitle = paste("Tasa de ahogados por cada 100 mil turistas"),
    caption = "Fuente: Elaboración propia")

# Comparar este gráfico con el que tendría que dar
ggplot(spain_solo_peninsula, aes(fill=muertes_100k_turistas)) + geom_sf(lwd=0.2) +
  scale_fill_viridis_c(name='Tasa\nAhogados',direction=-1) +
  labs(title = paste(
    "Gráfico 9. Mapa de España"),
    subtitle = paste("Tasa de ahogados por cada 100 mil turistas"),
    caption = "Fuente: Elaboración propia")



# Mapas extra ----
theme_custom_map <- function(base_size = 11,
                             base_family = "",
                             base_line_size = base_size / 22,
                             base_rect_size = base_size / 22) {
  theme_bw(base_size = base_size, 
           base_family = base_family,
           base_line_size = base_line_size) %+replace%
    theme(
      axis.title = element_blank(), 
      axis.text = element_blank(),
      axis.ticks = element_blank(),
      complete = TRUE
    )
}

# PORTADA ----
# Ahogados en general
mapa <- spain %>% ggplot() +
  aes(fill= anual)+
  geom_sf()+
  labs(title = paste(
    "                  AHOGADOS EN ESPAÑA EN 2022"),
    caption = "Fuente: Elaboracción propia")+
  theme_custom_map()

mapa + scale_fill_gradientn(colours = rev(grDevices::heat.colors(10)), name = NULL)


# G10 ----
options(scipen = 999)
# tasa esp acuaticos
mapa <- spain_solo_peninsula %>% ggplot() +
  aes(fill= tasa_esp_acuatico_100k_hab)+
  geom_sf()+
  labs(title = paste(
    "Gráfico 10. Piscinas en España en 2022"),
    caption = "Fuente: Elaboracción propia")+
  theme_custom_map()

colores <- c("#296AB8", "#3A83C6", "#4D9BD3", "#60B4E1", "#73CCEF", "#86E5FD")


mapa + scale_fill_gradientn(colours = rev(colores), name = NULL)

# G11 ----
mapa <- spain_solo_peninsula %>% ggplot() +
  aes(fill= socos_por_tasa_esp_acuatico_100k_hab)+
  geom_sf()+
  labs(title = paste(
    "Gráfico 11. Tasa de socorristas en España en 2014"),
    caption = "Fuente: Elaboracción propia")+
  theme_custom_map()
mapa + scale_fill_gradientn(colours = rev(grDevices::heat.colors(10)), name = NULL)



# El color del mapa
base_color <- "#FDE90D"
# Otros colores
color_points <- c(
  lighten(base_color, 0.5),
  base_color,
  darken(base_color, 0.8)
)
color_palette <- colorRampPalette(color_points)
colors_map <- color_palette(10)


# MAPA INTERACTIVO ----

sysfonts::font_add_google("Montserrat", "Montserrat")

# Usar showtext para familias topográficas
showtext::showtext_auto()
# Colores
col_spec <- RColorBrewer::brewer.pal(11, "Spectral")

# Función de una gama de colores
col_spec_fun <- colorRampPalette(col_spec)

m <- tm_shape(spain_solo_peninsula) +
  tm_polygons("horas_sol",
              border.col = "transparent",
              palette = colors_map,
              textNA = "Sin datos",
              title = "Horas de sol")
# Mapa dinámico
tmap_leaflet(m)



# Piscinas por municipios ----
mapa <- combinado %>% ggplot() +
  aes(fill= Piscinas)+
  geom_sf()+
  labs(title = paste(
    "Gráfico 12. Piscinas por municipios en España en 2022"),
    caption = "Fuente: Elaboración propia a partir de datos del Catastro")
mapa

colors_map <- color_palette(10)
m <- tm_shape(combinado) +
  tm_polygons("Piscinas por municipios",
              border.col = "transparent",
              textNA = "Sin datos",
              title = "Piscinas")
# Mapa dinámico
tmap_leaflet(m)