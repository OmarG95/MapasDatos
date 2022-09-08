##Borrar datos del entorno
rm(list=ls())
#Directorio de trabajo
library(pacman)
p_load(tidyverse, lubridate, scales, RPostgreSQL,stringr, rgdal,
       scales, yaml, janitor, ggrepel, tidyquant, cowplot, rpostgis, 
       gt,googlesheets4,readxl,stringi, xlsx)

#Esto se debe de cambiar en cada computadora
#Crear carpeta general de trabajo en unidad D

setwd("C:/Users/Omar González/Documents/ADIP/Participación/Ferias Bienestar")


alcaldias <- st_read("G:/.shortcut-targets-by-id/1o3RW9XtzyaaBxBvSYFPTHgER_wJ6O_VR/Analisis/Gabinete de Seguridad/200130_5_delitos/5delitos_nueva/06_mapa/Presentacion_domingos/limite_de_las_alcaldias/limite_de_las_alcaldias.shp")
colonias_480<-read.csv("ferias_seleccionadas_vfinal.csv", encoding="latin1")%>%
  rename(CVEUT=CLAVE.COLONIA)%>%
  select(2,3,5,7:8)

colonias<-st_read("C:/Users/Omar González/Documents/ADIP/SHP/COLONIAS_IECM/mgpc_2019.shp")%>%
  st_as_sf()%>%
  st_transform("+init=epsg:4326")%>%
  select(5,6)

colonias_ferias<-colonias_480%>%
  left_join(colonias)%>%
  mutate(prioritaria=case_when(prioritaria==1~"Prioritaria",
                               T~"No prioritaria"))%>%
  st_as_sf()%>%
  st_transform("+init=epsg:4326")


mapa_ferias<-leaflet() %>%
  addTiles()%>%
  setView( -99.15, 19.3, 10, zoom = 10 )%>%
  addPolygons(data=alcaldias, weight = 2,color="black",
              fillColor = "transparent")%>%
  # #Propuestas
  # addPolygons(data =propuesta, weight = 2, color = 'red',
  #             group="Colonias propuestas",
  #             stroke = T,
  #             popup = paste("<b>","Información de la colonia", "</b>", "<br>",
  #                           "<b>","Clave colonia: ","</b>",propuesta$cveut,"<br>",
  #                           "<b>","Colonia: ","</b>",propuesta$nomut,"<br>",
  #                           "<b>","Alcaldía: ","</b>",propuesta$nomdt,"<br>",
  #                           "<b>","Grado de marginación: ","</b>" ,propuesta$ids,"<br>",
  #                           "<b>","Población: ","</b>" ,propuesta$poblacion_total2020,"<br>")
  #             
#             
# 
# )%>%
#Marginadas
addPolygons(data =colonias_ferias, weight = 1, color = 'black',fillColor = "darkblue",
            group="Colonias seleccionadas",
            stroke = T,
            popup = paste("<b>","Información de la colonia", "</b>", "<br>",
                          "<b>","Clave colonia: ","</b>",colonias_ferias$CVEUT,"<br>",
                          "<b>","Colonia: ","</b>",colonias_ferias$NOMUT,"<br>",
                          "<b>","Alcaldía: ","</b>",colonias_ferias$ALCALDIA,"<br>",
                          "<b>","Índice de desarrollo social: ","</b>" ,colonias_ferias$clasif_IDS2020,"<br>",
                          "<b>","Prioritaria: ","</b>" ,colonias_ferias$prioritaria,"<br>",
                          "<b>","Ferias de bienestar previas: ","</b>" ,colonias_ferias$ferias,"<br>")
            
            
)%>%
  #Estilo
  addProviderTiles("CartoDB.Positron",
                   group = "CartoDB.Positron") %>%
  addProviderTiles("OpenStreetMap",
                   group = "OpenStreetMap") %>%
  addProviderTiles("Esri.WorldImagery",
                   group = "Esri.WorldImagery")%>%   
  #Controles
  addLayersControl(
    baseGroups = c("CartoDB.Positron","OpenStreetMap", "Esri.WorldImagery"),
    overlayGroups = c("Colonias seleccionadas"),
    options = layersControlOptions(collapsed = F),
    position = 'bottomleft')%>%
  #Desactivar
  #hideGroup(c("Cacdis","Pilares","Buffer 500 metros","Buffer 1 km"))%>%
  leaflet::addLegend(title = "Colonias", position = c("topright"), 
                     colors =  c("darkblue" ),
                     labels = c( "Colonias seleccionadas"))
htmlwidgets::saveWidget(mapa_ferias, file="C:/Users/Omar González/Documents/ADIP/Participación/Ferias Bienestar/mapa_480colonias.html")
