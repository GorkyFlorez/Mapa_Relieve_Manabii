library(sf)
library(ggplot2)
library(tidyverse)
library(raster)
library(ggspatial)
library(extrafont)
library(ggnewscale)
Elev <- raster("Raster_Manabi.tif")
Ecuador     <- getData('GADM', country='Ecuador', level=0) %>% st_as_sf()
Ecuado   <- getData('GADM', country='Ecuador', level=1) %>% st_as_sf()
Ecuad   <- getData('GADM', country='Ecuador', level=2) %>% st_as_sf()

Manabi      <- subset(Ecuad , NAME_1 == "Manabi")
Bolia_xy <- cbind(Manabi , st_coordinates(st_centroid(Manabi$geometry)))
slope = terrain(Elev  , opt = "slope") 
aspect = terrain(Elev , opt = "aspect")
hill = hillShade(slope, aspect, angle = 40, direction = 270)

hill.pa        <-  rasterToPoints(hill)
hill.pa_a      <-  data.frame(hill.pa)
dem.p          <-  rasterToPoints(Elev)
df             <-  data.frame(dem.p)


Mapa =ggplot()+
  geom_raster(data = hill.pa_a, aes(x,y, fill = layer), show.legend = F)+
  scale_fill_gradientn(colours=grey(1:100/100))+
  geom_sf(data = Manabi, fill=NA, color="black", size=0.2)+
  coord_sf(xlim = c(-81.08492 ,-79.40501), ylim = c(-1.947404  ,0.3818064),expand = FALSE)+
  theme_bw()+
  theme(axis.text.x  = element_text(face="bold", color="black", size=8),
        axis.text.y  = element_text(angle = 90,face="bold", color="black", size=8))+
  labs(x = NULL, y = NULL)+
  geom_label(data =  Bolia_xy , aes(x= X, y=Y, label = NAME_2), size = 2.5, color="black", fontface = "bold",fontfamily = "serif", alpha=0.3)+
  annotation_north_arrow(location="tl",which_north="true",style=north_arrow_fancy_orienteering ())+
  ggspatial::annotation_scale(location = "bl",bar_cols = c("grey60", "white"), text_family = "ArcherPro Book")+
  annotate(geom = "text", x = -80.4, y =-1.9, hjust = 0, vjust = 0, lineheight = .9,
           label = "Author: Gorky Florez (@gflorezc) Original Idea: Aprende R desde cero, Geometries: RStudio Data: ING-Peru, 2022;",
           size = 3, family = "serif", color = "grey50")+
  # title
  annotate(geom = "text", x = -81.05, y = -0.1, hjust = 0, vjust = 1, 
           label = "Manabi:  Shaded height \n     and relief map",
           size = 8, family="serif", color = "grey20")+
  annotate(geom = "text", x = -80, y = -1.8, hjust = 1, vjust = 0,
           label = "Codigo en Githab", fontface = 2,
           size = 3, family = "serif", color = "grey20")+
  annotate(geom = "text", x = -79.5, y = -1.85, hjust = 1, vjust = 0,
           label = "https://github.com/GorkyFlorez/Mapa_Relieve_Manabi",
           size = 3, family = "serif", color = "#35978f")+
  # date
  annotate(geom = "text", x = -81, y = -1.7, hjust = 0, vjust = 0,
           label = "2022", fontface = 2,
           size = 5, family = "serif", color = "#35978f")
ggsave("Mapas/Manabi.png", Mapa, width = 9, height = 11.76, 
       dpi = 900, type = "cairo-png")

colss <-c("#41641F","#6B9F3A","#EDAE5F","#955235","#9F3B03","#843907","#471600","#4E1C05","#50362F", "#391306")

library(tmap)

Mapa_tmap=tm_shape(hill,ylim=c(-1.947404  ,0.3818064),xlim=c(-81.08492 ,-79.40501)) +
  tm_raster(palette = gray(0:10 / 10), n = 100, legend.show = FALSE, alpha=0.7)+
  tm_shape(Elev) +
  tm_raster(alpha = 0.6, palette = colss ,n=4, style="cont",
            legend.show = T, title="Elevacion \n(m.s.n.m)")+
  tm_shape(Manabi)+
  tm_borders(col = "black")+
  tm_text("NAME_2",size = .6, col="black",shadow=TRUE,
          bg.color="white", bg.alpha=.3)+
  tm_scale_bar(width = 0.25, text.size = 0.5, text.color = "black", color.dark = "lightsteelblue4", 
               position = c(.01, 0.005), lwd = 1, color.light= "white")+
  tm_compass(type="8star", position=c(.01, 0.90), text.color = "black")+
  tm_layout( bg.color="grey75", 
            legend.title.size=.8,
            legend.position = c(.90, .005) ,
            fontface="bold",
            legend.format = c(text.align = "right", 
                              text.separator = "-"))+
  tm_credits("Manabi:  Shaded height \n     and relief map", position = c(.1, .8), col = "black", fontface="bold", size=2, fontfamily = "serif")+
  tm_grid(col = "grey",ticks = T, labels.col = "black")+
  tm_credits("Data: DEM SRTM \n#Aprende R desde Cero Para SIG \nGorky Florez Castillo", position = c(0.4, .01), col = "black", fontface="bold")+
  tm_logo(c("https://www.r-project.org/logo/Rlogo.png",
            system.file("img/tmap.png", package = "tmap")),height = 3, position = c(0.60, 0.05))

tmap_save(Mapa_tmap, "Mapas/Mapa_tmap_Manabi.png", dpi = 1200, width = 9, height = 11.76)

