library(lubridate)
library(tidyverse)
library(RColorBrewer)
library(cowplot)
library(viridis)
library(extrafont)

#import data
trends_recetas <- read_csv("trends_recetas.csv")

#estilo tema
theme_strip <- theme_minimal()+
  theme(axis.text.y = element_blank(),
        axis.line.y = element_blank(),
        axis.title = element_blank(),
        panel.grid.major = element_blank(),
        legend.title = element_blank(),
        axis.text.x = element_text(vjust = 3,
                                   color = "#A5A5A5"),
        panel.grid.minor = element_blank(),
        plot.title = element_text(size = 16, face = "bold",
                                  color = "#767676",
                                  family = "Leelawadee UI Semilight")
  )

pal <- wes_palette("Zissou1", 100, type = "continuous")


#pan
pan_casero <- trends_recetas %>% 
  mutate(pan = ifelse(as.character(pan_casero) == "< 1", "0", as.character(pan_casero))) %>% 
  select(semana, pan)

pan_casero$pan <- as.numeric(pan_casero$pan)
pan_casero$pan[is.na(pan_casero$pan)] <- 0


#plot pan
plot1 <- ggplot(pan_casero,
                aes(x = semana, y = 1, fill = pan))+
  geom_tile()+
  scale_x_date(date_labels = "%b", date_breaks = "1 month", expand = c(0, 0))+
  scale_y_continuous(expand = c(0, 0))+
  scale_fill_gradientn(colors = pal) +
  guides(fill = guide_colorbar(barwidth = 10, barheight = 1))+
  labs(title = '"P A N  C A S E R O"')+
  theme_strip +
  theme(legend.position = "top")
plot1



#churros
churros_caseros <- trends_recetas %>% 
  mutate(churros = ifelse(as.character(churros) == "< 1", "0", as.character(churros))) %>% 
  select(semana, churros)

churros_caseros$churros <- as.numeric(churros_caseros$churros)
churros_caseros$churros[is.na(churros_caseros$churros)] <- 0


#plot churros
plot2 <- ggplot(churros_caseros,
                aes(x = semana, y = 1, fill = churros))+
  geom_tile()+
  scale_x_date(date_labels = "%b", date_breaks = "1 month", expand = c(0, 0))+
  scale_y_continuous(expand = c(0, 0))+
  scale_fill_gradientn(colors = pal) +
  guides(fill = guide_colorbar(barwidth = 10, barheight = 1))+
  labs(title = '"C H U R R O S  C A S E R O S"')+
  theme_strip +
  theme(legend.position = "none")
plot2


#donuts

donuts_caseros <- trends_recetas %>% 
mutate(donuts = ifelse(as.character(donuts_caseros) == "< 1", "0", as.character(donuts_caseros))) %>% 
  select(semana, donuts)

donuts_caseros$donuts <- as.numeric(donuts_caseros$donuts)
donuts_caseros$donuts[is.na(donuts_caseros$donuts)] <- 0


#plot churros
plot3 <- ggplot(donuts_caseros,
                aes(x = semana, y = 1, fill = donuts))+
  geom_tile()+
  scale_x_date(date_labels = "%b", date_breaks = "1 month", expand = c(0, 0))+
  scale_y_continuous(expand = c(0, 0))+
  scale_fill_gradientn(colors = pal) +
  guides(fill = guide_colorbar(barwidth = 10, barheight = 1))+
  labs(title = '"D O N U T S  C A S E R O S"')+
  theme_strip +
  theme(legend.position = "none")
plot3

#masa madre

masamadre <- trends_recetas %>% 
  mutate(masa = ifelse(as.character(masa_madre) == "< 1", "0", as.character(masa_madre))) %>% 
  select(semana, masa)

masamadre$masa <- as.numeric(masamadre$masa)
masamadre$masa[is.na(masamadre$masa)] <- 0


#plot churros
plot4 <- ggplot(masamadre,
                aes(x = semana, y = 1, fill = masa))+
  geom_tile()+
  scale_x_date(date_labels = "%b", date_breaks = "1 month", expand = c(0, 0))+
  scale_y_continuous(expand = c(0, 0))+
  scale_fill_gradientn(colors = pal) +
  guides(fill = guide_colorbar(barwidth = 10, barheight = 1))+
  labs(title = '"M A S A  M A D R E"')+
  theme_strip +
  theme(legend.position = "none")
plot4


#levadura fresca
levadurafresca <- trends_recetas %>% 
  mutate(levadura = ifelse(as.character(levadura_fresca) == "< 1", "0", as.character(levadura_fresca))) %>% 
  select(semana, levadura)

levadurafresca$levadura <- as.numeric(levadurafresca$levadura)
levadurafresca$levadura[is.na(levadurafresca$levadura)] <- 0


#plot churros
plot5 <- ggplot(levadurafresca,
                aes(x = semana, y = 1, fill = levadura))+
  geom_tile()+
  scale_x_date(date_labels = "%b", date_breaks = "1 month", expand = c(0, 0))+
  scale_y_continuous(expand = c(0, 0))+
  scale_fill_gradientn(colors = pal) +
  guides(fill = guide_colorbar(barwidth = 10, barheight = 1))+
  labs(title = '"L E V A D U R A  F R E S C A"')+
  theme_strip +
  theme(legend.position = "none")
plot5



#leyenda arriba
column <- plot_grid(plot1 + theme(legend.position="none"),
                    plot2,
                    plot3,
                    plot4,
                    plot5,
                    nrow=5)

#legend
legend <- get_legend(
  plot1 + theme(legend.box.margin = margin(0, 0, 0, 0),
                legend.position="top")
)

#Caption
ggdraw() +
  draw_text("Visualización: Laura Navarro Soler | Datos: Google Trends 2020",
            size = 12, x = 0.44, y = 0.2, hjust = 0,
            color = "#8c8c8c", family = "Leelawadee UI Semilight") -> caption


#title
ggdraw() +
  draw_text("2 0 2 0",
            x= 0.5, 
            y = 0.8, 
            size = 34, 
            family = "Leelawadee UI") +
  draw_text("ESTAS HAN SIDO LAS RECETAS MÁS BUSCADAS EN GOOGLE EN ESPAÑA", 
            y = 0.6, 
            x = 0.5, 
            size = 16, 
            family = "Leelawadee UI Semilight") +
  draw_text("UN VALOR DE 100 REPRESENTA LA \nPOPULARIDAD MÁXIMA DEL TÉRMINO", 
            y = 0.4, 
            x = 0.5, 
            size = 16, 
            family = "Leelawadee UI Semilight",
            color = "#A5A5A5") -> header



final_plot <- plot_grid(
  header,
  legend,
  column,
  caption,
  rel_heights = c(2,0.4,5,0.5),
  nrow = 4
) 

final_plot


ggsave("RECETAS.png", 
       final_plot, 
       height = 11, width = 8, 
       units = "in", dpi = 300)
