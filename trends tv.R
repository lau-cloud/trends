library(lubridate)
library(tidyverse)
library(RColorBrewer)
library(cowplot)
library(viridis)
library(extrafont)

#import the annual temperatures
trends_2020 <- read_csv("trends_tv.csv")

tv <- trends_2020 %>% 
  select(semana, isla_tentaciones)

tv$isla_tentaciones <- as.numeric(tv$isla_tentaciones)
tv$isla_tentaciones[is.na(tv$isla_tentaciones)] <- 0


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


plot1 <- ggplot(tv,
       aes(x = semana, y = 1, fill = isla_tentaciones))+
  geom_tile()+
  scale_x_date(date_labels = "%b", date_breaks = "1 month", expand = c(0, 0))+
  scale_y_continuous(expand = c(0, 0))+
  scale_fill_gradientn(colors = pal) +
  guides(fill = guide_colorbar(barwidth = 10, barheight = 1))+
  labs(title = '"I S L A  D E  L A S   T E N T A C I O N E S"')+
  theme_strip +
  theme(legend.position = "top")
plot1

#SUPERVIVIENTES
super <- trends_2020 %>% 
  select(semana, supervivientes)

super$supervivientes <- as.numeric(super$supervivientes)
super$supervivientes[is.na(super$supervivientes)] <- 0

plot2 <- ggplot(super,
       aes(x = semana, y = 1, fill = supervivientes))+
  geom_tile()+
  scale_x_date(date_labels = "%b", date_breaks = "1 month", expand = c(0, 0))+
  scale_y_continuous(expand = c(0, 0))+
  scale_fill_gradientn(colors = pal)+
  guides(fill = guide_colorbar(barwidth = 1))+
  labs(title = '"S U P E R V I V I E N T E S"')+
  theme_strip +
  theme(legend.position = "none")

plot2

#CASAFUERTE
casa <- trends_2020 %>% 
  select(semana, casa_fuerte)

casa$casa_fuerte <- as.numeric(casa$casa_fuerte)
casa$casa_fuerte[is.na(casa$casa_fuerte)] <- 0


plot3 <- ggplot(casa,
       aes(x = semana, y = 1, fill = casa_fuerte))+
  geom_tile()+
  scale_x_date(date_labels = "%b", date_breaks = "1 month", expand = c(0, 0))+
  scale_y_continuous(expand = c(0, 0))+
  scale_fill_gradientn(colors = pal)+
  guides(fill = guide_colorbar(barwidth = 1))+
  labs(title = '"L A  C A S A  F U E R T E"')+
  theme_strip +
  theme(legend.position = "none")
plot3

#MASK SINGER
mask <- trends_2020 %>% 
  select(semana, mask_singer)

mask$mask_singer <- as.numeric(mask$mask_singer)
mask$mask_singer[is.na(mask$mask_singer)] <- 0


plot4 <- ggplot(mask,
       aes(x = semana, y = 1, fill = mask_singer))+
  geom_tile()+
  scale_x_date(date_labels = "%b", date_breaks = "1 month", expand = c(0, 0))+
  scale_y_continuous(expand = c(0, 0))+
  scale_fill_gradientn(colors = pal)+
  guides(fill = guide_colorbar(barwidth = 1))+
  labs(title ='"M A S K  S I N G E R"')+
  theme_strip + 
  theme(legend.position = "none")
plot4

#kobe bryant
gambito <- trends_2020 %>% 
  select(semana, gambito_dama)

gambito$gambito_dama <- as.numeric(gambito$gambito_dama)
gambito$gambito_dama[is.na(gambito$gambito_dama)] <- 0

plot5 <- ggplot(gambito,
                aes(x = semana, y = 1, fill = gambito_dama))+
  geom_tile()+
  scale_x_date(date_labels = "%b", date_breaks = "1 month", expand = c(0, 0))+
  scale_y_continuous(expand = c(0, 0))+
  scale_fill_gradientn(colors = pal) +
  guides(fill = guide_colorbar(barwidth = 8, barheight = 0.9))+
  labs(title = '"G A M B I T O  D E  D A M A')+
  theme_strip +
  theme(legend.position = "none")
plot5

#legend on the bottom
column <- plot_grid(plot1 + theme(legend.position="none"),
                    plot2,
                    plot3,
                    plot5,
                    plot4,
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
  draw_text("TÉRMINOS DE SERIES Y TV MÁS BUSCADOS EN GOOGLE EN ESPAÑA", 
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


ggsave("TV.png", 
       final_plot, 
       height = 11, width = 8, 
       units = "in", dpi = 300)




