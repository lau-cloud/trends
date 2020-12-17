library(lubridate)
library(tidyverse)
library(RColorBrewer)
library(cowplot)
library(viridis)
library(extrafont)

#import the annual temperatures
trends_2020 <- read_csv("trends_cuando.csv")

black_friday <- trends_2020 %>% 
  mutate(black = ifelse(as.character(black_friday) == "< 1", "0", as.character(black_friday))) %>% 
  select(setmana, black)

black_friday$black <- as.numeric(black_friday$black)
black_friday$black[is.na(black_friday$black)] <- 0


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


plot1 <- ggplot(black_friday,
                aes(x = setmana, y = 1, fill = black))+
  geom_tile()+
  scale_x_date(date_labels = "%b", date_breaks = "1 month", expand = c(0, 0))+
  scale_y_continuous(expand = c(0, 0))+
  scale_fill_gradientn(colors = pal) +
  guides(fill = guide_colorbar(barwidth = 10, barheight = 1))+
  labs(title = '"¿C U Á N D O   E S   B L A C K   F R I D A Y?"')+
  theme_strip +
  theme(legend.position = "none")
plot1

#peluquerías
pelus <- trends_2020 %>% 
  mutate(peluquerias = ifelse(as.character(pelus) == "< 1", "0", as.character(pelus))) %>% 
  select(setmana, peluquerias)

pelus$peluquerias <- as.numeric(pelus$peluquerias)
pelus$peluquerias[is.na(pelus$peluquerias)] <- 0

plot2 <- ggplot(pelus,
                aes(x = setmana, y = 1, fill = peluquerias))+
  geom_tile()+
  scale_x_date(date_labels = "%b", date_breaks = "1 month", expand = c(0, 0))+
  scale_y_continuous(expand = c(0, 0))+
  scale_fill_gradientn(colors = pal)+
  guides(fill = guide_colorbar(barwidth = 1))+
  labs(title = '"¿C U Á N D O   A B R E N   P E L U Q U E R Í A S?"')+
  theme_strip +
  theme(legend.position = "none")

plot2

#ERTES
ertes <- trends_2020 %>% 
  select(setmana, erte)

ertes$erte <- as.numeric(ertes$erte)
ertes$erte[is.na(ertes$erte)] <- 0


plot3 <- ggplot(ertes,
                aes(x = setmana, y = 1, fill = erte))+
  geom_tile()+
  scale_x_date(date_labels = "%b", date_breaks = "1 month", expand = c(0, 0))+
  scale_y_continuous(expand = c(0, 0))+
  scale_fill_gradientn(colors = pal)+
  guides(fill = guide_colorbar(barwidth = 1))+
  labs(title = '"¿C U Á N D O   S E   C O B R A   E L   E R T E?"')+
  theme_strip +
  theme(legend.position = "none")
plot3

#ESTADO DE ALARMA
alarma<- trends_2020 %>% 
  select(setmana, alarma)

alarma$alarma <- as.numeric(alarma$alarma)
alarma$alarma[is.na(alarma$alarma)] <- 0


plot4 <- ggplot(alarma,
                aes(x = setmana, y = 1, fill = alarma))+
  geom_tile()+
  scale_x_date(date_labels = "%b", date_breaks = "1 month", expand = c(0, 0))+
  scale_y_continuous(expand = c(0, 0))+
  scale_fill_gradientn(colors = pal)+
  guides(fill = guide_colorbar(barwidth = 1))+
  labs(title ='"¿C U Á N D O   S E   A C A B A   E L   E S T A D O   D E   A L A R M A?"')+
  theme_strip + 
  theme(legend.position = "none")
plot4

#CONFINAMIENTO
confinamiento <- trends_2020 %>% 
  select(setmana, confi)

confinamiento$confi <- as.numeric(confinamiento$confi)
confinamiento$confi[is.na(confinamiento$confi)] <- 0

plot5 <- ggplot(confinamiento,
                aes(x = setmana, y = 1, fill = confi))+
  geom_tile()+
  scale_x_date(date_labels = "%b", date_breaks = "1 month", expand = c(0, 0))+
  scale_y_continuous(expand = c(0, 0))+
  scale_fill_gradientn(colors = pal) +
  guides(fill = guide_colorbar(barwidth = 8, barheight = 0.9))+
  labs(title = '"¿C U Á N D O   S E   A C A B A   E L   C O N F I N A M I E N T O?')+
  theme_strip +
  theme(legend.position = "top")
plot5

#legend on the bottom
column <- plot_grid(plot5 + theme(legend.position="none"),
                    plot2,
                    plot3,
                    plot4,
                    plot1,
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
  draw_text("ESTAS HAN SIDO LSD BÚSQUEDAS EMPEZADAS POR 'CUÁNDO' EN GOOGLE EN ESPAÑA", 
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


ggsave("cuando.png", 
       final_plot, 
       height = 11, width = 8, 
       units = "in", dpi = 300)
