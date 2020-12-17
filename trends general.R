library(lubridate)
library(tidyverse)
library(RColorBrewer)
library(cowplot)
library(viridis)
library(extrafont)

#import the annual temperatures
trends_2020 <- read_csv("trends_general.csv")

trends <- trends_2020 %>% 
  mutate(covid = ifelse(as.character(coronavirus) == "< 1", "0", as.character(coronavirus))) %>% 
  select(semana, covid)

trends$covid1 <- as.numeric(trends$covid)
trends$covid1[is.na(trends$covid1)] <- 0


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


plot1 <- ggplot(trends,
       aes(x = semana, y = 1, fill = covid1))+
  geom_tile()+
  scale_x_date(date_labels = "%b", date_breaks = "1 month", expand = c(0, 0))+
  scale_y_continuous(expand = c(0, 0))+
  scale_fill_gradientn(colors = pal) +
  guides(fill = guide_colorbar(barwidth = 10, barheight = 1))+
  labs(title = '"C O R O N A V I R U S"')+
  theme_strip +
  theme(legend.position = "none")
plot1

#laliga
liga <- trends_2020 %>% 
  mutate(covid = ifelse(as.character(la_liga) == "< 1", "0", as.character(la_liga))) %>% 
  select(semana, la_liga)

liga$liga1 <- as.numeric(liga$la_liga)
liga$liga1[is.na(liga$liga1)] <- 0

plot2 <- ggplot(liga,
       aes(x = semana, y = 1, fill = liga1))+
  geom_tile()+
  scale_x_date(date_labels = "%b", date_breaks = "1 month", expand = c(0, 0))+
  scale_y_continuous(expand = c(0, 0))+
  scale_fill_gradientn(colors = pal)+
  guides(fill = guide_colorbar(barwidth = 1))+
  labs(title = '"L A  L I G A"')+
  theme_strip +
  theme(legend.position = "none")

plot2
#classroom
classroom <- trends_2020 %>% 
  select(semana, classroom)

classroom$class1 <- as.numeric(classroom$classroom)
classroom$class1[is.na(classroom$class1)] <- 0


plot3 <- ggplot(classroom,
       aes(x = semana, y = 1, fill = class1))+
  geom_tile()+
  scale_x_date(date_labels = "%b", date_breaks = "1 month", expand = c(0, 0))+
  scale_y_continuous(expand = c(0, 0))+
  scale_fill_gradientn(colors = pal)+
  guides(fill = guide_colorbar(barwidth = 1))+
  labs(title = '"C L A S S R O O M"')+
  theme_strip +
  theme(legend.position = "none")
plot3

#elecciones eeuu
elections <- trends_2020 %>% 
  select(semana, elecciones_eeuu)

elections$elecciones_eeuu1 <- as.numeric(elections$elecciones_eeuu)
elections$elecciones_eeuu1[is.na(elections$elecciones_eeuu1)] <- 0


plot4 <- ggplot(elections,
       aes(x = semana, y = 1, fill = elecciones_eeuu1))+
  geom_tile()+
  scale_x_date(date_labels = "%b", date_breaks = "1 month", expand = c(0, 0))+
  scale_y_continuous(expand = c(0, 0))+
  scale_fill_gradientn(colors = pal)+
  guides(fill = guide_colorbar(barwidth = 1))+
  labs(title ='"E L E C C I O N E S   E E. U U."')+
  theme_strip + 
  theme(legend.position = "none")
plot4

#kobe bryant
kobe <- trends_2020 %>% 
  mutate(kobe = ifelse(as.character(kobe_bryant) == "< 1", "0", as.character(kobe_bryant))) %>% 
  select(semana, kobe_bryant)

kobe$kobe_bryant <- as.numeric(kobe$kobe_bryant)
kobe$kobe_bryant[is.na(kobe$kobe_bryant)] <- 0

plot5 <- ggplot(kobe,
                aes(x = semana, y = 1, fill = kobe_bryant))+
  geom_tile()+
  scale_x_date(date_labels = "%b", date_breaks = "1 month", expand = c(0, 0))+
  scale_y_continuous(expand = c(0, 0))+
  scale_fill_gradientn(colors = pal) +
  guides(fill = guide_colorbar(barwidth = 8, barheight = 0.9))+
  labs(title = '"K O B E  B R Y A N T')+
  theme_strip +
  theme(legend.position = "top")
plot5

#legend on the bottom
column <- plot_grid(plot5 + theme(legend.position="none"),
                    plot1,
                    plot3,
                    plot2,
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
  draw_text("ESTOS HAN SIDO LOS TÉRMINOS MÁS BUSCADOS EN GOOGLE EN ESPAÑA", 
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


ggsave("general.png", 
       final_plot, 
       height = 11, width = 8, 
       units = "in", dpi = 300)




