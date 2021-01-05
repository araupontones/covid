library(COVID19)
library(tidyverse)
library(extrafont)
library(scales)
library(cowplot)

data <- covid19()

vars = c("casos", "recuperados", "Muertes", "tests", "poblacion")
por_mil = function(x,y){round(x * 100000 / y, digits = 2)}

drop = c("Diamond Princess", "MS Zaandam")

data_c = data %>%
  rename(Pais = administrative_area_level_1,
         casos = confirmed,
         recuperados = recovered,
         Muertes = deaths,
         poblacion = population) %>%
  group_by(Pais) %>%
  summarise(across(all_of(vars), max),
            .groups = 'drop') %>%
  mutate(`Por 100,000 habitantes` = por_mil(Muertes, poblacion),
         tests_mil = por_mil(tests, poblacion),
         Mexico = if_else(Pais == "Mexico", "MX", "Otro")) %>%
  filter(! Pais %in% drop) %>%
  rename(Totales = Muertes)



## styles
top = 15 ## number of top countries to vis
caption = 'Gráfica creada por @AndresArau\nDatos:Guidotti, E., Ardia, D., (2020), "COVID-19 Data Hub"'

## colors
gray = '#EFF0F1'

blue_IOM = '#0033A0'

max_blue = blue_IOM
midh_blue = '#4067c2'
midl_blue = '#8099d0'
min_blue = '#b3c2e3'

yellow_IOM = "#FFB81C"

red_IOM ="#D22630"
red_h = red_IOM
red_m = '#dd5c65'
red_l = "#e99397"



## tema
tema  = theme(
  text = element_text(family = "Calibri"),
  panel.background = element_blank(),
  legend.position = 'none',
  plot.title = element_text(hjust = 0, size = 14, face = 'bold'),
  axis.text.y = element_text(size = 12, hjust = 0),
  axis.ticks = element_blank(),
  axis.title.x = element_text(size = 14,face = "bold"),

  panel.grid.major.x = element_line(colour = gray),
  panel.grid.minor.x = element_line(colour = gray),
 
  )


## funcion para hacer graficas
barras = function(x){
  
  ggplot(data = data_c %>%
           arrange(desc(get(x))) %>%
           filter(row_number()<=top) %>%
           mutate(t = paste(row_number(), Pais, sep = ". ")),
         aes(x=get(x),
             y = reorder(t, get(x)),
             fill = Mexico)) +
    geom_bar(stat= 'identity') +
    labs(y = NULL,
         x = NULL,
         title = x,
         caption = caption) +
    scale_fill_manual(values =  c("#999999", "black")) +
    scale_x_continuous(labels = comma) +
    
    tema
  
}



## titulos de las graficas
names = c("Totales", "Por 100,000 habitantes", "poblacion")

## crear las graficas para las variables de "names"
charts <- map(names, barras)
names(charts) <- names

t1 =charts[["Totales"]] +
  labs(caption = "")
t2 =charts[["Por 100,000 habitantes"]]

## titulo de la grafica
title = ggdraw() + draw_label("Muertes por coronavirus hasta el 2 de agosto del 2020", fontfamily = "Calibri", fontface = "bold", size = 18)

## juntar las graficas
p = plot_grid(t1, t2)
plot_grid(title, p, ncol = 1, rel_heights = c(0.1,1))

