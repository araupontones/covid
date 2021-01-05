source("set_up.R")
source("styles.R")






#'read data-----------------------------------------------------------------------
df_raw = import(file.path(dir_raw, "Casos_Diarios_Municipio_Defunciones_20210102.csv")) 

centroids = import(file.path(dir_shapes, "centroids_municipios.rds")) %>%
  st_as_sf()

shape = maps::map('world', fill = TRUE, plot = F) %>%
  st_as_sf() %>%
  filter(ID == "Mexico")



#clean ------------------------------------------------------------------------

df_municipios = df_raw %>%
  #' clean IDs
  mutate(cve_ent = as.character(cve_ent),
         cve_ent = if_else(nchar(cve_ent)<5, paste0("0", cve_ent), cve_ent)
  ) %>%
  #' format to long
  pivot_longer(-c(cve_ent, poblacion, nombre),
               names_to = "fecha",
               values_to = "muertos") %>%
  #' count deaths by muincipio
  group_by(cve_ent, nombre) %>%
  summarise(muertos = sum(muertos), .groups = 'drop') %>%
  
  #'join with geometry
  right_join(centroids, by = c("cve_ent"="MUNICIPIO_RES_ID")) %>%
  select(nombre, muertos, geometry) %>%
  mutate(color = case_when(muertos >=1000 ~ "alto",
                           muertos <1000 & muertos >=500 ~ "medio",
                           muertos < 500 & muertos > 50 ~ "bajo",
                           T ~ "otro")
  ) %>%
  st_as_sf()



#' draw plot--------------------------------------------------------------------

plot = ggplot(data = shape) +
  #'Mexico boundaries ------------------------------------------------------
  geom_sf(
    color = alpha("gray",.4),
    fill = color_night
    ) +
  
  #'dots-------------------------------------------------------------------
  geom_sf(data = df_municipios,
          aes(color = color,
              fill= color),
          size = .2,
          show.legend = F,
          shape = 21) +
  
  #' colors ---------------------------------------------------------------
  scale_color_manual(values= c(alpha("red",1),
                               alpha("#F77200",.9),
                               alpha("#F7F700",.9),
                               alpha("#D7CCC8",.2)
  )
  )+
  scale_fill_manual(values= c(alpha("red",1),
                               alpha("#F77200",.9),
                               alpha("#F7F700",.9),
                               alpha("#D7CCC8",.2)
  )
  )+
  #'title --------------------------------------------------------------------
  annotate(
    'text',
    x = -128,
    y = 40,
    color = 'white',
    label = "Defunciones acumuladas por COVID-19",
    size = 9,
    family = "Times New Roman",
    hjust = 0
    )+
  #subtitle -------------------------------------------------------------------
  annotate(
    'text',
    x = -128,
    y = 37.5,
    color = 'white',
    label = "Del 2 de marzo del 2020 al 2 de enero del 2021",
    size = 6,
    family = "Times New Roman",
    hjust = 0)+
  
  annotate(
    'text',
    x = -128,
    y = 35.5,
    color = 'white',
    label = "Municipios mexicanos",
    size = 6,
    family = "Times New Roman",
    hjust = 0)+
  
  #' label of totals ----------------------------------------------------------
  annotate(
    'text',
    x = -100,
    y = 12,
    color = 'white',
    label = "Total: 126,851",
    size = 7,
    family = "Times New Roman",
    hjust = 0)+
  #' legend ----------------------------------------------------------------
      ## > 1,000
  geom_point(
    aes(
      x = -128,
      y = 18
      
    ),
    color = alpha("red",1),
    fill = alpha("red",1),
    size = 3,
    shape = 21
    
  )  +
  
    annotate(
    'text',
    x = -127,
    y = 18,
    color = 'white',
    label = "> 1,000",
    size = 4,
    family = "Times New Roman",
    hjust = 0) +
  
    ##1000 - 500
  geom_point(
    aes(
      x = -128,
      y = 16
      
    ),
    color = alpha("#F77200",.9),
    fill = alpha("#F77200",.9),
    size = 3
    
  )  +
  annotate(
    'text',
    x = -127,
    y = 16,
    color = 'white',
    label = "< 1,000 & >= 500 ",
    size = 4,
    family = "Times New Roman",
    hjust = 0) +
  
    ##500 - 50
  geom_point(
    aes(
      x = -128,
      y = 14
      
    ),
    color = alpha("#F7F700",.9),
    fill = alpha("#F7F700",.9),
    size = 3,
    shape = 21
    
  )  +
  annotate(
    'text',
    x = -127,
    y = 14,
    color = 'white',
    label = "< 500 & >= 50 ",
    size = 4,
    family = "Times New Roman",
    hjust = 0) +
  
  ##<50-------------
geom_point(
  aes(
    x = -128,
    y = 12
    
  ),
  color = alpha("#D7CCC8",.2),
  fill = alpha("#D7CCC8",.2),
  size = 3,
  shape = 21
  
)  +
  annotate(
    'text',
    x = -127,
    y = 12,
    color = 'white',
    label = "< 50 ",
    size = 4,
    family = "Times New Roman",
    hjust = 0) +
  
  annotate(
    'text',
    x = -128,
    y = 22,
    color = 'white',
    label = "Cada punto representa el\nnúmero de defunciones acumuladas\nen cada municipio:",
    size = 4,
    family = "Times New Roman",
    hjust = 0,
    fontface = 'italic') +
  
  
  
  #limits -------------------------------------------------------------------
  ylim(10, 40)+
  xlim(-128, -85) +
  
  # Caption -----------------------------------------------------------------
  labs(
    caption = "**Andres Arau** | Datos: datos.covid-19.conacyt.mx | 2 de enero 2021 | Inspirado por: James Cheshire"
  )+
 
  
  #theme ----------------------------------------------------------------------
  theme(
    panel.background = element_blank(),
    plot.background = element_rect(fill = color_night),
    panel.grid = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    axis.title = element_blank(),
    plot.caption = element_markdown(family = "Times New Roman", color = "white", size = 12, hjust = .5)
  ) 





plot

filename = file.path(dir_plots, "defunciones_2enero21.png")

ggsave(filename, plot,
       dpi = 600)





#' dots people death
# geom_sf(data = df_personas,
#       color ="#FBFAF0",
#       size = .2) +
#'dots high frequency municipios

names(df_ids)
setdiff(df_ids$cve_ent, shape$MUNICIPIO_RES_ID)

names(df_raw)


df_raw$nombre

head(df_raw$poblacion)
head(shape$MUNICIPIO_RES_ID)

