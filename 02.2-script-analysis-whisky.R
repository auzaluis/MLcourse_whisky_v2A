
library(tidyverse)
library(plotly)
library(viridis)
library(openxlsx)

glimpse(DF3)
DF3 <- DF3 %>% as_tibble()

# Generando gráficas ----

## Marca que más compra (lealtad)

tabla_lealtad <- DF3 %>% 
  # contar los casos
  count(`¿Cuál es la marca que más compra?`) %>% 
  # ordenar la tabla
  arrange(desc(n)) %>% 
  # calcular %
  mutate(Proporción = n/sum(n),
         Porcentaje = scales::percent(Proporción))


## Guardando en Excel 
write.xlsx(x = tabla_lealtad,
           file = "lealtad.xlsx")


tabla_lealtad %>% 
  
  rename(Lealtad = `¿Cuál es la marca que más compra?`) %>% 
  
  filter(Lealtad != "Ninguno") %>% 
  
  mutate(Lealtad = factor(Lealtad),
         Lealtad = fct_reorder(Lealtad, n, .desc = T)) %>% 
  
  ggplot(mapping = aes(x = Lealtad,
                       y = Proporción,
                       fill = Lealtad,
                       label = Porcentaje)) +
  
  geom_col() +
  
  geom_label(fill = "white") +
  
  labs(title = "Lealtad de marca",
       subtitle = "¿Cuál es la marca que más compra?",
       caption = "Johnnie Walker es la marca preferida") +
  
  scale_fill_viridis_d() +
  
  theme_minimal() +
  
  scale_y_continuous(labels = scales::percent) +
  
  theme(legend.position = "none",
        panel.grid.major.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        axis.title.x = element_blank())



## Prueba

DF3 %>% 
  pivot_longer(cols = starts_with("Prueba"),
               names_to = "Variable",
               values_to = "Prueba") %>% 
  select(Prueba) %>% 
  na.omit() %>% 
  count(Prueba) %>% 
  mutate(Proporción = n/nrow(DF3),
         Prueba = ifelse(test = Prueba == "Johnny Walker",
                         yes = "Johnnie Walker",
                         no = Prueba),
         Porcentaje = scales::percent(Proporción),
         Prueba = factor(Prueba),
         Prueba = fct_reorder(Prueba, n, .desc = T)) %>% 
  
  ggplot(mapping = aes(x = Prueba,
                       y = Proporción,
                       fill = Prueba,
                       label = Porcentaje)) +
  
  geom_col() +
  
  geom_label(fill = "white") +
  
  labs(title = "Prueba de marca",
       subtitle = "¿Cuáles de estar marcas ha comprado alguna vez?",
       caption = "Las marcas seguidoras no llegan ni al 50%") +
  
  scale_fill_viridis_d() +
  
  theme_minimal() +
  
  scale_y_continuous(labels = scales::percent) +
  
  theme(legend.position = "none",
        panel.grid.major.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        axis.title.x = element_blank())

  
  

DF3 %>% 
  pivot_longer(cols = starts_with("Prueba"),
               names_to = "Variable",
               values_to = "Prueba") %>% 
  select(Prueba) %>% 
  na.omit() %>% 
  count(Prueba) %>% 
  mutate(Proporción = n/nrow(DF3),
         Porcentaje = scales::percent(Proporción),
         Prueba = factor(Prueba),
         Prueba = fct_reorder(Prueba, n, .desc = T))
  
## Creación de funciones

table <- function(dataFrame, indicador){
  
   dataFrame %>% 
    pivot_longer(cols = starts_with(indicador),
                 names_to = "Variables",
                 values_to = "KPI") %>% 
    select(KPI) %>% 
    na.omit() %>% 
    count(KPI) %>% 
    mutate(Proporción = n/nrow(dataFrame))
  
}


table(dataFrame = DF3, indicador = "Conocimiento")
  
  
  
  
  





