
library(tidyverse)
library(plotly)
library(viridis)
library(openxlsx)

glimpse(DF3)
DF3 <- DF3 %>% as_tibble()

# Generando gráficas ----

## Marca que más compra

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
  
  filter(`¿Cuál es la marca que más compra?` != "Ninguno") %>% 
  
  ggplot(mapping = aes(x = `¿Cuál es la marca que más compra?`,
                       y = Proporción,
                       fill = `¿Cuál es la marca que más compra?`,
                       label = Porcentaje)) +
  
  geom_col() +
  
  geom_label(fill = "white") +
  
  scale_fill_viridis_d() +
  
  theme_minimal() +
  
  scale_y_continuous(labels = scales::percent) +
  
  theme(legend.position = "none")





