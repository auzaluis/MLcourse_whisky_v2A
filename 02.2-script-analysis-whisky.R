
library(tidyverse)
library(plotly)
library(viridis)
library(openxlsx)

glimpse(DF3)

DF3 <- DF3 %>%
  as_tibble() %>% 
  mutate(across(.cols = everything(),
                .fns = ~ gsub("Johnny Walker", "Johnnie Walker", .)))

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

  

## Creación de funciones

table <- function(dataFrame, indicador){
  
   dataFrame %>% 
    pivot_longer(cols = starts_with(indicador),
                 names_to = "Variables",
                 values_to = "Marcas") %>% 
    select(Marcas) %>% 
    na.omit() %>% 
    count(Marcas) %>% 
    mutate(Proporción = n/nrow(dataFrame),
           KPI = rep(indicador, nrow(.))) %>% 
    relocate(KPI, .before = Marcas)
  
}


table(dataFrame = DF3, indicador = "Conocimiento")

## Juntando data frames

tabla_funnel <- bind_rows(table(dataFrame = DF3,
                                indicador = "Conocimiento"),
                          table(dataFrame = DF3,
                                indicador = "Prueba"))



gg_funnel <- function(tabla, kpis, marcas, prop, porcentaje) {
  
  tabla %>% 
    
    ggplot(mapping = aes(x = .data[[kpis]],
                         y = .data[[prop]],
                         fill = .data[[kpis]],
                         label = .data[[porcentaje]])) +
    
    geom_col() +
    
    facet_wrap(~ .data[[marcas]]) +
    
    geom_label(fill = "white") +
    
    labs(title = "Funnel de marca",
         subtitle = "Conocimiento y Prueba",
         caption = "JW es la marca más conocida") +
    
    theme_minimal() +
    
    scale_y_continuous(labels = scales::percent) +
    
    theme(legend.position = "none",
          panel.grid.major.x = element_blank(),
          panel.grid.minor.y = element_blank(),
          axis.title = element_blank())
  
}

gg_funnel(tabla = tabla_funnel %>% 
            mutate(Porcentaje = scales::percent(Proporción)),
          kpis = "KPI",
          prop = "Proporción",
          marcas = "Marcas",
          porcentaje = "Porcentaje") +
  
  scale_fill_viridis_d()


# Agregando lealtad al gráfico

tabla_lealtad_2 <- tabla_lealtad %>% 
  rename(Marcas = `¿Cuál es la marca que más compra?`) %>% 
  select(-Porcentaje) %>% 
  mutate(KPI = rep("Lealtad", times = nrow(.))) %>% 
  relocate(KPI, .before = "Marcas") %>% 
  filter(Marcas != "Ninguno")

tabla_funnel_2 <- bind_rows(tabla_funnel,
                            tabla_lealtad_2)



gg_funnel(tabla = tabla_funnel_2 %>% 
            mutate(Porcentaje = scales::percent(Proporción)),
          kpis = "KPI",
          prop = "Proporción",
          marcas = "Marcas",
          porcentaje = "Porcentaje")



# Tabla de consideración

tabla_consideración <- DF3 %>% 
  
  select(starts_with("Consideración")) %>% 
  
  pivot_longer(cols = everything()) %>% 
  
  mutate(t2b = ifelse(test = value %in% c("Creo que SÍ la consideraría",
                                          "Sería mi 1ra opción"),
                      yes = 1,
                      no = 0)) %>% 
  
  separate(col = "name",
           into = c("KPI", "Marcas"),
           sep = "Consideración ") %>% 
  
  mutate(KPI = rep("Consideración",
                   times = nrow(.))) %>%
  
  mutate(Marcas = ifelse(test = Marcas == "Johnny Walker",
                         yes = "Johnnie Walker",
                         no = Marcas)) %>% 
  
  group_by(KPI, Marcas) %>% 
  
  summarise(n = sum(t2b)) %>% 
  
  ungroup() %>% 
  
  mutate(Proporción = n/nrow(DF3),
         Marcas = ifelse(Marcas == "Chivas",
                         yes = "Chivas Regal",
                         no = Marcas))

tabla_funnel_3 <- bind_rows(tabla_funnel_2,
                            tabla_consideración)

gg_funnel(tabla = tabla_funnel_3 %>% 
            mutate(Porcentaje = scales::percent(Proporción)),
          kpis = "KPI",
          prop = "Proporción",
          marcas = "Marcas",
          porcentaje = "Porcentaje")



# Tabla de recomendación

tabla_recomendación <- DF3 %>% 
  
  select(starts_with("Recomendación")) %>% 
  
  pivot_longer(cols = everything()) %>% 
  
  mutate(t2b = ifelse(test = value %in% c(9,10),
                      yes = 1,
                      no = 0)) %>% 
  
  separate(col = "name",
           into = c("KPI", "Marcas"),
           sep = "Recomendación ") %>% 
  
  mutate(KPI = rep("Recomendación",
                   times = nrow(.))) %>%
  
  mutate(Marcas = ifelse(test = Marcas == "Johnny Walker",
                         yes = "Johnnie Walker",
                         no = Marcas)) %>% 
  
  group_by(KPI, Marcas) %>% 
  
  summarise(n = sum(t2b)) %>% 
  
  ungroup() %>% 
  
  mutate(Proporción = n/nrow(DF3),
         Marcas = ifelse(Marcas == "Chivas",
                         yes = "Chivas Regal",
                         no = Marcas))

tabla_funnel_4 <- bind_rows(tabla_funnel_3,
                            tabla_recomendación)

gg_funnel(tabla = tabla_funnel_4 %>% 
            mutate(Porcentaje = scales::percent(Proporción),
                   KPI = fct_relevel(KPI, c("Conocimiento",
                                            "Consideración",
                                            "Prueba",
                                            "Lealtad",
                                            "Recomendación"))),
          kpis = "KPI",
          prop = "Proporción",
          marcas = "Marcas",
          porcentaje = "Porcentaje")







