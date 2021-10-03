
# sobre: estrructuración de datos para tabla
# about: data wrangling for table (here onwards code comments are in spanish)

library(tidyverse)
library(magrittr)
library(janitor)

# ---------------------
# importar bases
# ---------------------

options(scipen = 999)

metricas <- read_csv("https://raw.githubusercontent.com/owid/covid-19-data/master/public/data/vaccinations/vaccinations.csv")

metricas %>% 
  filter(
    location !="European Union",
    !is.na(people_vaccinated_per_hundred)
  ) %>% 
  select(location, iso_code, date, people_vaccinated_per_hundred, 
         people_fully_vaccinated_per_hundred, total_boosters_per_hundred) %>%
  group_split(location) %>% 
  map(., ~arrange(., desc(., date))) %>% 
  bind_rows() -> metricas

countrycode::codelist %>%
  select(iso_code = iso3c, nombre_espanol = un.name.es) %>% 
  filter(iso_code %in% (metricas$iso_code %>% unique)) %>% 
  janitor::remove_empty() %>% 
  left_join(metricas, .) %>% 
  mutate(
    nombre_espanol = case_when(
      location == "World" ~ "Mundo",
      location == "Scotland" ~ "Escocia",
      location == "Northern Ireland" ~ "Irlanda del Norte",
      location == "Isle of Man" ~ "Isla de Man",
      location == "England" ~ "Inglaterra",
      location == "Wales" ~ "Wales",
      location == "Gibraltar" ~ "Gibraltar", 
      T  ~ nombre_espanol
    ) 
  ) %>% 
  group_split(location) %>% 
  map(., ~arrange(., desc(date)) %>% slice(1)) %>% 
  bind_rows() -> temp



url <- "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv"
url_m <- "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_global.csv"

# descarga y limpieza
df <- read_csv(url) %>% mutate(base = "confirmados")
df_m <- read_csv(url_m) %>% mutate(base = "fallecidos")

df <- bind_rows(df, df_m) 
rm(df_m, url, url_m)

df %<>% rename(location = `Country/Region`)
temp$location[!temp$location %in% (df$location %>% unique)]
df$location %<>% gsub("US", "United States", .) 
df$location %<>% gsub("Korea\\, South", "South Korea", .) 
df$location %<>% gsub("Cabo Verde", "Cape Verde", .) 
df$location %<>% gsub("Timor-Leste", "Timor", .) 
df$location %<>% gsub("Burma", "Myanmar", .) 
df$location %<>% gsub("Congo \\(Brazzaville\\)", "Congo", .) 
df$location %<>% gsub("Congo \\(Kinshasa\\)", "Democratic Republic of Congo", .) 

temp$location[!temp$location %in% (df$location %>% unique)]

df %<>% 
  filter(location %in% temp$location) %>% 
  dplyr::select(-matches("Lat|Long")) %>% 
  dplyr::select(-`Province/State`) %>% 
  gather(fecha, casos_acumulados, -location, -base) 

df$fecha %<>% as.Date(., format = "%m/%d/%y")

# estandarización desde pacientes 0 e individualización de países en formato lista
df %>% 
  filter(casos_acumulados != 0) %>% 
  group_by(location, base, fecha) %>% 
  summarise_all(sum) %>% 
  ungroup() %>% 
  group_split(location, base) %>% 
  map(., ~arrange(., fecha)) %>% 
  map(., ~mutate(., dias = 1:nrow(.),
                 total_semanas = nrow(.)/7)) %>% 
  map(., ~mutate_if(., is.numeric, round, 0)) %>% 
  bind_rows() %>% 
  arrange(base, location, fecha) -> df

# añadir variable de número de semanas: extensión de un año 
tibble(
  semana = rep((1:120), 7)
) %>% 
  arrange(semana) %>% 
  mutate(dias = 1:nrow(.)) -> temp1

df %<>% merge(., temp1, all.x = T)
rm(temp1)

df %>% 
  group_by(fecha, base) %>% 
  summarise(casos_acumulados = sum(casos_acumulados)) %>% 
  mutate(location = "World") %>% 
  ungroup() %>% 
  group_split(base) -> mundo


tibble(
  semana = rep((1:120), 7)
) %>% 
  arrange(semana) %>% 
  mutate(dias = 1:nrow(.)) -> temp1

mundo[[1]] %>% 
  mutate(semana = temp1 %>% 
           slice(1:nrow(mundo[[1]])) %>% 
           pull(semana)) -> m1

mundo[[2]] %>% 
  mutate(semana = temp1 %>% 
           slice(1:nrow(mundo[[2]])) %>% 
           pull(semana)) %>% 
  bind_rows(., m1) %>% 
  bind_rows(df, .) -> df

rm(mundo, m1, temp1)

# calclulo de incidencia
df %>% 
  group_split(base, location) %>% 
  map(., ~arrange(., fecha)) %>% 
  map(., ~mutate(., incidencia = lag(casos_acumulados),
                 incidencia = casos_acumulados - incidencia,
                 incidencia = abs(incidencia))) %>%
  bind_rows() %>% 
  filter(!is.na(incidencia)) -> df 


# agrupacion por semanas
df %>% 
  group_by(location, semana) %>% 
  mutate(n = n()) %>% 
  filter(n >= 6) %>% 
  ungroup() %>% 
  group_by(base, location, semana) %>% 
  summarise(incidencia = sum(incidencia)) %>% 
  ungroup() %>% 
  group_split(base) -> df

df[[1]] %>% 
  group_by(location) %>% 
  summarise(confirmados = list(incidencia)) %>% 
  left_join(temp, .) -> temp

df[[2]] %>% 
  group_by(location) %>% 
  summarise(fallecidos = list(incidencia)) %>% 
  left_join(temp, .) -> temp

rm(df)


# últimos arreglos 
temp %>% 
  select(-iso_code, -location) %>% 
  filter(!nombre_espanol %in% c("Inglaterra", "Wales", "Irlanda del Norte", "Isla de Man", "Gibraltar", "Escocia")) %>%
  mutate_if(is.numeric, round, 2) -> temp

temp %>% 
  select(date, nombre_espanol, everything()) -> temp

temp$nombre_espanol %<>% gsub("Reino Unido de Gran Bretaña e Irlanda del Norte", "Reino Unido", .)
temp$nombre_espanol %<>% gsub("Estados Unidos de América", "Estados Unidos", .)
temp$nombre_espanol %<>% gsub("Bolivia \\(Estado Plurinacional de\\)", "Bolivia", .)
temp$nombre_espanol %<>% gsub("Venezuela \\(República Bolivariana de\\)", "Venezuela", .)
temp$nombre_espanol %<>% gsub("República Democrática Popular Lao", "Laos", .)
temp$nombre_espanol %<>% gsub("Irán \\(República Islámica del\\)", "Irán", .)
temp$nombre_espanol %<>% gsub("República Unida de Tanzanía", "Tanzanía", .)
temp$nombre_espanol %<>% gsub("Gambia \\(República de\\)", "Gambia", .)
temp$nombre_espanol %<>% gsub("República Árabe Siria", "Siria", .)
temp$nombre_espanol %<>% gsub("República de Moldova", "Moldova", .)
temp$nombre_espanol %<>% gsub("Federación de Rusia", "Rusia", .)
temp$nombre_espanol %<>% gsub("República de Corea", "Corea del Sur", .)

Sys.setlocale(locale = "pt_PT.UTF-8")

temp %>% 
  mutate(
    date =  format(date, "%d de %B"), 
    total_boosters_per_hundred = case_when(
      is.na(total_boosters_per_hundred) ~ 0, 
      T ~ total_boosters_per_hundred
    ), 
    people_fully_vaccinated_per_hundred = case_when(
      is.na(people_fully_vaccinated_per_hundred) ~ 0, 
      T ~ people_fully_vaccinated_per_hundred
    )
  ) %>% 
  filter(!is.na(nombre_espanol)) -> temp

# importar nombres en portugues
nombres <- rio::import("input/nombres.csv", sep = ";")

temp %<>% merge(., nombres, all.x = T) 

temp %<>%
  select(-nombre_espanol) %>% 
  rename(nombre_espanol = nombres_pt) %>% 
  select(date, nombre_espanol, everything()) %>% 
  filter(!is.na(nombre_espanol))









