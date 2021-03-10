# sobre: estrructuración de datos para tabla
# about: data wrangling for table (here onwards code comments are in spanish)

library(tidyverse)
library(magrittr)
library(janitor)

# ---------------------
# importar bases
# ---------------------

vacunas <- read_csv("https://raw.githubusercontent.com/owid/covid-19-data/master/public/data/vaccinations/locations.csv")
vacunas %>% 
  select(location, iso_code, vaccines) -> vacunas
  
metricas <- read_csv("https://raw.githubusercontent.com/owid/covid-19-data/master/public/data/vaccinations/vaccinations.csv")
metricas %>% 
  filter(location !="European Union") %>% 
  select(location, iso_code, date, people_vaccinated_per_hundred, people_fully_vaccinated_per_hundred) %>% 
  left_join(., vacunas) -> metricas

  
# función people_fully_vaccinated_per_hundred
limpieza_una <- function(.data) {
  .data %>% 
    select(-people_fully_vaccinated_per_hundred) %>% 
    filter(!is.na(people_vaccinated_per_hundred)) %>% 
    arrange(desc(date)) %>% 
  slice(1:7) %>% 
    mutate(
      # para al menos una persona vacunada
      promedio_una_vacuna = lag(people_vaccinated_per_hundred),
      diferencia_una_vacuna = promedio_una_vacuna -people_vaccinated_per_hundred,
      diferencia_una_vacuna = mean(diferencia_una_vacuna, na.rm = T),
      meta_una_vacuna = 100 - people_vaccinated_per_hundred,
      tiempo_una_vacuna = (meta_una_vacuna/diferencia_una_vacuna)/7,
      ritmo_semanal_una = diferencia_una_vacuna*7,
    ) %>% 
    slice(1) %>% 
    select(location, iso_code, date, vaccines, people_vaccinated_per_hundred, tiempo_una_vacuna, ritmo_semanal_una) -> temp
  
  return(temp)
}

metricas %>% 
  group_split(location) -> temp1  

map_dfr(temp1, limpieza_una) -> temp1




limpieza_dos <- function(.data) {
  .data %>% 
    select(-people_vaccinated_per_hundred) %>% 
    filter(!is.na(people_fully_vaccinated_per_hundred)) %>% 
    arrange(desc(date)) %>% 
    slice(1:7) %>% 
    mutate(
      promedio_dos_vacuna = lag(people_fully_vaccinated_per_hundred),
      diferencia_dos_vacuna = promedio_dos_vacuna - people_fully_vaccinated_per_hundred,
      diferencia_dos_vacuna = mean(diferencia_dos_vacuna, na.rm = T),
      meta_dos_vacuna = 100 - people_fully_vaccinated_per_hundred,
      tiempo_dos_vacuna = (meta_dos_vacuna/diferencia_dos_vacuna)/7,
      ritmo_semanal_dos = diferencia_dos_vacuna*7
    ) %>% 
    slice(1) %>% 
    select(location, iso_code, date, vaccines, people_fully_vaccinated_per_hundred, tiempo_dos_vacuna, ritmo_semanal_dos) -> temp
  
  return(temp)
}

metricas %>% 
  group_split(location) -> temp2  

map_dfr(temp2, limpieza_dos) -> temp2

left_join(temp1, temp2) -> temp


# añadir columna con nombres de países en español
countrycode::codelist %>%
  select(iso_code = iso3c, nombre_espanol = un.name.es) %>% 
  filter(iso_code %in% (temp$iso_code %>% unique)) %>% 
  janitor::remove_empty() %>% 
  left_join(temp, .) %>% 
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
  ) -> temp 
  
rm(metricas, temp1, temp2, vacunas, limpieza_una, limpieza_dos)

#--------------------------------------------------------
# añadir columna de curvas de fallecidos y contagiados
#--------------------------------------------------------

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
  select(-iso_code, -location, people_vaccinated_per_hundred, confirmados,
         fallecidos, -people_fully_vaccinated_per_hundred, -vaccines,
         tiempo_una_vacuna, ritmo_semanal_una) %>% 
  filter(!is.na(tiempo_una_vacuna)) %>% 
  filter(!is.infinite(tiempo_una_vacuna)) %>% 
  filter(!is.na(ritmo_semanal_una)) %>% 
  filter(!is.infinite(ritmo_semanal_una)) %>% 
  select(-contains("_dos")) %>% 
  filter(!nombre_espanol %in% c("Inglaterra", "Wales", "Irlanda del Norte", "Isla de Man", "Gibraltar", "Escocia")) %>%
  mutate(tiempo_una_vacuna = round(tiempo_una_vacuna, 0), 
         people_vaccinated_per_hundred = round(people_vaccinated_per_hundred, 2)) -> temp

temp %>% 
  select(date, nombre_espanol, everything()) -> temp
temp$nombre_espanol %<>% gsub("Reino Unido de Gran Bretaña e Irlanda del Norte", "Reino Unido", .)
temp$nombre_espanol %<>% gsub("Estados Unidos de América", "Estados Unidos", .)


Sys.setlocale(locale = "es_ES.UTF-8")


temp %>% 
  select(1:3, 5, 4, 6:7) %>% 
  mutate(
    date =  format(date, "%d de %B")  
  ) %>% 
  filter(!is.na(nombre_espanol)) -> temp


temp %<>% filter(nombre_espanol != "Emiratos Árabes Unidos")


# nombres al portugués
nombres <- rio::import("input/nombres.csv", sep = ";")

temp %<>% merge(., nombres, all.x = T)
temp %<>%
  select(-nombre_espanol) %>% 
  rename(nombre_espanol = nombres_pt) %>% 
  select(nombre_espanol, everything())








