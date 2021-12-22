library(tidyverse)
library(lubridate)
library(rnoaa)
library(dplyr)
library(raster)
library(sp)
library(sf)
library(elevatr)
library(rLandsat)
library(rvest)
library(curl)
library(RStoolbox)
library(RCurl)
library(MODIS)
library(exactextractr)

# Получение списка всех метеостанций
stations = ghcnd_stations(refresh = FALSE)

# После получения списка всех станций, получите список станций ближайших к Люберцам,
# создав таблицу с именем города и координатами парка 
Lyubertsy = data.frame(id="Lyub",
                       latitude = c(55.696161),
                       longitude= c(37.884302))

station_list = meteo_nearby_stations(lat_lon_df = Lyubertsy, station_data = stations,
                                     limit = 20, var = c("PRCP", "TAVG"),
                                     year_min = 2015, year_max = 2020)

station_list =station_list[[1]] 
station_list = station_list %>% filter(name %in% c("MOSCOW","PAVLOVSKIJ POSAD"))

# Для получения всех данных с метеостанции, зная ее идентификатор, используйте след. команду
one_station = meteo_tidy_ghcnd("RSM00027612")
two_station = meteo_tidy_ghcnd("RSM00027523")

all_data = rbind(one_station, two_station)
all_data = all_data %>% mutate(year = year(date))%>% 
  filter(year > 2010 & year < 2022) %>% 
  mutate(tavg = tavg/10, prcp = prcp/10)

all_data = all_data %>% dplyr::select(-tmax, -tmin, -snwd)

all_data$prcp[is.na(all_data$prcp)] = 0 
Lyubertsy_cum = all_data %>% mutate(month = month(date)) %>% 
  filter(month > 4 & month < 10) %>% 
  group_by(year) %>% 
  mutate(prcp_cum = cumsum(prcp))

Lyubertsy_cum %>% summarise(prcp_avg = max(prcp_cum), n = n())

# Посмотрите, для каких лет есть разумные данные и дальше работайте с годом, который вам больше нравится
#   <dbl>   <dbl>   <int>
#2  2012     522.   306
#3  2013     962.   306
#4  2014     452.   306


# Загрузите kml файл с полигоном вашего парка из любых онлайн карт
park_sf <- read_sf('D:/Рабочий стол/polygon/Lyubertsy_park.kml')

# Сконевртируем объект в sp и загрузим для ншаей местности ЦМР из пакета elevatr
park_sp = as_Spatial(st_zm(park_sf), 
                     cast=TRUE, 
                     IDs = paste0("ID", seq_along(from)))
prj = proj4string(park_sp)
park_dem = elevatr::get_elev_raster(park_sp, 14, prj)
plot(park_dem)
plot(st_geometry(park_sf), add = TRUE)

park_dem_mask = crop(park_dem, park_sp)
plot(park_dem_mask)
plot(st_geometry(park_sf), add = TRUE)
#qmap(park_dem_mask, park_sp)

# Самописная функция для пакета Landsat т.к. встроенная функция не работает
search_result_download = function(search_results){
  for(i in 1:nrow(search_results)){
    landsat_image_name = search_results$entit_1___2d[i]
    landsat_page_url = search_results$download_url[i]
    landsat_page = read_html(landsat_page_url, encoding = "UTF-8")
    landsat_files = landsat_page %>% html_nodes("a") %>% html_attr("href")
    landsat_path_url = landsat_page_url %>% str_remove("index.html")
    landsat_files_url = str_c(landsat_path_url, landsat_files)
    landsat_down_folder = str_c( getwd(),"/landsat/", landsat_image_name,"/")
    dir.create(landsat_down_folder)
    
    for(j in 1:length(landsat_files_url)){
      landsat_down_file = str_c(landsat_down_folder,landsat_files[j])
      print(landsat_files_url[j])
      print(landsat_down_file)
      
      download.file(landsat_files_url[j],
                    destfile = landsat_down_file,
                    method = "libcurl",
                    mode = "wb")
    }
    
  }
}

install.packages("devtools")
library(devtools)
devtools::install_github("atlanhq/rLandsat")
library(rLandsat)

# Найдем и скачаем данные Landsat для нашего парка
search2 = rLandsat::landsat_search(min_date = "2013-05-01", 
                                   max_date = "2013-09-30", 
                                   country = "Russian Federation", 
                                   source = "aws")

your_lat = 55.69
your_lon = 37.88
  
search_result = search2 %>% filter(min_lat < your_lat, max_lat > your_lat, 
                                   max_lon > your_lon, min_lon < your_lon, 
                                   clou_1___2over < 15)










