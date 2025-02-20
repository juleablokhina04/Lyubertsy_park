### load packages
library(tidyverse)
library(sf)
sf::sf_use_s2(FALSE)
library(osmdata)
library(ggmap)
library(leaflet)
library(RColorBrewer)

features = available_features()
head(available_features())

# Features to check  - area, bicycle, bridge, building, boundary, agricultural,
# footway, golf_cart, lanes, water, wood


map = get_map(getbb("Lyubertsy"), source = "osm",  maptype="roadmap")
ggmap(map)

query = getbb("Lyubertsy") %>% opq() %>% add_osm_feature("water")
Lyubertsy_water = osmdata_sf(query)

query = getbb("Lyubertsy") %>% opq() %>% add_osm_feature("building")
Lyubertsy_building = osmdata_sf(query)

ggplot()+
  geom_sf(data = Lyubertsy_building$osm_polygons,
          mapping = aes(),
          fill = "green")

ggplot()+
  geom_sf(data = Lyubertsy_water$osm_polygons,
          mapping = aes(),
          fill = "blue")

### Задание 1.Наташинский парк, г.Люберцы

park_sf <- read_sf('D:/Рабочий стол/polygon/Lyubertsy_park.geojson')
plot(park_sf)

### Предварительный этап

get_overpass_url()
overpass_url = "https://maps.mail.ru/osm/tools/overpass/api/interpreter"
set_overpass_url(overpass_url)
bbox = st_bbox(park_sf$geometry) %>% matrix(ncol=2,nrow=2) 

mad_map <- get_map(bbox, zoom = 17, source = "stamen")
plot(mad_map)
mad_map <- get_map(bbox, zoom = 16, source = "stamen")
plot(mad_map)
mad_map <- get_map(bbox, zoom = 15, source = "stamen")
plot(mad_map)

bbox = st_bbox(park_sf$geometry) %>% matrix(ncol=2,nrow=2) 
colnames(bbox) = c("min","max")
rownames(bbox) = c("x","y")

Lyubertsy_all = bbox %>% opq(timeout = 900) %>% add_osm_feature(key = "natural", value = available_tags("natural")) %>% osmdata_sf()

ggplot()+geom_sf(data = Lyubertsy_all$osm_polygons, aes())+geom_sf(data = Lyubertsy_all$osm_polygons, aes(fill=natural))+theme_bw()

### Подготовка данных OSM к анализу

map_polygons = st_read('D:/Рабочий стол/polygon/map.osm', layer = 'multipolygons', quiet = TRUE)

map_lines = st_read('D:/Рабочий стол/polygon/map.osm', layer = 'lines', quiet = TRUE)
map_polygons = map_polygons %>% filter(!is.na(natural) | !is.na(building))

map_lines = map_lines %>% filter(highway == "footway")
map_polygons = st_intersection(map_polygons, park_sf)
map_lines = st_intersection(map_lines, park_sf)

map_polygons$geometry = map_polygons$geometry %>% s2::s2_rebuild() %>%sf::st_as_sfc()
map_lines$geometry = map_lines$geometry %>% s2::s2_rebuild() %>% sf::st_as_sfc()
map_polygons = map_polygons %>% st_collection_extract(type="POLYGON")

map_polygons$natural[!is.na(map_polygons$building)] = "yards"
map_polygons$natural = as.factor(map_polygons$natural)

levels(map_polygons$natural) = c("Водные объекты", "Строения")
map_lines$highway = as.factor(map_lines$highway)

levels(map_lines$highway) = c("асфальт")

### Cоздание собственных карт

ggplot()+geom_sf(map_polygons,map=aes(fill=natural))+
geom_sf(map_lines,map=aes(color=highway))+theme_bw()

### Создание собственных интерактивных карт c помощью Leaflet

leaflet() %>% addProviderTiles("OpenTopoMap") %>% addPolygons(data = park_sf$geometry %>% s2::s2_rebuild() %>%
            sf::st_as_sfc(),  color = "black") %>% addPolygons(data = map_polygons, fillColor = "grey", color = "grey") %>% addPolylines(data = map_lines, color = "red")

### Создание собственных карт с подложкой

ggmap(mad_map)+ geom_sf(data = map_polygons,map=aes(fill=natural), inherit.aes = FALSE )+
geom_sf(data = map_lines,map=aes(color=highway), inherit.aes = FALSE)+ xlab("Широта, °")+ ylab("Долгота, °")+
guides(fill = guide_legend(title="Легенда"),color =guide_legend(title = "Дороги") )+
theme_bw()+ theme(legend.position = "bottom")+ scale_fill_brewer(palette="Set3")

### Интерпретация данных OSM

library(lwgeom)

area = st_area(park_sf)
water_area = sum(st_area(map_polygons %>% filter(natural=="Водные объекты")))
building_area = sum(st_area(map_polygons %>% filter(!is.na(building))))
footway_length = st_length(map_lines)
footway_area = sum(footway_length*2)

### Рассчет запечатанности

summary_ha = tibble(water = round(as.numeric(water_area )/10000, 2),
                    build = round(as.double(building_area)/10000, 2),
                    road = round(as.double(footway_area)/10000, 2),
                    grass = round(as.numeric(area)/10000, 2) - water - build -road,
                    area = sum(round(as.numeric(area)/10000, 2)),
                    name = "Площадь, га")

summary_perc = summary_ha %>% mutate(water = water / area * 100,
                                     build = build / area * 100,
                                     road = road / area * 100,
                                     grass = grass/area*100,
                                     area =  100,
                                     name = "Доля, %")

summary_final = summary_ha %>% mutate(area = (build+road)/(area-water)*100,
                                      water = NA,
                                      build = NA,
                                      road = NA,
                                      grass = NA,
                                      name = "Итого запечатано %")

### Создание сводной таблицы

library(gt)

summary_table = rbind(summary_ha,summary_perc, summary_final) 
summary_table = summary_table %>% select(name,water,road,build,grass,area)

sum_tabl = summary_table %>% gt(rowname_col = "name") %>% tab_header(title = "Сводная таблица типов ландшафта парка культуры «Наташинский парк»",
                                                                     subtitle = "Площади, доли и степень запечатанности") %>% 
  cols_label(water = "Водные объекты",
             area = "Итого",
             road = "Дорожное покрытие",
             build = "Строения",
             grass = "Газон") %>%  fmt_number(columns = everything()) %>% fmt_missing(columns = everything())
