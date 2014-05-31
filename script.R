#+ license, echo=FALSE
# 
# Copyright (C) 2014 Simon Garnier
# 
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
# 
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
# 
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.

#+ libraries, echo=FALSE
require("data.table")
require("dplyr")
require("acs")
require("maps")
require("mapdata")
require("ggplot2")
if (!require("graphZoo")) {
  require("devtools")
  install_github("morpionZ/graphZoo")
}
require("extrafont")
loadfonts()
source("functions.R")


#+ load.eia.data, echo=FALSE
if (file.exists("EIA.csv")) {
  eia.data <- fread("EIA.csv")
} else {
  eia.key <- readline(prompt = "Provide EIA API key to continue: ")
  base.url <- paste0("http://api.eia.gov/series/?api_key=", eia.key, "&series_id=ELEC.CONS_TOT_BTU.")
  var <- expand.grid(fuel.type = c("COW", "PEL", "PC", "NG"), 
                     state = state.abb)
  full.url <- paste0(base.url, var$fuel.type, "-", var$state, "-99.M")
  
  eia.data <- data.table(URL = full.url,
                         STATE = var$state,
                         FUEL = var$fuel.type) %>%
    group_by(STATE, FUEL) %>%
    do(readEIA(URL)) %>%
    ungroup()
  
  write.csv(eia.data, "EIA.csv", row.names = FALSE)
}


#+ load.pop.data, echo=FALSE
if (file.exists("FRED.csv")) {
  fred.data <- fread("FRED.csv")
} else {
  fred.key <- readline(prompt = "Provide FRED API key to continue: ")
  base.url <- paste0("http://api.stlouisfed.org/fred/series/observations?realtime_start=1776-07-04&realtime_end=9999-12-31&api_key=", fred.key, "&file_type=json&series_id=")
  full.url <- paste0(base.url, state.abb, "POP")
  
  fred.data <- data.table(URL = full.url,
                          STATE = state.abb) %>%
    group_by(STATE) %>%
    do(readFRED(URL)) %>%
    ungroup()
  
  write.csv(fred.data, "FRED.csv", row.names = FALSE)
}

fred.data <- fred.data %>%
  group_by(STATE, YEAR) %>%
  summarize(POPULATION = max(POPULATION))


#+ combine.data, echo=FALSE
comb.data <- merge(eia.data, fred.data, by = c("STATE", "YEAR"), all.x = TRUE)


#+ plot.2013.consumption.by.state, echo=TRUE
tmp <- comb.data %>%
  filter(YEAR == 2013) %>%
  group_by(STATE) %>%
  summarize(CONSUMPTION = sum(CONSUMPTION, na.rm = TRUE),
            POPULATION = POPULATION[1]) %>%
  mutate(PERCAP = (CONSUMPTION / POPULATION) * 100000)

us.state.map <- as.data.table(map_data('state', project="albers", par = c(39, 45))) %>%
  mutate(STATE = state.abb[match(toupper(region), toupper(state.name))]) %>%
  merge(tmp, by = "STATE", all.x = TRUE)

start.long <- -0.425 #min(us.state.map$long)
start.lat <- min(us.state.map$lat)

alaska.map <- as.data.table(map_data("world2Hires", "USA:Alaska", project="albers", par=c(39, 45))) %>%
  mutate(long = long - (min(long) - start.long),
         lat = lat - (min(lat) - start.lat)) %>%
  mutate(long = (long - min(long)) * 0.35 + min(long),
         lat = (lat - min(lat)) * 0.35 + min(lat)) %>%
  mutate(STATE = "AK") %>%
  merge(tmp, by = "STATE", all.x = TRUE)

xlim <- range(us.state.map$long, na.rm = TRUE)
#xlim <- xlim + diff(xlim) * c(-.05, .05)
ylim <- range(us.state.map$lat, na.rm = TRUE)
#ylim <- ylim + diff(xlim) * c(-.025, .025)
subtitle <- "Million MMBtu per 100,000 inhabitants"

g <- ggplot() +
  geom_polygon(data = us.state.map, 
               aes(x = long, y = lat, group = group, fill = PERCAP),
               color = "white") +
  geom_polygon(data = alaska.map, 
               aes(x = long, y = lat, group = group, fill = PERCAP),
               color = "white") +
  coord_fixed(xlim = xlim, ylim = ylim) +
  theme_graphzoo(base_size = 28, family = "Ume P Gothic") +
  theme(axis.line = element_blank(), 
        axis.text.x = element_blank(), 
        axis.text.y = element_blank(),
        axis.ticks = element_blank(), 
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.margin = unit(c(1, 0, 0, -1), "lines"),
        legend.position = "bottom") +
  scale_fill_gradient(low = "green3", high = "red3", 
                      limits = c(0, 100), trans = "sqrt",
                       guide = guide_colorbar(title = element_blank(),
                                              label.position = "bottom",
                                              barwidth = 30)) +
  ggtitle(bquote(atop("Electric consumption by US state", 
                      atop(italic(.(subtitle)), ""))))

g <- addBanner(g, font.size = 5.83,
               l.txt = "GRAPHZOO.TUMBLR.COM", r.txt = "SOURCE: EIA, FRED")

png("electric_consumption_percap.png", width = 900, height = 700, bg = "#F0F0F0")
g
dev.off()















