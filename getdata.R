library(tidyverse)
library(httr)

key <- Sys.getenv("NLSFI")
string <- "puna*"
q <- paste0("https://avoin-paikkatieto.maanmittauslaitos.fi/geographic-names/features/v1/collections/placenames/items?spelling_case_insensitive=", 
            string, "&api-key=", key)
r <- GET(q)

res <- jsonlite::parse_json(r, simplifyVector = TRUE)$features$geometry
(nrow(res))
# 304

res_c <- res %>% 
  mutate(lon = sapply(coordinates, "[[", 1), 
         lat = sapply(coordinates, "[[", 2))

p <- ggplot() +
  stat_density2d(data = res_c, 
                 aes(x = lon, y = lat, fill = ..density..), 
                 geom = 'tile', contour = F, alpha = .9) +  
  scale_fill_distiller(palette = "Reds", direction = 1) +
  labs(title = 'Density of puna* in Finnish place names',
       caption = 'Data by National Land Survey of Finland | @ttso') +
  theme(axis.text = element_blank(),
        axis.title = element_blank(),
        axis.ticks = element_blank(),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "white"),
        plot.title = element_text(size = 35, hjust = 0.5),
        plot.caption = element_text(size = 20)) +
  guides(fill = "none")

p

ggsave("fi_puna.png", width = 35, height = 65, dpi = 72, units = "cm", device = 'png')
