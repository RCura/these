library(tidyverse)

abandonment_data <- read.csv("data_section.io.csv", dec=",", stringsAsFactors = FALSE)

ggplot(abandonment_data) +
  aes(x = Load_Time, y = Number_pages_viewed) +
  geom_point() +
  geom_line() +
  scale_y_continuous(limits = c(1,10), breaks = c(2,4,6,8,10),
                     minor_breaks = c(1,3,5,7,9)) +
  labs(title = "Nombre de pages visitées par site selon leur durée d'affichage",
       subtitle = "Plus le temps de chargement est long, moins le visiteur consultera de pages",
       caption = "Sources : D'après section.io (www.section.io/blog/page-load-time-bounce-rate/)",
       x = "Temps de chargement d'une page [secondes]",
       y = "Nombre de pages vues")

ggsave(last_plot(), filename = "abandon_pages.pdf", width = 20, height = 10, units = "cm")
