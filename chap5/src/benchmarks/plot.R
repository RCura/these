library(tidyverse)
library(ggalt)

bench_data <- read_csv("plot_updated.csv")

bench_plot <- bench_data %>%
  gather(key = Operation, value = Time, `Load/Read`,Aggregation, Join) %>%
  mutate(Operation = case_when(
    Operation == "Load/Read" ~ "Insertion de données",
    Operation == "Join" ~ "Jointure",
    Operation == "Aggregation" ~ "Aggrégation de données"
  )) %>%
  mutate(System = if_else(`SQL?` == "yes",
                          true = paste0(System, "\n(SQL)"),
                          false = paste0(System, "\n(non SQL)"))) %>%
  mutate(Operation = factor(Operation, levels = c("Insertion de données",
                                                  "Aggrégation de données",
                                                  "Jointure"))) %>%
  mutate(Time = Time + 1) %>%
  mutate(System = fct_reorder(System, Time, .fun = min))

ticks <- c(0.5,1,3,5,10,30,60,120,300,600)
ticks2 <- ticks - 1

isSQL <- c(rep("black", times = 2),
           "grey",
           rep("black", times = 2),
           rep("grey", times= 2),
           rep("black", times = 5))

ggplot(bench_plot) +
  aes(x = System, y = Time, color = Type, point.colour=Type) +
  geom_lollipop(point.size=2) +
  facet_wrap(~Operation, nrow = 1) +
  scale_x_discrete(position ="top") +
  scale_y_log10(breaks = ticks2, labels = ticks, limits = c(.9,500)) +
  labs(title= "Performances comparées de SGBD",
       subtitle = "Temps nécessaires à l'execution de requêtes",
       caption = "R. Cura (2018), d'après S. Pafka (2017)
       * MapD (CPU) : Benchmark mené sur un système moins puissant que les autres",
       x = "Systèmes de Gestion de Bases de Données",
       y = "Durée de la requête [secondes]\n (Échelle logarithmique)") +
  coord_flip() +
  theme(legend.position="bottom",
        axis.text.x = element_text(angle = 45, hjust = 1),
        axis.text.y = element_text(colour = isSQL)) +
  guides(colour = guide_legend(title = "Types de Bases de Données",
                               override.aes = list(size=6),
                               title.position = "top",
                               title.hjust = .5,
                               label.position = "bottom"))
ggsave(last_plot(), filename = "benchmark_results.pdf", width = 20, height = 25, units = "cm")
