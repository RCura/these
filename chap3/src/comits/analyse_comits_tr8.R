library(readxl)

library(tidyverse)

comits <- read_excel("comits.xlsx",
                     col_types = c("text", "text", "text",
                                   "date", "numeric", "text", "numeric",
                                   "numeric", "numeric", "numeric")) %>%
  rownames_to_column() %>%
  mutate(comitNb = rank(desc(as.numeric(rowname)))) %>%
  mutate(SommeNb = `NbMécanisme` + NbParams + NbRefactoring + NbAjustRegles) %>%
  filter(SommeNb > 0) %>%
  mutate(comitNb = n():1) %>%
  mutate_at(funs(ratio =  ./SommeNb), .vars = vars(`NbMécanisme`, NbParams, NbRefactoring, NbAjustRegles)) %>%
  gather(key = Type, value = Nb, `NbMécanisme_ratio`, NbParams_ratio, NbRefactoring_ratio, NbAjustRegles_ratio)

library(pBrackets) 

bracketsGrob <- function(...){
  l <- list(...)
  e <- new.env()
  e$l <- l
  grid:::recordGrob(  {
    do.call(grid.brackets, l)
  }, e)
}

thisPlot <- ggplot(comits %>% filter(Type != "NbRefactoring_ratio")) +
  geom_col(data = comits %>% filter(Type == "NbMécanisme_ratio"), aes(comitNb, Nb, fill = Type)) +
  geom_col(data = comits %>% filter(Type == "NbParams_ratio"), aes(x = comitNb, y = -Nb, fill = Type)) +
  scale_fill_grey(name = "Type de modification",start = .1, end = .6,
                      labels = c("Ajout de mécanisme", "Modification des valeurs de paramètre")) +
  scale_y_continuous(name = "Composition de chaque modification",
                     breaks = c(-1, -.5, 0, .5, 1), 
                     labels = paste0(c(100, 50, 0, 50, 100), "%")) +
  scale_x_continuous(name = "Modifications du modèle") +
  labs(title = "Évolution de la composition des modifications apportées au code du modèle",
       subtitle = "Parmi les commits ayant amené une modification fonctionnelle au modèle",
       caption = 'Relevés effectués le 06/03/2017, sur les versions 0 à 3 de SimFeodal - Commit #76372d0') +
  annotation_custom(bracketsGrob(0.22, .25, 0.04, .25, h=0.05, lwd=2, col="black")) + # Période A 
  annotate("text", x = 10.75, y = -0.775, label = "A",  size = 6) +
  annotation_custom(bracketsGrob(0.31, .51, .37, .51, h=0.05, lwd=2, col="black")) + # Période B 
  annotate("text", x = 36.5, y = 0.22, label = "B",  size = 6) +
  annotation_custom(bracketsGrob(0.42, .49, .39, .49, h=0.05, lwd=2, col="black")) + # Période C 
  annotate("text", x = 44.5, y = -0.25, label = "C",  size = 6) +
  annotation_custom(bracketsGrob(0.58, .51, .615, .51, h=0.05, lwd=2, col="black")) + # Période D 65-75
  annotate("text", x = 68, y = 0.22, label = "D",  size = 6) +
  annotation_custom(bracketsGrob(0.695, .49, .675, .49, h=0.05, lwd=2, col="black")) + # Période E 80-82
  annotate("text", x = 78.5, y = -0.25, label = "E",  size = 6) +
  geom_segment(x = 26, y = -1, xend = 26, yend = 1, linetype = "dashed", colour = "grey30", alpha = .2) +
  annotate("text", x = 27.5, y = 0, label = "Passage de 1000 à 4000 FP",  size = 3, angle = 90) +
  theme_light() +
  theme_minimal() +
  theme(legend.position="bottom")

thisPlot

ggsave(thisPlot, filename = "plotComits.pdf",  width = 20, height = 10, units = "cm")




b1 <- bracketsGrob(0.33, 0.05, 0, 0.05, h=0.05, lwd=2, col="red")
