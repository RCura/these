library(tidyverse)

abc <- tibble::tribble(
  ~Var1, ~Var2, ~Var3,   ~Var4,
      1,    2.5,    80, "Grand",
      2,    4.4,   100, "Petit",
      5,    2.7,    30, "Petit",
     10,     6,    20, "Moyen",
     30,    1.2,    80, "Grand",
     50,    3.1,    30, "Moyen"
  )


ggplot(data = abc) +
  aes(x = Var1, y = Var2,
      size = Var3, colour = Var4) +
  geom_point() +
  scale_x_log10() +
  labs(title = "Titre du graphique",
       subtitle = "Sous-titre",
       caption = "Sources",
       x = "Variable 1", y = "Variable 2") +
  theme_light()
