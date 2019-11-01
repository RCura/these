library(tidyverse)

tableau <- readRDS("/home/robin/Dropbox/1_These/0_Manuscrit/chap3/src/tableau_etapes.Rds")
tableau %>%
  select(-Version, -Modifications, -Type) %>%
  rename(etape = `Étape`,
         param_meca = `Param/meca`,
         phenomene = `Phénomène`,
         typeParam = Type__1) %>%
  filter(Agent != "Chateaux") %>%
  filter(Agent != "Seigneur") %>%
  group_by(etape, Agent) %>%
  tally() %>%
  ggplot(aes(etape, n, fill = Agent)) +
  geom_col(position = "dodge") +
  facet_wrap(~ Agent)



tableau %>%
  filter(Agent != "Chateaux") %>%
  filter(Agent != "Seigneur")

  
  