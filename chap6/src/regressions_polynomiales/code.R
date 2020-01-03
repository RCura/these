library(tidyverse)

foo <- tibble(X = 1:50) %>%
	mutate(Y = X * (2+rnorm(n = 50, mean = 0, sd = 1)) + rnorm(n =50, mean = 0, sd = 5))
#saveRDS(foo, "foo.RDS")
foo <- readRDS("foo.RDS")


r2 <- list()
models <- list()
for (i in 1:10){
  models[i] <- list(lm(data = foo, Y ~ poly(X, i)))
	r2[i] <- summary(models[[i]])$r.squared
}


p1 <- r2 %>%
  unlist() %>%
  enframe() %>%
  rename_all(~c("Degré polynomial", "R2")) %>%
  ggplot() +
  aes(`Degré polynomial`, R2) +
  geom_point(aes(colour = as.factor(`Degré polynomial`))) +
  scale_colour_viridis_d("Degré polynomial", guide = FALSE) +
  scale_x_continuous(breaks = seq(0, 10, 2)) +
  theme_minimal()

foobar <- foo
for (i in 1:10){
  foobar[paste0("deg", i)] <- predict(models[[i]])
}
p2 <- foobar %>%
  gather(Degre, Prediction, -X, -Y) %>%
  mutate(Degre = str_remove_all(Degre, "deg")) %>%
  mutate(`Degré polynomial` = as.numeric(Degre)) %>%
  ggplot() +
  geom_line(aes(X, Prediction, colour = as.factor(`Degré polynomial`), group = `Degré polynomial`), alpha = .5) +
  geom_point(aes(X, Y)) +
  scale_colour_viridis_d("Degré polynomial", guide = guide_legend(nrow = 1)) +
  theme_minimal() +
  theme(legend.position = "bottom")

  

library(patchwork)

layout <- "
BBBB
AAAA
AAAA
AAAA
"


p2 + p1  + plot_layout(design = layout, guides = "collect") & theme(legend.position = 'bottom')


ggsave(last_plot(), file="regression_polynomiale.svg", width=20, height=10, units = "cm")
