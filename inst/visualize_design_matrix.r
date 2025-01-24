library(ggplot2)


des_mat %>%
  ggplot( aes(x = n, succ_final, color = gamma,
              group = gamma, alpha = succ_interim)) +
  geom_point() +
  geom_line() +
  theme_minimal()
