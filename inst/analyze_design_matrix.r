source(here::here("inst","functions","load_stuff.r"))

des_mat <- readRDS(here::here("data","des_mat.rds"))

des_mat_filtered <- des_mat %>%
  filter(event_trt == 0.05,
         ir == .6,
         event_control == 0.3,
         dropouts == .25)

model_final_filtered <- glm(
  cbind(succ_final, nsim - succ_final) ~ ns(n,df = 2) + ns(gamma,df=2),
  data = des_mat_filtered,
  family = binomial
)

summary(model_final_filtered)

#model_final_filtered %>% effects::predictorEffects() %>% plot()

##

model_final <- glm(
  cbind(succ_final, nsim - succ_final) ~ ns(n,df = 2) + ns(gamma,df=1) +
    (event_trt + event_control + dropouts + ir) * ns(n,df=2),
  data = des_mat,
  family = binomial
)

# model_final %>% effects::predictorEffects() %>% plot()

pr <- expand.grid(
  n = seq(0, 65, length.out = 100),
  gamma = -0.5,
  event_trt = c(.05,.10),
  event_control = c(.25,.30),
  dropouts = c(.15,.25,.35),
  ir = c(.55,.65,.75)
)

pr$pr <- predict(model_final, newdata = pr, type = "response")

# Produce a figure at the desired levels
fig_1 <-
  pr %>%
    filter(
      gamma == -0.5,
      event_trt == .05,
      event_control == .3,
      dropouts == .25,
      ir == .65
    ) %>%
    ggplot( aes(n, pr)) +
    geom_line() +
    theme_minimal() +
    geom_hline(yintercept = .8, linetype = 2, color = "salmon4") +
    geom_vline(xintercept = 46, linetype = 2, color = "salmon4") +
    scale_y_continuous(labels = scales::label_percent(accuracy = 1),
                       breaks = c(.5,.7,.8,.9)) +
    scale_x_continuous(breaks = c(0,10,20,30,40,46,50,60)) +
    labs( x = "Sample size (per arm)",
          y = "Probability of success (Power)",
          title = "Power curve for standard assumptions",
          )

# Produce a figure faceted for the different levels of dropouts
fig_2 <-
  pr %>%
    filter(
      gamma == -0.5,
      event_trt == .05,
      event_control == .3,
      ir == .65
    ) %>%
    ggplot( aes(n, pr,
                color = factor(dropouts),
                group = paste0(event_control,
                               dropouts, ir))) +
    geom_line() +
    geom_hline(yintercept = .8, linetype = 2, color = "salmon4") +
    theme_minimal() +
    scale_y_continuous(labels = scales::label_percent(accuracy = 1),
                       breaks = c(.5,.7,.8,.9))  +
  labs( x = "Sample size (per arm)",
        y = "Probability of success (Power)",
        title = "Power curve for different ratios of dropouts",
        color = "Prop.censored"
  )

# produce a figure faceted by different ratios of event_trt & control
fig_3 <-
  pr %>%
    filter(
      gamma == -0.5,
      dropouts == .25,
      ir == .65
    ) %>%
    ggplot( aes(n, pr,
                color = factor(event_trt),
                linetype = factor(event_control),
                group = paste0(event_control,
                               event_trt,
                               dropouts, ir))) +
    geom_line() +
    geom_hline(yintercept = .8, linetype = 2, color = "salmon4") +
    theme_minimal() +
    scale_y_continuous(labels = scales::label_percent(accuracy = 1),
                       breaks = c(.5,.7,.8,.9)) +
  labs( x = "Sample size (per arm)",
        y = "Probability of success (Power)",
        title = "Power curve for different ratios of events",
        color = "Event rate in treatment arm",
        linetype = "Event rate in control arm"
  )

# produce a figure faceted by different information ratios
fig_4 <-
  pr %>%
    filter(
      gamma == -0.5,
      dropouts == .25,
      event_trt == .05,
      event_control == .3
    ) %>%
    ggplot( aes(n, pr,
                color = factor(ir),
                group = paste0(event_control,
                               event_trt,
                               dropouts, ir))) +
    geom_line() +
    geom_hline(yintercept = .8, linetype = 2, color = "salmon4") +
    theme_minimal() +
    scale_y_continuous(labels = scales::label_percent(accuracy = 1),
                       breaks = c(.5,.7,.8,.9)) +
  labs( x = "Sample size (per arm)",
        y = "Probability of success (Power)",
        title = "Power curve for different place of the interim analysis",
        color = "interim sampsize /\nfinal sampsize"
  )

