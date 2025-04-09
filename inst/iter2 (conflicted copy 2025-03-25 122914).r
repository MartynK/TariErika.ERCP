# Simulates the proported study whch includes a pre-planned interim analysis
# and contains two arms with 5% and 30% probabilities for an event to occur
# along with a 25% dropout rate (censoring)

# Introduces a des_mat design matrix which describes the different variables
# influencing the outcome

library(coxed)
library(survival)
library(ggfortify)

source(here::here("inst","functions","load_stuff.r"))

gen_arms <- function(n) {
  return(
    data.frame( ther =
                  c(
                    rep(0.0 , n),
                    rep(1   , n)
                  )))
}

des_mat <- expand.grid(
  n = c(20, 30, 40, 50, 60),
  nsim = c(100),
  ir = c(0.5),
  gamma = c(-1, 0, 1),
  event_control = c(0.3),
  event_trt = c(0.05)
) %>%
  mutate( succ_interim = 0,
          succ_final = 0
  )


pb <- txtProgressBar()
for (i in 1:nrow(des_mat)) {

  # Get the pval tresholds for interim/final analysis
  design <- rpact::getDesignGroupSequential(kMax = 2,
                                            typeOfDesign = "asHSD",
                                            gammaA = des_mat$gamma[i])
  pvals  <- design[["stageLevels"]] * 2


  # Make the max time of pot. events longer so we can right censor 28+days;
  # was determined via mean prop. of censored values
  simdata <- sim.survdata(N=des_mat$n[i], T=365,
                          censor = .25, # This is the dropout rate
                          num.data.frames= des_mat$nsim[i],
                          X = gen_arms(des_mat$n[i])
                          , beta = 0 # No difference in the *shape* of the surv. curve
                          #, fixed.hazard = TRUE # DONT! It would condition the results to that function
  )


  for (j in 1:des_mat$nsim[i]) {

    data <- simdata[[j]]$data
    data$ther <- as.factor(data$ther)

    # dividing into two dfs for the two arms
    data0 <- data[data$ther == 0,]
    data1 <- data[data$ther == 1,]

    # To simulate the scenario where no events occurred up until end of study,
    # we set the event indicator to FALSE for the first X%;Y% of the observations
    # in the two arms, respectively.
    no_events_control <- rbinom(1, des_mat$n[i], 1 - des_mat$event_control[i])

    data0$failed[1:no_events_control] <- FALSE
    data0$y[1:no_events_control]      <- 365

    no_events_trt <- rbinom(1, des_mat$n[i], 1 - des_mat$event_trt[i])

    data1$failed[1:no_events_trt] <- FALSE
    data1$y[1:no_events_trt]      <- 365

    # reshuffling
    data0 <- data0[sample(nrow(data0)),]
    data1 <- data1[sample(nrow(data1)),]

    # getting the interim data based on ir
    data_interim <- rbind(data0[1:ceiling(des_mat$n[i]*des_mat$ir[i]),],
                          data1[1:ceiling(des_mat$n[i]*des_mat$ir[i]),])
    # and the additional data
    data_additional <- rbind(data0[(ceiling(des_mat$n[i]*des_mat$ir[i])+1):nrow(data0),],
                             data1[(ceiling(des_mat$n[i]*des_mat$ir[i])+1):nrow(data1),])
    data_full <- rbind(data_interim, data_additional)


    # Doing the interim analysis
    s <- with(data_interim, Surv( y, failed))

    # # going the log-rank way
    # mod1 <- survfit(s ~ ther, data_interim)
    # summary(mod1)
    # autoplot(mod1)

    # going the cox way
    mod2 <- coxph(s~ther,
                  data_interim, ties="breslow") # for comparability
    #summary(mod2)

    pval_interim <- summary(mod2)$logtest[3]

    # Success at interim?
    succ_interim <- ifelse(pval_interim < pvals[1], TRUE, FALSE)
    des_mat$succ_interim[i] <- des_mat$succ_interim[i] + succ_interim


    # if interim is not successful, go to the final
    if (succ_interim == FALSE) {
      s <- with(data_full, Surv( y, failed))
      mod2 <- coxph(s~ther, data_full, ties="breslow") # for comparability
      #summary(mod2)
      pval_final <- summary(mod2)$logtest[3]
      succ_final <- ifelse(pval_final <  pvals[2],
                           TRUE, FALSE)
    } else {
      succ_final <- TRUE
    }

    des_mat$succ_final[i] <- des_mat$succ_final[i] + succ_final

  }
  setTxtProgressBar(pb, i/nrow(des_mat))
}
close(pb)
