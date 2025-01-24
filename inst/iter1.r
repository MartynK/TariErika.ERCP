# Simulates the proported study whch includes a pre-planned interim analysis
# and contains two arms with 5% and 30% probabilities for an event to occur
# along with a 25% dropout rate (censoring)

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

no_events <- function (beta, p, power, alpha = 0.05) {
  # ’beta’ the log hazard ratio
  # ’p’ the proportion of subjects in the treatment group
  # ’power’ the desired level of power
  # ’alpha’ the Type I error
  q <- 1 - p
  z.power <- qnorm(power)
  ca <- abs(qnorm(alpha / 2))
  round((ca + z.power)^2 / (p * q * beta^2))
}

set.seed(1234)
n       <- 100
nsim    <- 3
ir      <- 0.5
lambda  <- -1
event_control <- 0.3
event_trt     <- 0.05

# Make the max time of pot. events longer so we can right censor 28+days;
# was determined via mean prop. of censored values
simdata <- sim.survdata(N=n, T=365,
                        censor = .25, # This is the dropout rate
                        num.data.frames= nsim,
                        X = gen_arms(n)
                        , beta = 0 # No difference in the *shape* of the surv. curve
                        #, fixed.hazard = TRUE # DONT! It would condition the results to that function
)

i <- 1

data <- simdata[[i]]$data
data$ther <- as.factor(data$ther)

# dividing into two dfs for the two arms
data0 <- data[data$ther == 0,]
data1 <- data[data$ther == 1,]

# To simulate the scenario where no events occurred up until end of study,
# we set the event indicator to FALSE for the first X%;Y% of the observations
# in the two arms, respectively.
data0$failed[1:ceiling(n*(1-event_control))] <- FALSE
data0$y[1:ceiling(n*(1-event_control))]      <- 365

data1$failed[1:ceiling(n*(1-event_trt))] <- FALSE
data1$y[1:ceiling(n*(1-event_trt))]      <- 365

# reshuffling
data0 <- data0[sample(nrow(data0)),]
data1 <- data1[sample(nrow(data1)),]

# getting the interim data based on ir
data_interim <- rbind(data0[1:ceiling(n*ir),], data1[1:ceiling(n*ir),])
# and the additional data
data_additional <- rbind(data0[(ceiling(n*ir)+1):nrow(data0),],
                         data1[(ceiling(n*ir)+1):nrow(data1),])
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

#### ISSUE *** ISSUE *** ISSUE *** ISSUE *** ISSUE ***
# MY HWANG SHIH DECANI FUNCTION IS **BROKEN!!!**
succ_interim <- ifelse(pval_interim < hwang_shih_decani(ir,.05,lambda),
                       TRUE, FALSE)

# if interim is not successful, go to the final
if (succ_interim == FALSE) {
  s <- with(data_full, Surv( y, failed))
  mod2 <- coxph(s~ther, data_full, ties="breslow") # for comparability
  #summary(mod2)
  pval_final <- summary(mod2)$logtest[3]
  succ_final <- ifelse(pval_final < hwang_shih_decani(1,.05,lambda),
                       TRUE, FALSE)
} else {
  succ_final <- TRUE
}

# Bunch of visualizations

survminer::ggforest(mod2,data)

# We create a new data frame for each group
new_data <- data.frame(ther = levels(data$ther))  # i.e., "0" and "1"

# Create survival curves *based on the Cox model* (mod2)
cox_surv <- survfit(mod2, newdata = new_data)

# Plot
survminer::ggsurvplot(
  cox_surv,
  data = data,
  legend.title = "Treatment",
  legend.labs = c("Control (0)", "Treatment (1)"),
  xlab = "Time",
  ylab = "Survival Probability",
  conf.int = TRUE,       # show confidence intervals
  risk.table = TRUE      # show number at risk table
)
