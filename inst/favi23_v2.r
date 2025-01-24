library(coxed)
library(survival)
library(ggfortify)

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
n       <- 10
n_short <- 34
nsim    <- 3

# Make the max time of pot. events longer so we can right censor 28+days;
# was determined via mean prop. of censored values
simdata <- sim.survdata(N=n, T=180, censor = 0.5,
                        num.data.frames= nsim,
                        X = gen_arms(n)
                        , beta = log(2)
                        #, fixed.hazard = TRUE # DONT! It would condition the results to that function
                        )

cens <- c()
res_bestarm <- c()
res_biggestcox <- c()
reass_n <- c()

alphacox <- .01
zcox     <- qnorm(1-(alphacox/2))

alphacox2 <- .05 # theyadd up (@ worst)
zcox2     <- qnorm(1-(alphacox2/2))

res_median <- c()
res_zcox <- c()
res_endcox <- c()
res_endmedian <- c()

pb <- txtProgressBar()
for (i in 1:nsim) {

  data <- simdata[[i]]$data
  data$ther <- as.factor(data$ther)
  #data$failed[data$y > 28] <- FALSE # right censoring not cured patients
  cens <- c(cens, (4*n - sum(data$failed))/(4*n))
  data$y[data$y > 28] <- 28

  data_short <- data[0,]
  data_long  <- data[0,]

  for (j in 1:length(unique(data$ther))) {
    data_short <- rbind( data_short, data[data$ther == unique(data$ther)[j],][1:n_short,])
    #data_long  <- rbind( data_short, data[data$ther == unique(data$ther)[i],][(n_short + 1):n,])
  }

  data_long <- data
  data <- data_short

  s <- with(data, Surv( y, failed))
  mod1 <- survfit(s ~ ther, data)
  # summary(mod1)
  # plot(mod1)
  # autoplot(mod1)

  mod2 <- coxph(s~ther, data,ties="breslow") # for comparability
  summary(mod2)

  wmod2 <- survfit(mod2)
  autoplot(wmod2)
  autoplot(mod1)

  res_bestarm <- c(res_bestarm,
                   ifelse(
                    which(summary(mod2)$conf.int[1:3,1] ==
                            max(summary(mod2)$conf.int[1:3,1]))[[1]] == 3,
                    TRUE, FALSE))

  res_biggestcox <- c(res_biggestcox,
                      ifelse(summary(mod2)$coefficients[3,4] > zcox, TRUE, FALSE))

  res_zcox <- c(res_zcox,summary(mod2)$coefficients[3,4])

  res_median <- c(res_median,
                  ifelse( quantile(mod1, probs = .5)$lower[1] > quantile(mod1, probs = .5)$upper[4], TRUE, FALSE))

  reass_n <- c( reass_n, no_events( beta = max(summary(mod2)$coefficients[,1]), p = .5, power = .8, alpha = .04))

  sl <- with(data_long, Surv( y, failed))
  mod3 <- coxph(sl~ther, data_long, ties="breslow") # for comparability
  mod4 <- survfit(sl ~ ther, data_long)
  oldbestarm <- which(summary(mod2)$coefficients[,1] == max(summary(mod2)$coefficients[,1]))
  res_endcox <- c(res_endcox,
                      ifelse(summary(mod3)$coefficients[oldbestarm,4] > zcox2, TRUE, FALSE))
  res_endmedian <- c(res_endmedian,
                  ifelse( quantile(mod4, probs = .5)$lower[1] > quantile(mod4, probs = .5)$upper[oldbestarm], TRUE, FALSE))


  setTxtProgressBar(pb,i/nsim)
}
close(pb)

# no_events( beta = log(1.75), p = .5, power = .9, alpha = .05)
# 134


binom.test(sum(res_bestarm), nsim)

binom.test(sum(res_biggestcox), nsim)

binom.test(sum(res_median,na.rm=T), nsim)

binom.test(sum(res_endcox), nsim)

# Csak a 2. lépcsőben...
binom.test( sum(res_endcox[res_biggestcox == FALSE]), sum(res_biggestcox == FALSE))

binom.test(sum(res_endmedian, na.rm = T), nsim)

median(cens)
mean(cens)
hist(cens)

hist(res_zcox)
quantile(res_zcox,.05)

hist(reass_n)
mean(reass_n)
median(reass_n)

