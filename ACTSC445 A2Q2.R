### ACTSC445 A2 Q2 and Q5 ###

td <- read.csv("TD.csv")
td_adj_ts <- ts(data=td["Adj.Close"])

### part b ### 

# calculate negative daily log-returns
n <- length(td_adj_ts)
td_lrn <- -log(td_adj_ts[-1]/td_adj_ts[-n])

# fit normal dist. for sequence of negative log-returns
x <- seq(min(td_lrn), max(td_lrn), length = n)
y <- dnorm(x, mean = mean(td_lrn), sd = sd(td_lrn))
plot(x, y, 
     main = "Normal distribution for sequence of negative log-returns",
     xlab = "Negative Log Return", ylab = "Probability Density",
     pch=16, cex = 0.8, cex.main = 0.8)


### part c ###

# Develop an estimate for VaR_0.95 based on the fit normal dist.
# since td_lrn ~ N(u, sigma^2) we calculate VaR_0.95 as:
var_95 <- mean(td_lrn) + qnorm(0.95)*sd(td_lrn)


### part d ###

# Apply binomial distribution method to backtest VaR estimate from part c

# R is the number of losses larger than VaR estimate that is, all 
# log returns greater than var_95
R <- length(td_lrn[td_lrn > var_95])

# T is the total number of observations, note that n already contains the
# total number of stocks, so T = n - 1
T_tot <- n-1

# we let H_0:  VaR_0.95 = Var_0.95_hat
# so R ~ Bin(T,0.05) and it can be approximated as R ~ N(0,1) 
# we then calculate the observed value R_norm
R_norm <- (R - 0.05*T_tot) / sqrt(0.05*(1-0.05)*T_tot)

# find the p-value, p_val = 0.653505
# We see that it is greater than 0.1
p_val <- 2 * (pnorm(R_norm))


### part e ###

X_t = 0
Y_t = 0
n_00 = 0
n_01 = 0
n_10 = 0
n_11 = 0

# go through each day and check for states: n_00, n_01, n_10, n_11
for (t in 1:(T_tot)) {
  # stop loop on last day
  if (t == 251) {
    break
  }
  # check if exception occurs on day t
  if (td_lrn[t] > var_95) {
    X_t = 1
  } else {
    X_t = 0
  }
  # check if exception occurs on day t+1
  if (td_lrn[t+1] > var_95) {
    Y_t = 1
  } else {
    Y_t = 0
  }
  # check the contingency events
  # i,j = 0
  if (X_t == 0 && Y_t == 0) {
    n_00 = n_00 + 1
  }
  # i=0,j=1
  else if (X_t == 0 && Y_t == 1) {
    n_01 = n_01 + 1
  }
  # i=1,j=0
  else if (X_t == 1 && Y_t == 0) {
    n_10 = n_10 + 1
  }
  # i,j = 1
  else {
    n_11 = n_11 + 1
  }
}

# calculate pi values
pi_0 <- n_01 / (n_00 + n_01)
pi_1 <- n_11 / (n_10 + n_11)
pi <- (n_01 + n_11) / (n-1)

# calculate LR 
LR_ind_num <- ((1-pi)^(n_00 + n_10)) * pi^(n_01 + n_11)
LR_ind_den <- ((1-pi_0)^(n_00)) * (pi_0^(n_01)) * 
              ((1-pi_1)^(n_10)) * (pi_1^(n_11))
LR_ind <- -2*log(LR_ind_num / LR_ind_den)

# note that the lr_qchi = 3.841459 
lr_qchi <- qchisq(0.95, df=1) 


### Q5 ###

# b)

# i) exact probability value
q5_n <- c(10,20,30,40,50)
q5_m <- c(60,100)
pr_exact = c()
pr_ij = 0

for (i in q5_m) {
  for (j in q5_n) {
    pr_ij = (1 - exp(-i/10))^j
    pr_exact = c(pr_exact, pr_ij)
  }
  pr_exact = c(pr_exact, "next")
}

# ii) approx. probability value
pr_approx = c()

for (i in q5_m) {
  for (j in q5_n) {
    pr_ij = exp(-exp(-(i-(10*log(j)))/10))
    pr_approx = c(pr_approx, pr_ij)
  }
  pr_approx = c(pr_approx, "next")
}
  
  
  
  
  
  
  













