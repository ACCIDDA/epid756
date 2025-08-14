
library(ggplot2) # load plotting package
library(stats) # load stats package

############ Using functions in R ########################

# Lets return to the iris data set
data("iris")
ggplot(iris) +
  geom_point(aes(x = Petal.Length, y = Sepal.Length, color = Species)) +
  theme_bw()

# Remember this from last class?
for (i in seq_len(nrow(iris))) {
  if (iris$Species[i] == "setosa") {
    iris$summary_value[i] <- iris$Sepal.Length[i] * iris$Sepal.Width[i]
    iris$metric[i] <- "Sepal area"
  } else {
    iris$summary_value[i] <- iris$Petal.Length[i] * iris$Petal.Width[i]
    iris$metric[i] <- "Petal area"
  }
}

summary(iris$summary_value[iris$metric == "Sepal area"])
summary(iris$summary_value[iris$metric == "Petal area"])

# Let's look at another way using base R functions
?apply

sepal_area <- apply(X = iris[iris$Species == "setosa", 1:2],
                    MARGIN = 1,
                    FUN = prod)
petal_area <- apply(X = iris[iris$Species != "setosa", 3:4],
                    MARGIN = 1,
                    FUN = prod)

# Some function arguments are optional
petal_area2 <- apply(X = iris[iris$Species != "setosa", 3:4],
                     MARGIN = 1,
                     FUN = prod,
                     simplify = FALSE)

# We can nest functions together
# Here we are looking at the correlation between petal and sepal area
cor(apply(X = iris[, 1:2], MARGIN = 1, FUN = prod),
    apply(X = iris[, 3:4], MARGIN = 1, FUN = prod))

ggplot() +
  geom_point(aes(x = apply(X = iris[, 1:2], MARGIN = 1, FUN = prod),
                 y = apply(X = iris[, 3:4], MARGIN = 1, FUN = prod),
                 color = iris$Species)) +
  labs(x = "Sepal area",
       y = "Petal area",
       color = "Species") +
  theme_bw()

############ Intro to regression in R ##########################

# Lets run a linear regression of petal length on sepal length
?lm
iris_model <- lm(Petal.Length ~ Sepal.Length + Species, data = iris)
summary(iris_model) # we can use summary() to look at lots of different objects!
str(iris_model) # look at the lm object

plot(iris_model$fitted.values, iris_model$residuals, pch = 19)
plot(iris_model) # some handy built-in diagnostic plots

# What if we want to make sure that our model is working the way we think it is?


############## Example: simulating data in R #####################

# Simple model with a single covariate
# Outcome = score on calculus test
# Predictor = # of hours studied
beta0 <- 30
beta1 <- 8
x <- runif(1000, 0, 24)
y <- rnorm(1000, beta0 + beta1 * x, 5)
summary(lm(y ~ x))

# TO DO: Do the same procedure above 1000 times and look at a histogram
# of the estimates for beta0 and beta1
beta0_hat <- c()
beta1_hat <- c()
for (i in 1:1000) {
  x <- runif(1000, 0, 24)
  y <- rnorm(1000, beta0 + beta1 * x, 2)
  lm_temp <- lm(y ~ x)
  beta0_hat <- c(beta0_hat, lm_temp$coefficients[1])
  beta1_hat <- c(beta1_hat, lm_temp$coefficients[2])
}

hist(beta0_hat, 20)
hist(beta1_hat, 20)

## How can we make this more flexible?
## Let's do an example with logistic regression

# Input values
n <- 1000 # sample size per simulation
n_sims <- 1000 # number of simulations
beta0 <- 0.5 # intercept on odds scale
beta <- c(1.2, 0.8, 2.3) # odds ratio for each covariate
# indicator for whether variable is "c"ontinuous or "d"ichotomous
var_type <- c("c", "c", "d")

res <- matrix(nrow = n_sims, ncol = length(beta) + 1)

for (s in seq_len(n_sims)) { # loop through simulations

  # create data frame to store output in
  x <- matrix(nrow = n, ncol = length(beta))

  for (i in seq_along(beta)) { # loop through covariates
    if (var_type[i] == "c") {
      ## TO DO: fill this in (hint check out `runif` function)
      x[, i] <- runif(n, 0, 10)
    } else if (var_type[i] == "d") {
      ## TO DO: fill this in (hint check out `sample` function)
      x[, i] <- sample(c(0, 1), n, replace = TRUE)
    } else {
      stop("Incompatible variable type ", var_type[i])
    }
  }

  # Now generate y values
  log_odds_y <- x %*% log(beta) + log(beta0)
  prob_y <- exp(log_odds_y) / (1 + exp(log_odds_y))

  # TO DO: sample final binary y values
  y <- as.numeric(runif(n) <= prob_y)

  lr <- glm(y ~ x, family = binomial(link = "logit"))
  res[s, ] <- lr$coefficients

}

hist(exp(res[, 1]), 20) # intercept, "true value" = 0.5
hist(exp(res[, 2]), 20) # X1, "true value" = 1.2
hist(exp(res[, 3]), 20) # X2, "true value" = 0.8
hist(exp(res[, 4]), 20) # X3, "true value" = 2.3
