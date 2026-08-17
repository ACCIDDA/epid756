#### Set-up ####

# install.packages('deSolve') # install deSolve package if never used before
library(deSolve) # run every time you start a new R session to load the package
library(tidyverse)
options(scipen = 999) # setting to print numbers without scientific notation

# pseudo-code for simulating epidemic dynamics with compartmental models - in four steps:

# (1) use ordinary differential equations to represent dynamics: transitions into and out of population compartments 
# transitions <- entry rates – exit rates

# (2) specify parameters for differential equations 
# parameters <- fixed value or draw value from probability distribution

# (3) initialize population 
# states <- starting size of each compartment
# times <- time points to simulate dynamics into and out of compartments

# (4) simulate population by solving system of differential equations for given states, times, parameters 
# solve(states, times, parameters, transitions)

#### Susceptible-Infected (SI) Model ####

# in our model world, a new infection is introduced into a completely susceptible population
# assume that the contact rate per day is 1; the probability of infection given contact is 0.25
# therefore, the effective contact rate (beta) per day is 0.25
# there is no immunity or recovery
# we will simulate epidemic dynamics over 60 days using a compartmental structure

# (1) specify differential equations representing transitions into and out of compartments
# compartments: susceptible (S), infected (I)
# movements: infection (from S to I)
si_model <- function(times, states, params){ 
  # creating a list of states and parameters to then reference
  with(as.list(c(states, params)), { 
    # total modeled population
    N <- S+I
    # force of infection
    lambda <- beta*(I/N)
    # differential equations
    dS <- -lambda*S
    dI <- lambda*S
    # return the transition rates
    list(c(dS, dI))
  })
}

# (2) specify parameters for differential equations 
params <- c(beta = 0.25) # effective contact rate, per day

# (3) initialize population 
states <- c(S = 99, I = 1) 
times <- seq(from = 1, to = 60, by = 1)

# (4) simulate population by solving differential equations for given states, times, parameters
?ode()
ode(y = states, times = times, func = si_model, parms = params)

# assign ode output to new object
model1 <- as.data.frame( #  # converting to data.frame to easily extract columns
  ode(y = states, times = times, func = si_model, parms = params)
) 

model1 %>% 
  pivot_longer(cols = c(S, I), names_to = "compartment", values_to = "count") %>% 
  mutate(compartment = factor(compartment,
                              levels = c("S", "I"), 
                              labels = c("Susceptible", "Infected"))) %>% 
  ggplot(aes(x = time, y = count, color = compartment)) +
  geom_line() + 
  labs(x = "Days", y = "Count", color = "Compartment") +
  theme_classic() 

#### Susceptible-Infected-Recovered (SIR) Model ####

# now, an acute, immunizing infection is re-introduced into the population
# 75% of the population has pre-existing immunity, 25% is susceptible
# while people are infected and infectious, the effective contact rate per day is 2 (beta)
# people are infected and infectious for 5 days before recovery, making the recovery rate (gamma) = 1/5
# let's again simulate epidemic dynamics over 60 days

# (1) specify differential equations representing movements into and out of compartments
# compartments: susceptible (S), infected (I), recovered (R)
# movements: infection (from S to I), recovery (from I to R)
sir_model <- function(times, states, params){ 
  # creating a list of states and parameters to then reference
  with(as.list(c(states, params)), { 
    # total modeled population
    N <- S+I+R
    # force of infection
    lambda <- beta*(I/N)
    # differential equations
    dS <- -lambda*S
    dI <- (lambda*S)-(gamma*I)
    dR <- gamma*I
    # return the transition rates
    list(c(dS, dI, dR))
  })
}

# (2) specify parameters for differential equations 
params <- c(beta = 2, gamma = 1/5) 

# (3) initialize population 
states <- c(S = 250, I = 1, R = 749) 
times <- seq(from = 1, to = 60, by = 1)

# (4) simulate population by solving differential equations for given states, times, parameters
model2 <- as.data.frame(
  ode(y = states, times = times, func = sir_model, parms = params)
) 

model2 %>% 
  pivot_longer(cols = c(S, I, R), names_to = "compartment", values_to = "count") %>% 
  mutate(compartment = factor(compartment,
                              levels = c("S", "I", "R"), 
                              labels = c("Susceptible", "Infected", "Recovered"))) %>% 
  ggplot(aes(x = time, y = count, color = compartment)) +
  geom_line() + 
  labs(x = "Days", y = "Count", color = "Compartment") +
  theme_classic() 
