
#### Set-up ####

library(deSolve) # run every time you start a new R session to load packages
library(tidyverse)
options(scipen = 999) # setting to print numbers without scientific notation

# function for the same ggplot code we'll use to look at output of each model
# in function call, specify output dataframe which must contain time, count, compartment columns
plot_model_sims <- function(output){
  ggplot(data = output, aes(x = time, y = count, color = compartment)) +
    geom_line() + 
    labs(x = "Days", y = "Count", color = "Compartment") +
    theme_classic() 
}

#### Susceptible-Exposed-Infected-Recovered (SEIR) Model ####

# now, we are incorporating a 4th "pre-infectious" state: exposed to infection but not yet infectious (E)
# once infected, susceptible individuals move to the exposed state
# exposed individuals become infectious at a rate delta 
# in new function, write differential equations for rate of change in numbers of S, E, I, R
seir_model <- function(times, states, params){ 
  # creating a list of states and parameters to then reference
  with(as.list(c(states, params)), { 
    # total modeled population
    N <- S+E+I+R
    # force of infection
    lambda <- beta*(I/N)
    # differential equations assuming steady state population: 
    # birth of new s = mortality of s, e, i, r
    dS <- mu*(N-S) - lambda*S 
    dE <- lambda*S - (mu+delta)*E
    dI <- delta*E - (mu+gamma)*I
    dR <- gamma*I - mu*R
    # return the transition rates
    list(c(dS, dE, dI, dR))
  })
}

# specify parameters for differential equations using 1-day time units
params <- c(beta = 0.5, # daily effective contact rate is 0.5
            delta = 1/7, # people are pre-infectious for 1 week on average
            gamma = 1/14, # people are infectious for 2 weeks before recovering
            mu = 1/(60*365.25)) # life expectancy is 60 years

# completely susceptible population during 180 days, recording population sizes each day
states <- c(S = 999, E = 1, I = 0, R = 0) # order matters! must match list of transition rates
times <- seq(from = 0, to = 180, by = 1)

# simulate population by solving differential equations for given states, times, parameters
seir_output <- as.data.frame(
  ode(y = states, times = times, func = seir_model, parms = params)
) 
# manipulate data for plotting: make longer, specify order in which compartments are displayed and their labels
seir_output %>% 
  pivot_longer(cols = c(S, E, I, R), names_to = "compartment", values_to = "count") %>% 
  mutate(compartment = factor(compartment, 
                              levels = c("S", "E", "I", "R"),
                              labels = c("Susceptible", "Exposed", "Infected", "Recovered"))) %>% 
  plot_model_sims()

# what if we really aren't sure what the duration of the pre-infectious period is? how much does our uncertainty matter? 
duration <- seq(from = 1, to = 14, by = 1) # duration could range from 1 day to 2 weeks

# loop through each value of delta and rerun ode function
output <- tibble() # empty dataframe to collect results
for(i in 1:length(duration)){ # rerun ode function for each delta (1/duration)
  params <- c(delta = 1/duration[i], beta = 0.5, gamma = 1/14, mu = 1/(60*365.25)) # beta, gamma, mu stay constant
  out <- as.data.frame(ode(y = states, times = times, func = seir_model, parms = params))  
  out$delta <- params["delta"] # keep track of delta used for each simulation
  output <- bind_rows(output, out) # append each simulation to output
}
# visualize output from simulations
output %>% 
  group_by(delta) %>% 
  pivot_longer(cols = c(S, E, I, R), names_to = "compartment", values_to = "count") %>% 
  mutate(duration_exposed = 1/delta,
         compartment = factor(compartment,
                              levels = c("S", "E", "I", "R"), 
                              labels = c("Susceptible", "Exposed", "Infected", "Recovered"))) %>% 
  ggplot(aes(x = time, y = count, group = duration_exposed, color = duration_exposed)) +
  geom_line() + 
  facet_wrap(~compartment) +
  labs(x = "Days", y = "Count", color = "Days exposed") +
  scale_color_continuous(n.breaks = 8, low = "red", high = "blue") +
  theme_classic() 

#### Adding Vaccination ####

# we can add a vaccinated state (V) to incorporate the process of vaccination
# susceptible individuals are vaccinated at a rate v 
# for simplicity, we assume the vaccine is 100% effective: once vaccinated, individuals cannot become infected
# in new function, write differential equations for rate of change in numbers of S, E, I, R, V
seirv_model <- function(times, states, params){ 
  # creating a list of states and parameters to then reference
  with(as.list(c(states, params)), { 
    # total modeled population
    N <- S+E+I+R+V
    # force of infection
    lambda <- beta*(I/N)
    # differential equations assuming steady state population: 
    # birth of new s = mortality of s, e, i, r, v
    dS <- mu*(N-S) - (v+lambda)*S 
    dE <- lambda*S - (mu+delta)*E
    dI <- delta*E - (mu+gamma)*I
    dR <- gamma*I - mu*R
    dV <- v*S - mu*V
    # return the transition rates
    list(c(dS, dE, dI, dR, dV))
  })
}
# specify parameters for differential equations using 1-day time units - same as before but adding vaccination rate
params <- c(beta = 0.5, # daily effective contact rate is 0.5
            delta = 1/7, # people are pre-infectious for 1 week on average
            gamma = 1/14, # people are infectious for 2 weeks before recovering
            mu = 1/(60*365.25), # life expectancy is 60 years
            v = 0.01) # daily vaccination rate

# completely susceptible population during 180 days, recording population sizes each day
states <- c(S = 999, E = 1, I = 0, R = 0, V = 0) 
times <- seq(from = 0, to = 180, by = 1)

# simulate population by solving differential equations for given states, times, parameters
seirv_output <- as.data.frame(
  ode(y = states, times = times, func = seirv_model, parms = params)
) 
# manipulate data for plotting: make longer, specify order in which compartments are displayed and their labels
seirv_output %>% 
  pivot_longer(cols = c(S, E, I, R, V), names_to = "compartment", values_to = "count") %>% 
  mutate(compartment = factor(compartment,
                              levels = c("S", "E", "I", "R", "V"), 
                              labels = c("Susceptible", "Exposed", "Infected", "Recovered", "Vaccinated"))) %>% 
  plot_model_sims()

#### Adding time dependency ####

# what if the start of vaccination is delayed?
# adding this time dependency to the seirv function
seirv_delay_model <- function(times, states, params){ 
  # creating a list of states and parameters to then reference
  with(as.list(c(states, params)), { 
    # total modeled population
    N <- S+E+I+R+V
    # force of infection
    lambda <- beta*(I/N)
    # time-dependent vaccination
    v_t <- if_else(condition = times < delay, true = 0, false = v)
    # differential equations assuming steady state population: 
    # birth of new s = mortality of s, e, i, r, v
    dS <- mu*(N-S) - (v_t+lambda)*S 
    dE <- lambda*S - (mu+delta)*E
    dI <- delta*E - (mu+gamma)*I
    dR <- gamma*I - mu*R
    dV <- v_t*S - mu*V
    # return the transition rates
    list(c(dS, dE, dI, dR, dV))
  })
}

# specify parameters - same as before but adding delay
params <- c(beta = 0.5, # daily effective contact rate is 0.5
            delta = 1/7, # people are pre-infectious for 1 week on average
            gamma = 1/14, # people are infectious for 2 weeks before recovering
            mu = 1/(60*365.25), # life expectancy is 60 years
            v = 0.01,
            delay = 30) # daily vaccination rate

# completely susceptible population during 180 days, recording population sizes each day
states <- c(S = 999, E = 1, I = 0, R = 0, V = 0) 
times <- seq(from = 0, to = 180, by = 1)

# simulate population by solving differential equations for given states, times, parameters
seirv_delay_output <- as.data.frame(
  ode(y = states, times = times, func = seirv_delay_model, parms = params)
) 

# manipulate data for plotting: make longer, specify order in which compartments are displayed and their labels
seirv_delay_output %>% 
  pivot_longer(cols = c(S, E, I, R, V), names_to = "compartment", values_to = "count") %>% 
  mutate(compartment = factor(compartment,
                              levels = c("S", "E", "I", "R", "V"), 
                              labels = c("Susceptible", "Exposed", "Infected", "Recovered", "Vaccinated"))) %>% 
  plot_model_sims()

# create one dataframe with output from all model runs
compare_output <- bind_rows(seir_output %>% 
                              mutate(V = 0, model = "No vaccination"),
                            seirv_delay_output %>% 
                              mutate(model = "Delayed vaccination"),
                            seirv_output %>% 
                              mutate(model = "Immediate vaccination")) 

# manipulate data for plotting: make longer, specify order in which compartments are displayed and their labels
compare_output %>% 
  pivot_longer(cols = c(S, E, I, R, V), names_to = "compartment", values_to = "count") %>% 
  mutate(compartment = factor(compartment,
                              levels = c("S", "E", "I", "R", "V"), 
                              labels = c("Susceptible", "Exposed", "Infected", "Recovered", "Vaccinated")),
         model = factor(model, levels = c("No vaccination", "Delayed vaccination", "Immediate vaccination"))) %>% 
  plot_model_sims() + 
  facet_wrap(~model) # use subplots to show output from each model
