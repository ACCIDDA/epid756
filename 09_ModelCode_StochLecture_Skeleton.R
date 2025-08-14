##' Susceptible-Exposed-Infected-Recovered (SEIR) Model 
##' Does dx_dt for an SEIR model (assumes frequency dependence)
##' 
##' @param times the times this is run over
##' @param states the states, assumed to be S, E, I and R in that order
##' @param params the model parameters: beta (transmission parameter), delta (1/latent period), gamma (recovery rate), mu (birth/death rate)
##'
##' @return a list containing a vector of dS, dE, dI and dR. 
seir_model_dx_dt <- function(times, states, params){ 
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



##' function to run an SEIR model. Returns results in a "tidy" format
##' 
##' @param max_t the max time to go to
##' @param start_state the starting state
##' @param params  the SEIR parameters
##' 
##' @return a tibble with columns time, compartment and count
##' 
run_seir <- function(max_t, start_state, params) {
  require(deSolve)
  require(tidyverse)
  res <- as.data.frame(
    ode(y = start_state, times = 1:max_t, func = seir_model_dx_dt, parms = params)
  ) 
  
  res <- res %>% 
    pivot_longer(cols = c(S, E, I, R), names_to = "compartment", values_to = "count") %>% 
    mutate(compartment = factor(compartment, 
                                levels = c("S", "E", "I", "R"),
                                labels = c("Susceptible", "Exposed", "Infected", "Recovered"))) 
  
  return(res)
}



##' Function to do a time step for a stochastic SEIR model
##' No births or deaths for this one!
##' 
##' @param state the starting state
##' @param params the parameters
##' @param dt the length of the time step
##' 
##' @return the new state
stoch_seir_timestep_closed <- function (state, params,dt) {
  
}

##' Function that does just the birth death process
##' for a stochastic SEIR model
##' 
##' @param state the starting state
##' @param mu the birth/death rate
##' @param dt the length of the time step
##' 
##' @return the new state
stoch_seir_birthdeath <- function (state, mu, dt) {
  
}

##' Function to do a timestep in a stochastic SEIR model
##' that allows for a birth death process, or not.
##' 
##' 
##' 
##' @param state the starting state
##' @param params the parameters
##' @param dt the length of the time step
##' 
##' @return the new state
stoch_seir_timestep <- function (state, params, dt) {
  
}


##' Function to run a stochastic SEIR model
##' 
##' 
##' @param max_t the max time to go to
##' @param start_state the starting state
##' @param params  the SEIR parameters
##' @param dt the timestep
##' 
##' @return a tibble with columns time, compartment and count
run_stoch_seir <- function(max_t, start_state, params, dt) {
  
}