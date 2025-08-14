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
  nw_state <- state
  with(as.list(c(state, params)), { 
    N <- S+E+I+R
    ##calculate the force of infection
    lambda <- beta*(I/N)
    
    ##calculate state transitions
    StoE <- rbinom(1,S,1-exp(-dt*lambda))
    EtoI <- rbinom(1,E,1-exp(-dt*delta))
    ItoR <- rbinom(1,I,1-exp(-dt*gamma))
    
    ##Make new state
    nw_state['S'] <- S - StoE
    nw_state['E'] <- E + StoE - EtoI
    nw_state['I'] <- I + EtoI - ItoR
    nw_state['R'] <- R + ItoR
    return(nw_state)
  })
  
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
  nw_state <- state
  
  with(as.list(state), { 
    N <- S+E+I+R
    
    rate <- 1-exp(-dt*mu)
    ##calculate state transitions
    births <- rbinom(1,N,rate)
    Sdeaths<- rbinom(1,S,rate)
    Edeaths<- rbinom(1,E,rate)
    Ideaths <- rbinom(1,I,rate)
    Rdeaths <- rbinom(1,R,rate)
    
    ##Make new state
    nw_state['S'] <- S + births - Sdeaths
    nw_state['E'] <- E - Edeaths
    nw_state['I'] <- I - Ideaths
    nw_state['R'] <- R - Rdeaths
    return(nw_state)
  })
  
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
  nw_state <- stoch_seir_timestep_closed(state, params, dt)
  
  ##if there is a mu in the params vector do birth/death process
  if (!is.na(params["mu"])) {
    nw_state <- stoch_seir_birthdeath(nw_state, params["mu"], dt)
  }
  
  return(nw_state)
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
  
  require(tidyverse)
  
  t <- 0 
  rc <- list()
  rc[[1]] <- c(time=t,start_state)
  i <- 1
  
  ##loop through time steps
  while (t < max_t && (rc[[i]]['I']+rc[[i]]['E'])>0) {
    t <- t+dt
    
    i <- i+1
    rc[[i]]<- c(time=t,stoch_seir_timestep(rc[[i-1]][-1],params, dt))
  }
  
  
  rc <- bind_rows(rc)
  
  rc <- rc %>% 
    pivot_longer(cols = c(S, E, I, R), names_to = "compartment", values_to = "count") %>% 
    mutate(compartment = factor(compartment, 
                                levels = c("S", "E", "I", "R"),
                                labels = c("Susceptible", "Exposed", "Infected", "Recovered"))) 
  
  
  return(rc)
  
}