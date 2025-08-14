############################################################################################
## Function to simulate SIR epidemic by solving a system of ordinary differential equations
############################################################################################

# Function arguments:
# time is the time step where dX/dt etc is being evaluated
# states is a vector of the X values
# params is a vector of values for the parameters
#
# in the case of SIR model X = (S, I, R) its values are the sizes of population compartments

sir_model <- function(time, states, params){
  # creating a list of states and parameters to then reference
  with(as.list(c(states, params)), {
    # total modeled population
    N <- S + I + R
    # force of infection
    lambda <- beta * (I / N)

    # this model assumes a constant total population
    # that means "balanced" transfers: all out-flows must be in-flows elsewhere
    # in particular, all mortality becomes new births

    death <- mu*c(S, I, R) # death occurs from all states
    birth <- sum(death) # birth == total death
    infection <- lambda * S
    recovery <- gamma * I

    # here we add flows to the state they go *in* to
    # and subtract where they leave *from*

    dS <- birth - infection
    dI <- infection - recovery
    dR <- recovery

    # add now we remove death from all states + return the transition rates
    list(c(dS, dI, dR) - death)
  })
}
