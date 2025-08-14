##' Code to run N simulations of a branching process epidemic.
##'
##' @param R0 the basic reproductive number
##' @param num_sims the number of simulations to do
##'
##' @return a vector with final sie of each simulation
sim_outbreaks <- function (R0, num_sims) {
    for (i in 1:num_sims) {
        res[i] <- sim_outbreak_sz(R0)
    }

    return(res)
}


##' Code to simulate a single outbreak from a branching process with R0
##'
##' @params R0 the reproductive number to simulate, should be less than 1
##'
##' @return the number of cases in this outbreak
sim_outbreak_sz <- function(R0) {

        ##Check that R0 value will not send up into an infinite loop
        if (R0>=1) {
            stop("R0 must be <1.")
        }

        ## Start with one case and keep simulating until we have a generation with 0
        ## cases, incrementing tot_cases each time.
        gen_sz <- 1
        tot_cases <- gen_sz

        while (gen_sz > 0) {
            ## Simulate the next generatoin
            gen_sz <- rpois(1, gen_sz*R0)

            tot_cases <- tot_cases+gen_sz
        }

    return(tot_cases)
}
