## Let's simulate a series of small outbreaks where the
## reproductive number is less than one to see what the distribution of small outbreaks.

## Specify characteristics of the pathogen.
R0 <- 0.5

## Simulate a number of chains of transmission for each of these.
NUM_SIMS <- 1000

## For holding the results
res <- rep(0,NUM_SIMS)

for (i in 1:NUM_SIMS) {

    ## Start with one case and keep simulating until we have a generation with 0
    ## cases, incrementing tot_cases each time.
    gen_sz <- 1
    tot_cases <- gen_sz

    while (gen_sz > 0) {
        tmp <- 0

        ##simulate offspring for each person in this generation.
        for (j in 1:gen_sz) {
            tmp <- tmp + rpois(1, R0)
        }
        gen_sz <- tmp

        tot_cases <- tot_cases+gen_sz
    }

    res[i] <- tot_cases
}


## Plot and summarize the distribution of outbreaks sizes.
hist(res, breaks=max(res))
print(summary(res))


