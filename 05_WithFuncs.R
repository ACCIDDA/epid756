## Let's simulate a series of small outbreaks where the
## reproductive number is less than one to see what the distribution of small outbreaks.

##Source the code
source("05_ExampleFuncs.R")

##simulate the outbreaks
res <- sim_outbreaks(0.5, 1000)
hist(res, breaks = max(res))
print(summary(res))


##Simulate outbreaks with R0=0.75
res <- sim_outbreaks(0.75, 1000)
hist(res, breaks = max(res))
print(summary(res))
