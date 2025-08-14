
##' Simulate houeshold data
##' @param n number of households
##' @param p transmission probability
##' @param max_size maximum household size
##' @param summarize if FALSE will return data frame with all generations, if TRUE will return only final sizes

hh_sim <- function(n, p, max_size, summarize) {
  
  out <- data.frame(hh_id = numeric(),
                    hh_size = numeric(),
                    gen = numeric(),
                    num_inf = numeric())
  
  for(i in 1:n) {
    
    size <- sample(2:max_size, 1) # randomly select hh size
    
    # Generation zero (index case)
    out <- bind_rows(out,
                     data.frame(hh_id = i,
                                hh_size = size,
                                gen = 0,
                                num_inf = 1))
    
    current_infs  <- 1
    g <- 1
    susceptibles <- size - 1
    
    # Keep simulating until no new infections or we run out of susceptibles
    while(current_infs > 0 & susceptibles > 0) {
      
      # probabilty that a susceptible hh member is infected during this generation
      inf_prob <- current_infs*p 
      
      # Sample number of infections in the next generation
      current_infs <- sum(sample(c(0,1), susceptibles, replace = T, prob = c(1-inf_prob, inf_prob)))
    
      out <- bind_rows(out,
                       data.frame(hh_id = i,
                                  hh_size = size,
                                  gen = g,
                                  num_inf = current_infs))
      
      # Updates for next generation
      g <- g+1
      susceptibles <- susceptibles - current_infs
    } 
    
  }
  
  if(summarize) {
    return(get_final_sizes(out))
  } else {
    return(out)  
  }
  
}


##' Calculates final size in each household
##' @param x data frame with infection generations

get_final_sizes <- function(x) {
  
  out <- x %>%
    group_by(hh_id) %>%
    summarize(hh_size = hh_size[1],
              num_inf = sum(num_inf))
  
  return(out)
  
}


##' Calculates likelihood for chain binomial model
##' @param x data frame of final sizes

compute_likelihood <- function(x, p) {
  
  m <- matrix(0, nrow = 3, ncol = 4)
  rownames(m) <- paste0("H", 2:4)
  colnames(m) <- paste0("I", 1:4)
  
  x <- x %>%
    group_by(hh_size, num_inf) %>%
    summarize(n = n())
  
  for(i in 1:nrow(x)) {
    m[x$hh_size[i]-1, x$num_inf[i]] <- x$n[i]
  }
  
  q <- 1-p
    
  llik <- 
    m["H2","I1"] * log(q) +              #1 out of 2 infected
    m["H2","I2"] * log(p) +              #2 out of 2 infected
    m["H3","I1"] * log(q^2) +            #1 out of 3 infected
    m["H3","I2"] * log(2*q^2*p) +        #2 out of 3 infected
    m["H3","I3"] * log(p^2 + 2*q*p^2) +  #3 out of 3 infected
    m["H4","I1"] * log(q^3) +            #1 out of 4 infected
    m["H4","I2"] * log(3*p*q^4) +        #2 out of 4 infected
    m["H4","I3"] * log(6*p^2*q^4 + 3*p^2*q^3) + #3 out of 4 infected
    m["H4","I4"] * log(6*p^3*q^3 + 3*p^3*q^2 + 3*p^3*q*(q+1) + p^3) #4 out of 4 infected
  
  return(llik)
  
}
