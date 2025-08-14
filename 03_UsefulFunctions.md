# Describing/understanding your data:

 - `str()` – internal structure of R object
 - `head()` – shows top rows of a data frame, first elements of a vector
 - `tail()` – shows bottom rows of a data frame, last elements of a vector
 - `table()` – frequency table
 - `summary()` – depends on object type
 - `hist()` – histogram

# Objects

 - `data.frame()`
 - `matrix()`
 - `list()`
 - `c()`
 - `rep()` – creates vector of repeated values
 - `seq()` – creates vector of a sequence of increasing or decreasing values

# Indexing

 - `is.na()` – returns TRUE or FALSE to indicate if value is NA
 - `which()` – returns indices which meet the given condition
 - `which.min()` – returns indices of minimum value
 - `which.max()` – returns indices of maximum value

# Apply functions:

 - `apply()` – applies a function to each row or column of an array, there are lots of variations on this available in base R

# Sampling

 - `sample()` – random sample from a set of given values
 - `rnorm()` – random sample from normal distribution
 - `rbinom()` – random sample from binomial distribution
 - `runif()` – random sample from uniform distribution

# Summary statistics:

 - `min()` – minimum
 - `max()` – maximum
 - `mean()`
 - `median()`
 - `quantile()`
 - `range()`
 - `IQR()`
 - `var()` – variance
 - `sd()` – standard deviation

# Other numeric operations

 - `sum()`
 - `prod()`– product
 - `cumsum()` – cumulative sum
 - `cumprod()` – cumulative product
 - `log()` – natural logarithm (base $e$)
 - `log10()` – log base 10
 - `exp()` – exponential
 - `round()` – round to a specified number of decimal places
 - `signif()` – round to a specified number of significant digits

# Regression
 - `lm()` – linear model
 - `glm()` – generalized linear model, can be used for logistic regression, poisson regression, negative binomial regression, etc.
