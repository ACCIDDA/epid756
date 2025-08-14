#' @title Basic programming structures in R
#' Conditional logic, loops, and functions

library(ggplot2) # load plotting package

#' @section Different data structures in R ###
#'
#' vectors
pets <- c("Cat", "Dog", "Cat", "Fish", "Dog", "Cat", "Bird", "Fish")
scores <- sample(1:10, 100, replace = TRUE) # check out ?sample in the console

#' Handy functions to see what values are in our vector
summary(scores)
table(pets)

#' We use square brackets `[]` to *subset* objects in R
pets[3]
scores[1:3]

# We can also name the elements in our vector
params <- c("beta" = 0.002, "gamma" = 1 / 5, "alpha" = 0.03)
params["gamma"]

## Matrices
m1 <- matrix(sample(-5:5, 9, replace = TRUE), nrow = 3) # make a 3x3 matrix
m2 <- matrix(c(1, 2, 3, 6, 7, 2, 4, 6, 8), nrow = 3) # make a 3x3 matrix

m1 + m2 # matrix addition
m1 %*% m2 # matrix multiplication
m1 * m2 # element-wise matrix multiplication

# We use [row number, column number] to subset matrices
m1[2, 3]
m1[1, ] # leaving an element in the bracket blank gives the whole row or column

## Data frames
# Making your own data frames
x <- data.frame(letter = c("a", "b", "c"),
                number = c(1, 2, 3))
str(x)

# Let's look at a bigger data frame
data("iris") # load in a pre-included data set in R
dim(iris)
head(iris)

ggplot(iris) + # we will talk about plotting later
  geom_point(aes(x = Sepal.Length, y = Petal.Length, color = Species))

# We can use $ to access a named column in a data frame
table(iris$Species)

# We can also use traditional bracket indexing
iris[1:3, ]

## Lists are groups of objects
list1 <- list(pets = pets,
              matrix = m1,
              irises = iris)
str(list1)

# We use double brackets [[i]] to access entries in a list
list1[[1]]
# get the first entry in the list, then the first two elements of that entry
list1[[1]][1:2]


#' @section Conditional logic

value <- 3 # set value to 3
value == 3
value != 4
value > 10


#' Chain logical operators using & (and) and | (or)
value > 5 & value < 10
value > 5 | value < 10

#' if/else statements are useful for writing flexible code
value <- sample(1:3, 1)
if (value == 3) {
  print("Nice number!")
} else {
  print("Sorry I don't like your number")
}


#' @section Loops

#' A "for loop" loops over a pre-specified set of values
for (i in 1:5) {
  print(i ^ 2)
}

#' This is particularly useful for looping over entries in a data object
#' n.b., `1:N` constructs a sequence 1, 2, ..., N
#' `1:nrow(somedata)` is often fine, but if `somedata` has no entries
#' the sequence becomes `1:0`, instead of just an empty index set
#' `seq_len(nrow(somedata))` is a safe way to avoid this problem
for (i in seq_len(nrow(iris))) {
  if (iris$Species[i] == "setosa") {
    iris$summary_value[i] <- iris$Sepal.Length[i] * iris$Sepal.Width[i]
    iris$metric[i] <- "Sepal area"
  } else {
    iris$summary_value[i] <- iris$Petal.Length[i] * iris$Petal.Width[i]
    iris$metric[i] <- "Petal area"
  }
}

plt <- ggplot(iris) +
  geom_boxplot(aes(x = Species, y = summary_value, fill = Species)) +
  facet_grid(cols = vars(metric), scales = "free_x") +
  theme_bw()

print(plt)

#' While loops keep going until a condition is satisfied
i <- 1
while (i <= 1e6) { # keep doubling until we reach 1 million
  print(i)
  i <- i * 2
}
