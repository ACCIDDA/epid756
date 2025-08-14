
#' load the core tidyverse libraries
library(tidyverse)

#### Simulating today's dataset ####

set.seed(8675309) # set seed to reproduce the random-number-generator results

n <- 1000 # sample size
beta0 <- 2 # intercept on log-odds scale
beta1 <- -0.05 # odds ratio on log scale
beta2 <- 0.8 # odds ratio on log scale

# simulating data as a tibble: a special type of data.frame in tidyverse
df <- tibble(
  # assigning IDs 1-1000 to each observation (per person)
  id = 1:n,
  # simulated population of older adults
  age = runif(n = n, min = 65, max = 90),
  # history of vaccination (1 if any/0 if none in prior 3 years)
  vax_hx = runif(n = n) <= 0.25,
  # probability of vaccinated this year (1) or not (0)
  vax_prob = 1 / (1 + exp(-(beta0 + beta1 * age + beta2 * vax_hx))),
  # indicator for whether there is missing data
  missing = runif(n = n) <= 0.20
)

# make sure simulated data is what we're expecting
summary(df$age)
table(df$vax_hx)
table(df$missing)
plot(df$age, df$vax_prob) # how do you interpret this plot?

# current vaccination status (yes/no) but some are missing
# ifelse is base R function and short hand for if() { } else { } code block
# here I'm specifying the function arguments (test, yes, no) but often people omit them
df$vax_current <- ifelse(
  test = df$missing == FALSE, # n.b. testing if *not* missing, not *is* missing
  yes = runif(n = n) <= df$vax_prob, # *not* missing
  no = NA # *is* missing
)

table(df$vax_current, useNA = "always")

#### Intro to dplyr for data manipulation ####

# "a grammar of data manipulation, providing a consistent set of verbs that help you solve the most common data manipulation challenges"
# two key concepts: action verbs and chaining together actions with the pipe

# introducing dplyr action verbs (functions)
# in each function, first argument always data.frame; subsequent arguments are for columns to operate on (using variable names); output always a new data frame

select(df, vax_current) # subset based on columns (variables)
filter(df, vax_current == 1) # subset based on rows (observations)
arrange(df, age) # changes ordering of rows
summarise(df, mean(vax_hx), mean(vax_current, na.rm = TRUE)) # reduces multiple values to a summary
mutate(df, followup = ifelse(is.na(vax_current), TRUE, FALSE)) # create new columns (variables)
group_by(df, vax_hx) # changes scope of subsequent functions: operate group-by-group instead of entire dataset

# the beauty of dplyr is using the pipe (%>%) to combine multiple verbs
# pass the object on the left-hand side of the pipe as the first argument of the function on the right-hand side

df %>% select(vax_current)
# is the same as
select(df, vax_current)

df %>%
  group_by(vax_current) %>%
  summarise(n(), mean(vax_hx))
# is the same as
summarise(group_by(df, vax_current), n(), mean(vax_hx))
# goal of making code more efficient and readable, reduces need to create intermediate objects in workflow

#### Manipulation example: Assess receipt of vaccination (history, current) by age group category ####

# create new variable for age group in 5-year increments using conditional logic
# if_else is dplyr version of ifelse from base R
# case_when is dplyr function to evaluate >1 ifelse/if_else statements (avoids nested statements)
df <- df %>%
  mutate(age_group = case_when(age <  70 ~ "65-69",
                               age >= 70 & age < 75 ~ "70-74",
                               age >= 75 & age < 80 ~ "75-79",
                               age >= 80 & age < 85 ~ "80-84",
                               age >= 85 ~ "85-90"))
# check out other function options for recoding continuous to categorical:
# cut from base R
# cut_interval, cut_number, cut_width from ggplot2

# check coding of age groups
df %>%
  group_by(age_group) %>%
  summarise(n = n(), min = min(age), max = max(age)) # naming new columns created with summarise

check <- df %>%
  select(id, age, age_group) %>%
  filter(age >=69.9 & age <70) %>%
  arrange(age) %>%
  mutate(round(age, digits = 1))

# assess vaccination history by age group
df %>%
  group_by(age_group) %>%
  summarise(n = n(),
            n_vax_hx = sum(vax_hx),
            prop_vax_hx = n_vax_hx/n)
# assess current vaccination by age group, consider missing data
df %>%
  group_by(age_group) %>%
  summarise(n = n(),
            n_missing = sum(is.na(vax_current)),
            n_vax_current = sum(vax_current, na.rm = T),
            prop_vax_nonmissing = n_vax_current/(n-n_missing),
            prop_vax_all = n_vax_current/n)
# complete case analysis
df %>%
  filter(!is.na(vax_current)) %>%
  group_by(age_group) %>%
  summarise(n = n(),
            n_vax_current = sum(vax_current),
            prop_vax_current = n_vax_current/n)

#### Intro to ggplot2 for data visualization ####

# powerful framework for developing elegant and complex plots - 3 key steps:
# 1. supply your data to plot
# 2. map variables in your data to aesthetic properties of the plot
# 3. choose a geom (type of graph) to illustrate those aesthetics

# always start with ggplot function, specifying data and mapping arguments
ggplot(data = df, mapping = aes(x = age))

# use the plus sign (+) to add geoms and other components to your plot
# note that the data and mapping arguments are always first two, so it's typical to drop their names
ggplot(df, aes(x = age)) +
  geom_histogram()

ggplot(df, aes(x = age_group)) +
  geom_bar()

ggplot(df, aes(x = age_group, y = age)) +
  geom_boxplot() +
  xlab("Age group") + # adding axis labels
  ylab("Age in years") +
  theme_classic() # themes modify overall appearance

# often useful to recode numeric variables as factors if they are treated as categorical variables
# factors will be plotted as discrete categories (vs. numeric variable with only values of 0 and 1 in the data)
# you can control the order in which the levels appear, labels will be added to your plot
ggplot(df, aes(x = age, # example of recoding vax_current as factor within ggplot function
               fill = factor(vax_current, levels = c(1,0), labels = c("Yes", "No")))) +
  geom_density(alpha = 0.5) +  # adjust transparency to reduce overplotting
  labs(x = "Age in years", fill = "Current vaccination") +  # all labels can be specified in labs function
  theme_classic()

#### Putting it all together: Examples of combining dplyr with ggplot ####

# pipe from dplyr to ggplot to manipulate/summarize data for plotting
df %>%
  filter(!is.na(vax_current)) %>% # can recode factors inside or outside of ggplot function
  mutate(vax_current_f = factor(vax_current, levels = c(1,0), labels = c("Yes", "No"))) %>%
  ggplot(aes(x = age, fill = factor(vax_current_f))) +
  geom_density(alpha = 0.5) +
  labs(x = "Age in years", fill = "Current vaccination") +
  theme_classic()

# it can be useful to save a new object with recoded variables or subsetted observations for plotting - especially if that version is used across multiple plots
df_for_plot <- df %>%
  filter(!is.na(vax_current)) %>%  # exclude missing data
  mutate(vax_current_f = factor(vax_current,  # recode factor variables
                                levels = c(1,0),
                                labels = c("Yes", "No")),
         vax_hx_f = factor(vax_hx,
                           levels = c(1,0),
                           labels = c("Vaccinated in prior 3 yr", "Not vaccinated in prior 3 yr")))

df_for_plot %>%
  group_by(age_group, vax_hx_f) %>%
  summarise(n = n(), prop_vax_current = mean(vax_current)) %>%  # summary statistics to plot
  ggplot(aes(x = age_group, y = prop_vax_current, fill = vax_hx_f)) +
  geom_bar(stat = "identity",  # plot proportions already calculated
           position = "dodge", # bars plotted next to each other rather than stacked
           width = 0.5) +   # modifying bar width
  scale_y_continuous(n.breaks = 6) + # adjusting breaks on y-axis
  labs(x = "Age in years", y = "Proportion currently vaccinated", fill = "History",
       title = "Current vaccination by age and history") +
  theme_bw() # trying out a different theme

# base R plot from before, but more informative
ggplot(df_for_plot, aes(x = age, y = vax_prob, color = vax_hx_f)) +
  geom_point(alpha = 0.5) +
  facet_wrap(~vax_current_f) + # divide into subplots based on vax_current_f
  ylim(0, 0.5) +
  labs(x = "Age in years", y = "Probability", color = "History",
       title = "Predicted probability of vaccination by age, history, and current status") +
  theme_classic() +
  theme(legend.position = "bottom") # modify default position of legend
# note use of "color" vs. "fill"
# fill defines the color with which a geom is filled, whereas color is the outline of a geom

# bonus: a way to visualize propensity score distributions
ggplot(df_for_plot, aes(x = vax_prob, group = vax_current_f, fill = vax_current_f)) +
  geom_histogram(aes(y = after_stat(density)), alpha = 0.75, binwidth = 0.02, position = position_dodge(width = 0.01)) +
  labs(x = "Probability", fill = "Current vaccination", title = "Predicted probability of vaccination, by observed status") +
  scale_fill_brewer(type = "qual", palette = 2, direction = -1) +
  theme_classic() +
  theme(legend.position = "bottom")

ggsave("figure_file_name_here.png")
# this will write out your last plot to your working directory
# can also save the ggplot as an object (using assignment operator), then print or write out using that object name
ggsave(name_of_your_object, file = "figure_file_name_here.png")
