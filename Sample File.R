# library
library(ggplot2)
library(formulaic)
library(dplyr)
library(broom)
library(forcats)
library(glmnet)


# Read file
data <- read.csv('Data/mtcars2.csv')

# Make plot
mtcars2 <- within(mtcars, {
  vs <- factor(vs, labels = c("V-shaped", "Straight"))
  am <- factor(am, labels = c("Automatic", "Manual"))
  cyl  <- factor(cyl)
  gear <- factor(gear)
})

p1 <- ggplot(mtcars2) +
  geom_point(aes(x = wt, y = mpg, colour = gear)) +
  labs(
    title = "Fuel economy declines as weight increases",
    subtitle = "(1973-74)",
    caption = "Data from the 1974 Motor Trend US magazine.",
    tag = "Figure 1",
    x = "Weight (1000 lbs)",
    y = "Fuel economy (mpg)",
    colour = "Gears"
  )

p1

# lasso regression model
output <- as.matrix(mtcars2[, 'mpg'])
inputs <-
  as.matrix(mtcars[, c('cyl',
                       'disp',
                       'hp',
                       'drat',
                       'wt',
                       'qsec',
                       'vs',
                       'am',
                       'gear',
                       'carb')])
formula1 <-
  create.formula(outcome.name = output,
                 input.names = inputs,
                 dat = mtcars2)

model1 <- cv.glmnet(x = inputs, y = output)

tidy(model1$glmnet.fit) %>%
  ggplot(aes(lambda, estimate, color = term)) + geom_line() + scale_x_log10()

plot(model1)