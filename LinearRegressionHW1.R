##   Use the /states.rds/ data set. Fit a model predicting energy consumed
##   per capita (energy) from the percentage of residents living in
##   metropolitan areas (metro). Be sure to
##   1. Examine/plot the data before fitting the model
##   2. Print and interpret the model `summary'
##   3. `plot' the model to look for deviations from modeling assumptions

##   Select one or more additional predictors to add to your model and
##   repeat steps 1-3. Is this model significantly better than the model
##   with /metro/ as the only predictor?

# Load data

states <- read_csv("~/states.csv")

# Create subset of states data focusing on percent and metro columns

subs1 <- subset(states, select = c("metro", "energy"))

# Create model with energy as dependent variable and metro as the explanatory variable.

mod1 <- lm(energy ~ metro, data = states)  # We see a very low R-squared of 0.1154, and adjusted 
                                           # R-squared of 0.097. The percent variable appears to
                                           # very little explanatory power.

# Steps 1-3.Look at plot, summary, correlation, and plot of data

plot(subs1)     
cor(subs1)


# Interpret the model summary

summary(mod1)   # Summary shows a slightly negative relationship between metro and energy usage.  
                # Coefficient of -2.2871, low R-squared of 0.1154 & adjusted R-squared of 0.097.
                # Metro is significant at 5% level.

# Plot model

plot(mod1)      # As metro increases, energy usage slightly decreases

# Add more variables to see if we can improve model.  Let's try area, green, and toxic.

mod2 <- lm(energy ~ metro + area + green + toxic, data = states)

# Look at summary of mod2

summary(mod2)   # Mod2 is a much better predictor of energy usage.  R-Squared and AdjR-Sq are
                # 0.7815 and .7612.  Green and toxic are significant (near 0%). Area is significant
                # at the 10% level, while metro is not significant.

# What if we drop metro and/or area?

mod3 <- lm(energy ~ area + green + toxic, data = states)
mod4 <- lm(energy ~ metro + green + toxic, data = states)
mod5 <- lm(energy ~ green + toxic, data = states)

# Concluding thoughts.

# mod3 has the highest R-squared and adjusted R-squared of .7802 and .7652. mod4 is the lowest
# with three variables, while mod5 has only two variables, but has values of .7627 and .7521. mod5
# is more simple with slightly worse explanotry power versus mod3. mod3 which includes area, green
# and toxic as explanatory variables appears to be the best model.

## Exercise: interactions and factors
## ────────────────────────────────────────

##   Use the states data set.

##   1. Add on to the regression equation that you created in exercise 1 by
##      generating an interaction term and testing the interaction.

##   2. Try adding region to the model. Are there significant differences
##      across the four regions?

# Generate an interaction term and add to model.

densitywaste <- states$density * states$waste
mod6 <- lm(energy ~ area + green + toxic + densitywaste, data = states)

# We see the interaction term densitywaste is not significant.

# Add region to the model.  Are there significant differences across the four regions?

mod7 <- lm(energy ~ area + green + toxic + region, data = states)

# All regions have t-values that don't appear to be significant.

# 
