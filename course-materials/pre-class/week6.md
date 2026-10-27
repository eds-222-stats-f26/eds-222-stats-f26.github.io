this week is about comparing linear regression models. how more complex models have benefits (more variation explained) but at a cost (more information "spent"). as a benefit, see how some widely used statistical tests are actually just comparisons between linear models of different complexity.

learning objectives: 
1. review categorical predictors
2. compute degrees of freedom from data and model
3. compare simpler and more complex models

# review categorical predictors

## dummy variables

category levels are text. you can't multiply a coefficient by a word. so we need dummy variables.

one level becomes the reference

every other level gets a dummy variable, 0 or 1

show a table converting category to dummy

k category levels get k-1 dummy variables

## two categories, represented three ways

show the equation, figure, and computational output for a 2-level category predictor

## 3+ categories, represented three ways

show the equation, figure, and computational output for a 4-level category predictor

## with linear regression, mix and match

show the equation, figure, and computational output for a 3-level category predictor plus a continuous predictor

# compute degrees of freedom from data and model

## degrees of freedom are inference currency

data gives us information

parameters of a model spend that information

the difference between the two is our "degrees of freedom" 

$df = n - p$

## information-complexity trade off

more parameters spend df to improve fit

too few parameters: model misses real structure (underfitting)

too many parameters: model fits the noise (overfitting)

every parameter has to earn its df

## example

give an example of a model with 2 level categorical predictor

## example

give an example of a model with the same 2 level categorical predictor plus a continuous variable

## which model is better

compare the two models. did spending the degree of freedom on the continuous variable add more information than the penalty for complexity?

# compare simpler and more complex models

## Cost-benefit

adding a parameter explains more, but at a cost

the benefit: smaller residuals, more variation explained

the cost: spent degrees of freedom, each parameter has less information behind it

## quantify the tradeoff

put the equation for the f statistic here

numerator: improvement in fit per df spent

denominator: leftover noise per df remaining

Bigger F means the extra parameters were worth it

## an example

compare intercept-only model to two-category model. visually and mathematically.

this asks: do the two categories explain enough of the variation to justify the cost?

this case has a special name: it's a t-test

## another example

compare intercept-only model to a 3+ category model. visually and mathematically.

this asks: does this categorical variable explain enough of the variation to justify the cost?

this also has a special name: it's anova

## third example

compare an additive model to an interactive model. visually and mathematically.

this asks: does the interaction explain enough of the variation to justify the cost?

# recap

more data means more information to spend on inference

what do we spend it on? parameters.

more complex models explain more variation, but do they explain _enough_ variation to justify their costs?
