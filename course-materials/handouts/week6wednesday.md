two-way anova handout

moved from the monday handout. same format: an A scenario (we do) and a B scenario (you do), same pdf style as the monday handout, with a key version (params: key).

# two-way anova

tasks:

- show the group means in an interaction plot (first factor on x, color by second factor, lines connecting means), with points behind.
- three nested models: y ~ A, y ~ A + B, y ~ A * B. for each: p and df.
- sketch what the additive model predicts on the interaction plot (parallel lines). how is it different from the interaction model?
- given the RSS of the additive and interaction models: how many df does the interaction cost? compute F by hand.
- given the anova() table for the interaction model: check your F. does the interaction earn its df?
- interpret the result in a sentence about the system, not coefficients.

## A (we do): fire and grazing

seedling height (cm) in plots that were burned or unburned, and grazed or ungrazed. ~10 plots per combination. real interaction: burning helps seedlings only when plots aren't grazed. on monday's draft, seed 641 with cell means unburned/ungrazed 20, burned/ungrazed 32, unburned/grazed 15, burned/grazed 16, sd 4 gave interaction p = 0.007.

## B (you do): warming and nutrients

kelp growth (cm/week) in tanks at ambient or warm temperature, with ambient or enriched nutrients. ~10 tanks per combination. no interaction: warming reduces growth and nutrients increase it, by about the same amount either way (additive model wins). on monday's draft, seed 651 with cell means ambient/ambient 10, warm/ambient 7, ambient/enriched 14, warm/enriched 11, sd 2.5 gave interaction p = 0.53.

# additive vs interactive model

same as above, but categorical and continuous instead of categorical and categorical

then another round, but continuous and continuous instead of categorical and continuous

# design the study

three examples of written descriptions of research problems. written from the point of view of a client. so they have a question, but it's not framed cleanly as "here's the predictor, here's the response." Students will: 1) fill in a table to say what the variables are, whether they're categorical/continuous, and whether they're predictors or responses, 2) draw a dag, 3) write the equations for the simpler and more complex models, and specify H0 and Ha, 4) sketch data and model fits that would support the hypothesis.