causality handout

three scenarios. each one features one of: a confounder, collider, or mediator. for each scenario: 

- a description (with predictor/response) and a DAG
- given this info, write out all the paths. 
- identify the backdoor paths
- identify the set of variables to include or exclude in the model.

then:

- show the data in a figure with 3 variables. the predictor, the response, and a third variable that's either a confounder, collider, or mediator.
- show two model equations. one has a bias, one does not. for the mediator one, make that: one has the total effect, one has the direct effect. don't label these - the student doesn't know which is biased.
- show blank axes. the coefficient estimate is on the x axis, the model on the y axis. provide the output of summary() and confint() for both models.
- students draw in the coefficient estimates. then they pick which one is biased and which is unbiased.