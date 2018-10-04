# prob_nonprob_samples
Simulation code for combining probability and non-probability samples

### p1_simulation_i.R
- Model to sample Non-prob units:
Logit(p) = -10 + 1 * X1 - 2 * X2 + 3 * X3.2 + 4 * X3.3 - 5 * X4.2 - 6 * X4.3 **_+ 7 * Y4_**
- Model to estimate pseudo-weights (methods 1(c) and 1(d)):
Logit(p) ~ X1 + X2 + factor(X3) + factor(X4)



### p1_simulation_i_truemodel.R

- Model to sample Non-prob units:
Logit(p) = -10 + 1 * X1 - 2 * X2 + 3 * X3.2 + 4 * X3.3 - 5 * X4.2 - 6 * X4.3
- Model to estimate pseudo-weights (methods 1(c) and 1(d)):
Logit(p) ~ X1 + X2 + factor(X3) + factor(X4)
