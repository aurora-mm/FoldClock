# Overview

A lightweight [R](https://www.r-project.org) script that leverages a past dating history and current event log to estimate when one is likely to find a meaningful match (high specificity) in Östergötland, Sweden. It fits a three‐stage Bayesian logistic model (meet, follow-up, resonate), uses informative beta priors (with fallback to empirical priors), and simulates the waiting‐time distribution for the "next relationship". Returns key quantiles (5th - 95th) along with a deterministic worst‐case scenario.

# Key Features

* Event logging: record venue, crowd size, vibe (1, cold, to 4, warm), and micro-signals (e.g. recognition or unsolicited contact).

* Bayesian modeling: three conditional Bernoulli regressions (meet, follow-up, resonate) with splines on date or linear time, fit via `brms`.

* Informative priors: beta-prior fallback when data are sparse (stage 2 & stage 3), plus Student t-priors on coefficients.

* Horizon simulation: Monte Carlo samples of waiting events to months; outputs median, quartiles, and tail quantiles.

* Worst-case deterministic: uses fixed low probability (p = 0.009) to show an absolute lower bound on the waiting horizon.

# Author

Linn Friberg
