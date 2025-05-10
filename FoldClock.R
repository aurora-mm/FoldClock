library(tidyverse)
library(lubridate)
library(janitor)
library(brms)        

# Data entry
# Crowd in numbers, approximate. Vibe on the scale 1-4 (cold-warm).
# Micro_sig: recognition, conversation out of the blue, contact.

events_log <- tribble(
  ~date,       ~venue,   ~crowd, ~vibe, ~micro_sig,
  "2025-05-07", "Onsdagscafe",  9,  2.0,   1,
  "2025-05-09", "Puben",        9,  1.5,   0,
  "2025-05-21", "Onsdagscafe", 10,  2.0,   0, # from here fill in real data
  "2025-05-28", "SocialClub",  40,  2.0,   0,
  "2025-05-30", "Speeddating", 15,  2.5,   1,
  "2025-05-30", "Speeddating", 15,  2.5,   1,
  "2025-05-31", "SocialClub",  70,  2.0,   1,
  "2025-06-04", "Onsdagscafe", 10,  2.0,   0,
  "2025-06-06", "Puben",       10,  1.0,   0,
  "2025-06-08", "Speltraff",    8,  2.0,   1
  
) %>% 
  mutate(
    date      = ymd(date),
    venue     = fct_inorder(venue),
    meet      = micro_sig,    
    follow_up  = 0, 
    resonate  = 0 
  ) %>% 
  clean_names()

# Feature engineering
logit <- function(p) log(p / (1 - p))
events_log <- events_log %>% 
  mutate(
    date_num = as.numeric(date),
    z_crowd  = as.numeric(scale(crowd)),
    z_vibe   = as.numeric(scale(vibe))
  )

# Model-formula helper
make_date_term <- function(df, k = 8) {
  nd <- n_distinct(df$date_num)
  if (nd >= k) {
    message(glue::glue("Using spline: k = {k} (n_unique = {nd})"))
    bf_term <- paste0("s(date_num, k = ", k, ")")
  } else {
    message(glue::glue("Too few dates (n_unique = {nd}); using linear date"))
    bf_term <- "date_num"
  }
  bf_term
}
date_term <- make_date_term(events_log, k = 4)

form_meet <- bf(
  as.formula(
    paste0("meet ~ 1 + venue + z_crowd + z_vibe + ", date_term)
  ),
  family = bernoulli()
)

form_follow <- bf(
  as.formula(
    paste0("follow_up ~ 1 + venue + z_crowd + z_vibe + ", date_term)
  ),
  family = bernoulli()
)

form_res <- bf(
  as.formula(
    paste0("resonate ~ 1 + venue + z_crowd + z_vibe + ", date_term)
  ),
  family = bernoulli()
)

# Priors
priors_meet <- c(
  prior(normal(logit(0.1), 0.4), class = Intercept),
  prior(student_t(3, 0, 2), class = b)
)

priors_follow <- c(
  prior(normal(logit(0.3), 0.4), class = Intercept),
  prior(student_t(3, 0, 2), class = b)
)

priors_res <- c(
  prior(normal(logit(0.3), 0.4), class = Intercept),
  prior(student_t(3, 0, 2), class = b)
)

# Fit stage 1
fit_meet <- brm(
  form_meet, data = events_log, prior = priors_meet,
  iter = 6000, seed = 12345,
  control = list(adapt_delta = 0.995, max_treedepth = 15)
)

# Fit stage 2
dat_follow <- filter(events_log, meet == 1)
if (nrow(dat_follow) >= 8) {
  fit_follow <- brm(
    form_follow, data = dat_follow, prior = priors_follow,
    iter = 6000, seed = 12345,
    control = list(adapt_delta = 0.995, max_treedepth = 15)
  )
} else {
  fit_follow <- NULL
}

dat_res <- filter(events_log, follow_up == 1)
if (nrow(dat_res) >= 8) {
  fit_res <- brm(
    form_res, data = dat_res, prior = priors_res,
    iter = 6000, seed = 12345,
    control = list(adapt_delta = 0.995, max_treedepth = 15)
  )
} else {
  fit_res <- NULL
}

# Posterior waiting time simulation
nsamp <- 10000
p_meet <- posterior_epred(fit_meet, newdata = events_log[1, ], ndraws = nsamp)[,1]

p_follow <- if (!is.null(fit_follow)) {
  posterior_epred(fit_follow, newdata = events_log[1, ], ndraws = nsamp)[,1]
} else {
  rbeta(nsamp, 5, 20) # fallback to known prior from before
}

p_res <- if (!is.null(fit_res)) {
  posterior_epred(fit_res, newdata = events_log[1, ], ndraws = nsamp)[,1]
} else {
  rbeta(nsamp, 6, 18) # fallback to known prior from before
}

p_total <- p_meet * p_follow * p_res
p_total <- pmax(pmin(p_total, 1 - 1e-6), 1e-6)

wait_events <- rgeom(nsamp, p_total) + 1
wait_months <- wait_events / 4
quantile(wait_months, c(.05, .25, .5, .75, .95))

p_hat <- mean(p_total)
expected_events <- 1 / p_hat
expected_months <- expected_events / 4
expected_months

# Worst case scenario
p_fixed <- 0.009
wait_events_fixed  <- rgeom(nsamp, p_fixed) + 1
wait_months_fixed  <- wait_events_fixed / 4
quantile(wait_months_fixed, c(.05, .25, .5, .75, .95))
