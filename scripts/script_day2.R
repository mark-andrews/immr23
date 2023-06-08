library(tidyverse)
library(lme4)
library(modelr)

# vivsc: varying intercept, varying slope, correlation
M_vivsc <- lmer(Reaction ~ Days + (Days|Subject), 
                REML = FALSE,
                data = sleepstudy)

# vi: varying intercept only
M_vi <- lmer(Reaction ~ Days + ( 1 |Subject), 
             REML = FALSE,
             data = sleepstudy)

# vs: varying slope only
M_vs <- lmer(Reaction ~ Days + (0 + Days|Subject),
             REML = FALSE,
             data = sleepstudy)

# vivs: varying intercept, varying slope, no correlation
M_vivs <- lmer(Reaction ~ Days + (Days||Subject), 
               REML = FALSE,
               data = sleepstudy)


# Visualize predictions of model M_vivsc
add_predictions(sleepstudy, M_vivsc) %>% 
  ggplot(aes(x = Days, y = Reaction, colour = Subject)) +
  geom_point() +
  geom_line(aes(y = pred)) +
  facet_wrap(~Subject) +
  ggtitle('VI/VS/C')

# Alternative visualization of model fit
add_predictions(sleepstudy, M_vivsc) %>% 
  ggplot(aes(x = Days, y = Reaction, colour = Subject)) +
  geom_point() +
  geom_line(alpha = 0.25, linewidth = 0.25) +
  geom_line(aes(y = pred)) +
  ggtitle('VI/VS/C')

# Visualize predictions of model M_vi
add_predictions(sleepstudy, M_vi) %>% 
  ggplot(aes(x = Days, y = Reaction, colour = Subject)) +
  geom_point() +
  geom_line(aes(y = pred)) +
  facet_wrap(~Subject) +
  ggtitle('VI')

# Visualize predictions of model M_vi: Alternative visualization
add_predictions(sleepstudy, M_vi) %>% 
  ggplot(aes(x = Days, y = Reaction, colour = Subject)) +
  geom_point() +
  geom_line(alpha = 0.25, linewidth = 0.25) +
  geom_line(aes(y = pred)) +
  ggtitle('VI')


# Visualize predictions of model M_vs
add_predictions(sleepstudy, M_vs) %>% 
  ggplot(aes(x = Days, y = Reaction, colour = Subject)) +
  geom_point() +
  geom_line(aes(y = pred)) +
  facet_wrap(~Subject) +
  ggtitle('VS')

# Visualize predictions of model M_vs: Alternative visualization
add_predictions(sleepstudy, M_vs) %>% 
  ggplot(aes(x = Days, y = Reaction, colour = Subject)) +
  geom_point() +
  geom_line(alpha = 0.25, linewidth = 0.25) +
  geom_line(aes(y = pred)) +
  ggtitle('VS')


# Visualize predictions of model M_vivs
add_predictions(sleepstudy, M_vivs) %>% 
  ggplot(aes(x = Days, y = Reaction, colour = Subject)) +
  geom_point() +
  geom_line(aes(y = pred)) +
  facet_wrap(~Subject) +
  ggtitle('VI/VS')

# Visualize predictions of model M_vivs: Alternative visualization
add_predictions(sleepstudy, M_vivs) %>% 
  ggplot(aes(x = Days, y = Reaction, colour = Subject)) +
  geom_point() +
  geom_line(alpha = 0.25, linewidth = 0.25) +
  geom_line(aes(y = pred)) +
  ggtitle('VI/VS')


# Log likelihoods of models -----------------------------------------------

logLik(M_vivsc) # log likelihood of model VI/VS/C
logLik(M_vivs)  # log likelihood of model VI/VS

-2 * logLik(M_vivsc) # deviance of model VI/VS/C
-2 * logLik(M_vivs)  # deviance of model VI/VS 

deviance(M_vivsc)
deviance(M_vivs)

deviance(M_vivs) - deviance(M_vivsc)

# Using Wilks's theorem: log-likelihood ratio test
anova(M_vivs, M_vivsc)

anova(M_vi, M_vivs) 
