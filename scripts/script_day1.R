library(tidyverse)

rats_df <- read_csv("https://raw.githubusercontent.com/mark-andrews/immr23/main/data/rats.csv")

rats_df_42 <- filter(rats_df, batch == 42)

# Binomial model of the probability of developing a tumour in batch 42
M_1 <- glm(cbind(m, n-m) ~ 1, data = rats_df_42, family = binomial())

# the estimate of the probability of developing a tumour in batch 42
plogis(coef(M_1))

# the 95% confidence interval on that probaility
plogis(confint.default(M_1))


# Binomial model for the probability of tumours in each batch
M_2 <- glm(cbind(m, n-m) ~ 0 + factor(batch), 
           data = rats_df, 
           family = binomial())

# the 95% confidence interval on the probability in each
plogis(confint.default(M_2))

# the range of MLE of the 71 probabilities
round(plogis(coef(M_2)), 3)
       


# Multilevel binomial model -----------------------------------------------

library(lme4)

M_3 <- glmer(cbind(m, n-m) ~ 1 + (1|batch),
             data = rats_df,
             family = binomial())

summary(M_3)

# probability of tumour in the average batch
plogis(fixef(M_3))
