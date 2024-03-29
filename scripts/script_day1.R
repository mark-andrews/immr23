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

# probability of tumour 2 standard deviations below mean
plogis(-1.94 - 2 * 0.66)

# probability of tumour 2 standard deviations above mean
plogis(-1.94 + 2 * 0.66)

# get random effects
ranef(M_3)

# the estimated effect for batch 1
plogis(fixef(M_3) - 0.6298 )

coef(M_3)





# Normal random effects models --------------------------------------------

alcohol_df <- read_csv("https://raw.githubusercontent.com/mark-andrews/immr23/main/data/alcohol.csv")

alcohol_df_russia <- filter(alcohol_df, country == 'Russia')

M_4 <- lm(alcohol ~ 1, data = alcohol_df_russia)
coef(M_4)
sigma(M_4)

M_5 <- lmer(alcohol ~ 1 + (1|country), data = alcohol_df)
summary(M_5)

# country specific means
coef(M_5)

# random effects; random offsets from mean
ranef(M_5)


# Intraclass correlation coefficient ICC
VarCorr(M_5)
4.7125 ^ 2 / (4.7125 ^ 2 + 1.0525 ^ 2)

# Multilevel linear models ------------------------------------------------

# scatterplot
ggplot(sleepstudy,
       aes(x = Days, y = Reaction)
) + geom_point()

# scatterplot; one per subject
ggplot(sleepstudy,
       aes(x = Days, y = Reaction)
) + geom_point() + 
  facet_wrap(~Subject) +
  stat_smooth(method = 'lm')


# simple linear regression with this data
# lm(Reaction ~ 1 + Days, data = sleepstudy)

M_6 <- lmer(Reaction ~ 1 + Days + (1 + Days | Subject), 
            data = sleepstudy )
summary(M_6)
confint(M_6) # confidence interval on parameters

coef(M_6)
ranef(M_6)

M_6a <- lmer(Reaction ~ Days + ( Days | Subject), 
            data = sleepstudy )

# Varying intercept only multilevel linear models

M_7 <- lmer(Reaction ~ 1 + Days + (1 | Subject), 
            data = sleepstudy )


# Varying slope only multilevel linear models
M_8 <- lmer(Reaction ~ 1 + Days + ( 0 + Days | Subject), 
            data = sleepstudy )


# Varying slope and varying intercept, but NO correlation


M_9 <- lmer(Reaction ~ 1 + Days + (1 | Subject) + ( 0 + Days | Subject), 
            data = sleepstudy )

summary(M_9)


M_10 <- lmer(Reaction ~ 1 + Days + (1 + Days || Subject), 
            data = sleepstudy )
