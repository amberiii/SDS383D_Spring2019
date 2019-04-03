library(lme4)
library(tidyverse)
library(lattice)

cheese = read.csv("../data/cheese.csv")
summary(cheese)

boxplot(log(vol) ~ disp, data=cheese)

# confounders?
plot(log(vol) ~ log(price), data=cheese)
boxplot(price ~ disp, data=cheese)
boxplot(vol ~ store, data=cheese, log='y', las=2, cex.axis=0.6)
boxplot(price ~ store, data=cheese, las=2, cex.axis=0.6)


# main effects only
# how much does display activity affect demand?
lm1 = lm(log(vol) ~ log(price) + disp + store, data=cheese)
confint(lm1) %>% head(5) %>% round(3)

# does the presence of a display change price elasticity of demand?
lm2 = lm(log(vol) ~ log(price) + disp + log(price):disp + store, data=cheese)
confint(lm2) %>% round(3)

# Does price elasticity of demand change store by store?
lm3 = lm(log(vol) ~ log(price) + disp + log(price):disp + store + store:log(price), data=cheese)
confint(lm3) %>% round(3)

ggplot(subset(cheese, store == "SOUTH CAROLINA - WINN DIXIE")) +
         geom_point(aes(x=price, y=log(vol), color=factor(disp)))

ggplot(subset(cheese, store == "CHARLOTTE - BI LO")) +
  geom_point(aes(x=price, y=log(vol), color=factor(disp)))

ggplot(subset(cheese, store == "SOUTH CAROLINA - BI LO")) +
  geom_point(aes(x=price, y=log(vol), color=factor(disp)))


# common regression parameters across stores
hlm1 = lmer(log(vol) ~ log(price) + disp + (1 | store), data=cheese)
summary(hlm1)
coef(hlm1)

# different elasticity store by store
hlm2 = lmer(log(vol) ~ disp + (log(price) + 1 | store), data=cheese)
summary(hlm2)
coef(hlm2)


# interactions store by store
hlm3 = lmer(log(vol) ~ (1 + log(price) + disp + log(price):disp | store), data=cheese)
summary(hlm3)
coef(hlm3)

lm_kroger = lm(log(vol) ~ log(price) + disp + log(price):disp,
	data=subset(cheese, store == 'DALLAS/FT. WORTH - KROGER CO'))
coef(hlm3)$store['DALLAS/FT. WORTH - KROGER CO',]

plot(vol ~ price,
	data=subset(cheese, store == 'DALLAS/FT. WORTH - KROGER CO'))
points(vol ~ price,
	pch=19, col='blue',
	data=subset(cheese, store == 'DALLAS/FT. WORTH - KROGER CO'
						& disp == 0))
points(vol ~ price,
	pch=19, col='red',
	data=subset(cheese, store == 'DALLAS/FT. WORTH - KROGER CO'
						& disp == 1))

# Add curves				
curve(exp(9.933197)*x^(-1.800734), add=TRUE, col='blue')
curve(exp(9.933197 + 0.9507379)*x^(-1.800734-0.623256), add=TRUE, col='red')



# What about for a store that didn't use displays often?
xtabs(~store + disp, data=cheese)
coef(hlm3)$store['HARRISBURG/SCRANTN - GIANT FOOD STO',]


plot(vol ~ price,
	data=subset(cheese, store == 'HARRISBURG/SCRANTN - GIANT FOOD STO'))
points(vol ~ price,
	pch=19, col='blue',
	data=subset(cheese, store == 'HARRISBURG/SCRANTN - GIANT FOOD STO'
						& disp == 0))
points(vol ~ price,
	pch=19, col='red',
	data=subset(cheese, store == 'HARRISBURG/SCRANTN - GIANT FOOD STO'
						& disp == 1))

# Add curves				
curve(exp(8.355578)*x^(-1.208426), add=TRUE, col='blue')
curve(exp(8.355578 + 0.5653274)*x^(-1.208426-0.4296085), add=TRUE, col='red')

