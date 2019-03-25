library(mosaic)

ut2000 = read.csv("../data/ut2000.csv", header=TRUE)
ut2000$GPA = ut2000$GPA*100

# Main effects
lm1 = lm(GPA ~ SAT.C + School, data=ut2000)
summary(lm1)

plotModel(lm1) + facet_wrap( ~ School )

# With interaction b/t school and SAT scores
lm2 = lm(GPA ~ SAT.C + School + SAT.C:School, data=ut2000)
anova(lm2)  # sequential F test for each variable added
summary(lm2)

# uh-oh...
plotModel(lm2) + facet_wrap( ~ School )
