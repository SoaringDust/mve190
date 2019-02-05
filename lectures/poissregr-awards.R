awarddata <- read.csv("poisson_sim.csv")
attach(awarddata)
prog <- factor(prog, levels=1:3, labels=c("General", "Academic", "Vocational"))

table(prog,num_awards)

# Here we check VERY INFORMALLY that E(Y|X)=Var(Y|X) for X=prog. We see that for at least one level this is not verified.
# Further down we actually invoke a proper testing procedure.

# conditional mean and variances should be about similar for Poisson regression to be applied
mean(num_awards[prog=="General"]) # similar
var(num_awards[prog=="General"])

mean(num_awards[prog=="Academic"]) # not very similar but not terribly off
var(num_awards[prog=="Academic"])

mean(num_awards[prog=="Vocational"]) # similar
var(num_awards[prog=="Vocational"])

hist(num_awards[prog=="General"])
hist(num_awards[prog=="Academic"])
hist(num_awards[prog=="Vocational"])
# histograms above are quite different, suggesting that "program" could be a good covariate (if they were very similar then
# the response would be essentially independent from program)

plot(prog,num_awards)

library("AER") # useful to test for overdispersion/underdispersion, see below

summary(m0 <- glm(num_awards ~ prog , family="poisson"))
# Notice, our previous check for mean=variance was very crude, and in once instance (Vocational case) it failed (var>mean).
# Let's check it with a formal test
dispersiontest(m0,alternative="two.sided") # we get a significant p-value and a dispersion > 1 -->overdispersion! here we should better use negative binomial!

# exp(beta1)=exp(1.61)=5.00
# compared to students from the general program, a student from the academic program can expect to get about 5 times more awards.
# That seems quite a lot. We'll see it is not quite like that. The above does not consider any information provided by the math grade.
# Some students may expect to get less awards according to their math grade.

summary(m1 <- glm(num_awards ~ prog + math, family="poisson"))
dispersiontest(m1,alternative="two.sided") # <-- all good! We fail to reject H0:mean=variance
exp(1.08) # 2.96 --> about 3
# compared to students from the general program and for fixed final math exam, a student from the academic program 
# can expect to get about 3 times more awards.
exp(0.07) # 1.07
# for a fixed study program, a unit increase in the students maths grade predicts a 7% increase (from 1 to 1.07) in the
# number of awards. Or, should the students population get on average an increase of 10 points in the
# maths grade (for fixed program), we would have an increase of awards equal to exp(10*0.07)=2, that is double the awards

# Likelihood radio test between model m0 and m1 (i.e. do we need to add math when program is in the model?)
anova(m0,m1)
# Difference between deviances is 45.01, from a chi-squared with 1 df
qchisq(0.95,1)  # = 3.84
# D_diff > 3.84, rject H0=reduced model. We need the extra info provided by the maths grade

# Now the other way around:
# we have a model with maths grade only, do we need to add the program?
anova(glm(num_awards ~ math, family="poisson"),glm(num_awards ~ math + prog , family="poisson"))
# Difference between deviances is 14.57, from a chi-squared with 2 df
qchisq(0.95,2)  # = 5.99
# D_diff > 5.99, rject H0=reduced model. We need the extra info provided by the program, when maths is already in the model

# Read more at http://stats.idre.ucla.edu/r/dae/poisson-regression/