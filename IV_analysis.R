library("readxl")
library("dplyr")
library("sem")
library("lmtest")
library("ivreg")
library("tseries")
library("stargazer")
library("systemfit")

# Import Data
FOS = read_excel("dataForStudents.xlsx")
str(FOS)
summary(FOS)

# We can see that NA data are present in the dataset, but we will follow carefuly
# since we have just 101 observation - if we remove all NA obs we would have just a few obs left.


# We will test use correlation matrix to check correlation between nlesson and nfile
cor(cbind(FOS$nlesson, FOS$nfile, FOS$full_time, FOS$part_time))


# Could work effect the score?
work_fultime = FOS %>%
  filter(working == "Full time") %>%
  summarise(mean(mark_t_FoS), mean(nlesson))

work_parttime = FOS %>%
  filter(working == "Part time") %>%
  summarise(mean(mark_t_FoS), mean(nlesson))

workn = FOS %>%
  filter(working == "No") %>%
  summarise(mean(mark_t_FoS), mean(nlesson))

working = FOS %>%
  filter(working != "No") %>%
  summarise(mean(mark_t_FoS), mean(nlesson))

work_effect = as.matrix(rbind(work_fultime, work_parttime, working, workn))
rownames(work_effect) = c("Full time", "Part time", "Job", "No job")
work_effect
# There seems to be an efect of working on the final grade and lesson attendance, therefore we will create dummy
# There are 11 NA obs in working, we will assume these individuals do not have a job
FOS$working[is.na(FOS$working)] = "No"

FOS$full_time =0
FOS$full_time[FOS$working == "Full time"] = 1
FOS$part_time = 0
FOS$part_time[FOS$working == "Part time"] = 1
FOS$job = 0
FOS$job[FOS$working != "No"] = 1


##Simple linear model for cross-section
simple_model = lm(mark_t_FoS ~ nlesson + nfile, data = FOS)
summary(simple_model)


# is nlesson variable correlated with working?
#We will use correlation matrix, to check it
cormat = cor(cbind(FOS$nlesson, FOS$full_time, FOS$part_time, FOS$job))
labels = c("nlesson", "full_time", "part_time","job")
colnames(cormat) = labels
rownames(cormat) = labels
cormat


## IV model
# We will estimate model with instrumental variable job, which is 1 for individual with job, 0 otherwise.
# firstly we will test whether the cov(nlesson, job != 0).
cov_model = lm(nlesson ~ job, data = FOS)
summary(cov_model)


# In the case we would like to use TSLS with, variables full_time, part_time we will test cov also for these variables:
full_time = lm(nlesson ~ full_time, data = FOS)
summary(full_time)

part_time = lm(nlesson ~ part_time, data = FOS)
summary(part_time)
# Not significant as well

# We will also assume exogeneity of job, since we cannot test it yet.
# Estimate the model with IV job.
IVreg = tsls(mark_t_FoS ~ nlesson + nfile, ~ job + nfile, data = FOS)
summary(IVreg)
summary(FOS$ent_math)
# Since in variable ent_math there are 5 NA's, we will use approximation and assume the individuals with NA would have average of ent_math points

FOS$ent_math[is.na(FOS$ent_math)] = mean(FOS$ent_math, na.rm = TRUE)

# Simple lm with ent_math
lmmath = lm(mark_t_FoS ~ nlesson + nfile + ent_math, data = FOS)
summary(lmmath)


IVmath = ivreg(mark_t_FoS ~ nlesson + nfile + ent_math | job + nfile + ent_math, data = FOS)
summary(IVreg_math)

# Test for heteroscedasticity
bptest(lmmath)

# for IV we will use Goldfeld-Quandt Test
gqtest(IVmath)


# Test for endogeneity
hausman.systemfit(IVreg_math, lmmath)

# Normality testing
#residuals
res = lmmath$residuals

jarque.bera.test(res)
# p-value > 0.05, we cannot reject normality

stargazer(simple_model, lmmath, header=FALSE, 
          title='Simple Linear Regression model', 
          single.row=TRUE, type='text')
