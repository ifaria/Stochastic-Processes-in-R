### Medical Screening
set.seed(5825)
m = 50000
PI = numeric(m); PI[1] =.5	# vector for simulated prevalance and an arbitrary initial value
alpha = 1.5; beta = 35		  # parameters for beta prior
eta = .96; theta = .93		  # sensitivity and specificity
n = 500; A = 36; B = n - A	# our sample data
gamma = numeric(m)

### We set up a Gibbs Sampler to simulate the prevalence in the system.
### First, we set up a simulation to determine the PVP(gamma), which is
### represented as num.x/den.x and will be used as a probability in a
### random binomial generator to generate a value X. Similarly, for another
### value Y (represented by num.y/den.y) we will use 1 - the PVN(delta) within
### the random binomial generator. We will use X and Y in conjunction with
### the prior beta parameters to simulate the prevalence of this desease.

for (i in 2:m)
{
num.x = PI[i-1]*eta; den.x = num.x + (1-PI[i-1])*(1 - theta)
X = rbinom(1, A, num.x/den.x)
num.y = PI[i-1]*(1 - eta); den.y = num.y + (1-PI[i-1])*theta
Y = rbinom(1, B, num.y/den.y)
PI[i] = rbeta(1, X + Y + alpha, n - X - Y + beta)
gamma[i] = num.x/den.x
}

### Now we will examine the data after the burn-in period to gather meaninful
### results (we use only the last half of the simulation to represent the
### after burn-in. 

aft.brn = seq(m/2 +1,m)
mean(PI[aft.brn])	    	    ### Bayesian Estimate of Prevalance
round(quantile(PI[aft.brn], ### 95% interval for prevalance
(.025, .975))*n,) 	        ### 95% interval for prevalance
                            ### Written in terms of quantity of people in the sample population
                            ### The upper bounds would be anything after 17 people
quantile(PI[aft.brn], c(.025, .975))
                            ### in terms of percentage
mean(gamma[aft.brn])	      ### Bayesian Estimate of PVP


par(mfrow=c(2,2))
 plot(aft.brn, PI[aft.brn], type="l", xlab="Step (After Burn-In)", ylab="Prevalence")
 hist(PI[aft.brn], breaks=25, prob=T, col="wheat", xlab="Step (After Burn-In)", ylab="Prevalence", main="Bayesian Prob. Interval for Prevalence")
abline(v = .034, col=4, lty=2)
abline(v = .001, col=4, lty=2) 
acf(PI[aft.brn]); 
plot(1:m, cumsum(PI)/(1:m), type="l", ylim=c(0,.03), xlab="Step", ylab="Cumulative Prevalence")
par(mfrow=c(2,2)) 



