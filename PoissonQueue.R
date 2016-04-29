set.seed(5825)
N = 2; lam = 4; mu = 5; rho = lam/mu
m = 50000; x = t = numeric(m); x[1] = 0 
### There is one server and a capacity of 2 within the system.
### States are accordingly: 0 - the barber is free, 1 - the barber is 
### working and nobody is waiting, and 2 - one customer is waiting and 
### one is being worked on. Both arrivals and service rates are exponentially
### distributed; at rates of 4 and 5, respectively.

for (i in 2:m) {
	if (x[i-1] == 0){
		x[i] = 1; t[i - 1] = rexp(1, lam)} else {
	if (x[i-1] == N) {
		x[i] = N-1; t[i-1] = rexp(1, mu)} else {
		x[i] = sample(x[i-1]+c(-1,1), 1, prob=c(mu, lam));
			t[i-1] = rexp(1, lam+mu)}}}

### In this loop, x represents the state changes (the number of customers 
### in the barber shop). It ranges from 0:2.
### t is the probability that a customer either arrives or departs.
t.avg = numeric(N+1); states=0:N; p = rho^states *(1-rho)/(1-rho^(N+1))
### t.avg measures the mean time customers waited per state
### p is the exact amount of time they spent in the system
for (j in 1:(N+1)) 
{
t.avg[j] = sum(t[x==(j-1)])/sum(t)
}
### Below is the time average distribution according to the states
round(cbind(states, p, t.avg), 3)
lam.e = (1-t.avg[3])*lam; lam.e ### The effective rate of entry
L = sum((0:2)*t.avg); L ### Ave number waiting in the queue
L.q = t.avg[3]; L.q	### Ave number of customers waiting in the system
W = L/lam.e; W 		### Ave time in the system
W.q = W - 1/mu; W.q	### Ave customers' wait time in the queue


### discrete-time chain
P = matrix((1/60)*c(56, 4, 0, 5, 51, 4, 0, 5, 55), byrow=T, nrow=3)
set.seed(5825)
m = 10^6; y = numeric(m); y[1] = 2
for (i in 2:m) y[i] = sample(1:3, 1, prob=P[y[i-1],])
### The simulation earlier gave us a time average between states accordingly,
### States 0, 1, 2: .406, .330, .263
round(summary(as.factor(y))/m, 4)
### The simulation is very similar to the discretized version


