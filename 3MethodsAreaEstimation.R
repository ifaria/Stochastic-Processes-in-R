### Finding C using a Reimann Integration
### To do this, we need create a grid with sufficiently small intervals
### that we will use to create thin rectangles that we will use to determine
### the area under the PDF.
set.seed(5825)
m = 50000; a = 0; b = 3;
w = (b - a)/m
g = seq(a + w/2, b - w/2, length = m)
h = exp(-g^3)
sum(w*h)
### The area under the curve is .8929795 and it should equal 1
### this discrepancy is caused by 'C,' the unknown constant
### to determine C, I divide 1 by the sum
const = 1/sum(w*h);const
### C is the contant: 1.119847
const = 1.119847

### Finding E(X)
### To find the expected value, we do a simlar approach to integration.
### Here, we "integrate" the PDF multiplied by its x variable, which
### in this Reimann case, would be a sequence within the grid we created.
m = 50000; a = 0; b = 3;
w = (b - a)/m
g = seq(a + w/2, b, by=w)
h = g*const*exp(-g^3)
sum(w*h)
### E(x) = .5054683

### Reimann approximation P(.5 < x < 1.5)
a = .5; b = 1.5
w = (b - a)/m
g = seq(a+w/2, b, by=w)
cd = const*exp(-g^3)
sum(w*cd)
### The sum of the Reimann rectangles between .5 and 1.5: .4520823
### This method finds the area within P(.5 < x < 1.5), but it is not
### a Monte Carlo method because it does not use random numbers.

### Simple Monte Carlo P(.5 < x < 1.5)
u = runif(m, a, b)
MC = const*exp(-u^3)
sum(w*MC)
### Sum of Monte Carlo method: .4512563

### Acc-Rej inside rectangle with verticies at (0, 0) and (.5, 1.5)
m = 10^6; a = .5; b = 1.5
u = runif(m, a, b); h = runif(m, 0, )
frac.acc = mean(h<MC)
area = (b-a)
area*frac.acc
### Area within the acceptance region: .45126

### The 3 previous methods for measuring the area work well and generate
### very similar values. The only method that we cannot use is called the
### Random Samplikng Method. Not only does this method often generate the
### largest margin of error, but it is only really necessary when the 
### density function of the distribution is not known
