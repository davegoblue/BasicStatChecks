---
title: "Inferential Stats - Condensed Chapter Notes"
author: "davegoblue"
date: "March 13, 2016"
output: html_document
---

## Synopsis and Overview  
This document contains some of the key concepts, statistics, and R formulae from the UvA Inferential Statistics course.  It is intended as a helpful reference sheet for future use.  

### Module 1: Comparing Two Groups  
There are two schools of thought, frequentist (more common) and Bayesian (growing):  
  
* Frequentist - what are the odds of observing X given my hypothesized population?  
* Bayesian - what are the odds of a hypothesized population given that I observed X?  

There are a few good cautions raised about significance and confidence levels:  
  
* Statistical Significance - The x% confidence interval is constructed such that if we run the experiment infinite times, x% of our intervals would contain the true population mean  
* Practical Significance - does it actually matter that Ho: P=0 is rejected?  The confidence interval helps to assess effect magnitude  

Recall that there are two key error types:  
  
* Type I (reject a true null) - happens (1 - alpha), where alpha is pre-set  
* Type II (fail to reject a false null) -- happend beta, so (1 - beta) is the test power  
* Power is increased by better instruments, more homogenous samples, larger N, larger effect, one-sided tests, parametric tests (for sufficiently large N)  

####_Two indepedent proportions_  
The main metric is a z-test, and it generally requires at least 10 positive and 10 negative per group for one-sided tests, and at least 5 positive and 5 negative per group for two-sided tests:  

The test statistic is designed to follow a z-distribution, specifically:  
  
* Since null is equality, the test stastic starts as (p1-hat - p2-hat)  
* Standard error is then sqrt(p-hat * (1 - p-hat) * (1/n1 + 1/n2))  
* P-hat is the pooled proportion, or (n1 * p1-hat + n2 * p2-hat) / (n1 + n2)  
* CI (after the fact) can no longer use the pooled proportion since null is no longer assumed  
* (p1-hat - p2-hat) +/- z(alpha/2) * sqrt(p-hat1 * (1-p-hat1) / n1 + p-hat2 * (1-p-hat2) / n2)  

This can also be expressed as a relative risk, or p1/p2, with its own assumed confidence intervals.  

See below for an example from two random normal distributions:  


```r
set.seed(0313160758)
norm1 <- rnorm(100,mean=2,sd=1)
norm2 <- rnorm(120,mean=2.5,sd=1.5)
p1Norm <- norm1 > 2
p2Norm <- norm2 > 2
p1Hat <- mean(p1Norm)
p2Hat <- mean(p2Norm)
n1 <- length(p1Norm)
n2 <- length(p2Norm)

## Calculate means, difference, pooled proportion, standard error
print(paste0("Means of the data are P1: ",p1Hat," and P2: ",p2Hat))
```

```
## [1] "Means of the data are P1: 0.6 and P2: 0.55"
```

```r
poolProp <- (n1*p1Hat + n2*p2Hat) / (n1 + n2)
stdError <- sqrt(poolProp * (1-poolProp) * (1/n1 + 1/n2))
print(paste0("Pooled proportion is: ",round(poolProp,3)," with stderr: ",round(stdError,3)))
```

```
## [1] "Pooled proportion is: 0.573 with stderr: 0.067"
```

```r
## Calculate test statistic and p-value
zTestStat <- (p1Hat - p2Hat) / stdError
pTwoSided <- 1 - 2 * abs((pnorm(zTestStat) - 0.5))
print(paste0("The z-stat of: ",round(zTestStat,3),"which has z^2: ",round(zTestStat^2,3),
             " has two-sided significance: ",round(pTwoSided,3)
             )
      )
```

```
## [1] "The z-stat of: 0.746which has z^2: 0.557 has two-sided significance: 0.455"
```

```r
## Calculate post-hoc CI
newStdErr <- sqrt(p1Hat * (1 - p1Hat) / n1 + p2Hat * (1 - p2Hat) / n2)
critZ <- qnorm(.05/2, lower.tail=FALSE)
print(paste0("Post-hoc 95% CI for difference in proportions is ",
             paste(round(p1Hat - p2Hat + c(-1,1) * critZ * newStdErr, 3), collapse=" , ")
             )
      )
```

```
## [1] "Post-hoc 95% CI for difference in proportions is -0.081 , 0.181"
```

```r
## Comparison to R -- note that R uses chi-squared which is directly related to N(0.1)
prop.test(x=c(sum(p1Norm),sum(p2Norm)) , n=c(n1, n2) , correct=FALSE)
```

```
## 
## 	2-sample test for equality of proportions without continuity
## 	correction
## 
## data:  c(sum(p1Norm), sum(p2Norm)) out of c(n1, n2)
## X-squared = 0.55724, df = 1, p-value = 0.4554
## alternative hypothesis: two.sided
## 95 percent confidence interval:
##  -0.08092941  0.18092941
## sample estimates:
## prop 1 prop 2 
##   0.60   0.55
```
  
And, we observe that the findings from R match the hand calculations, with the exception that R runs a chi-squared test and the hand calculations are a z-test.  If x~N(0,1) then x^2 ~ chi-squared df=1, so these are functionally the same approach and outcome.  
  
####_Two indepedent means_  
For two indepednent means, the t-test is applied.  This requires independence of samples, roughly normal population distributions (though t is robust against this), and a "large enough" (N ~30) sample:  
  
The test statistic is designed to follow a t-distribution, specifically:  
  
#####_Unequal variance_  
* Since null is equality, the test stastic starts as (x1-hat - x2-hat)  
* Pooled standard error is then unPSE = sqrt(s1^2/n1 + s2^2/n2)  
* df is a cluster - (s1^2/n1 + s2^2/n2)^2 / [ (s1^2/n1)^2 / (n1 - 1) + (s2^2/n2)^2 / (n2 - 1) ]  
* Test-statistic is (x1-hat - x2-hat) / unPSE, following t with df  
* Post-hoc CI is (x1-bar - x2-bar) +/- t(alpha/2) * unPSE 
  
#####_Assume equal variance_  

* Pooled standard error is then eqPSE = sqrt( [ (n1-1) * s1^2 + (n2-1) * s2^2 ] / [ (n1 - 1) + (n2 - 1) ]  
* df is easy - n1 + n2 - 2    
* Test-statistic is (x1-hat - x2-hat) / eqPSE, following t with df  
  
Note that the unequal variances approach is greatly preferred, as the equal variances approach can be sensitive to violations of normality.  

See below for an example from two random normal distributions (we will keep the norm1 and norm2 as originally drawn, but without any conversions for "greater than 2":  


```r
x1Hat <- mean(norm1)
x2Hat <- mean(norm2)
s1 <- sd(norm1)
s2 <- sd(norm2)
n1 <- length(p1Norm)
n2 <- length(p2Norm)

## Report on individual means, standard deviations, and sample sizes
print(paste0("Distribution 1 has mean: ",round(x1Hat,2)," with std ",round(s1,2)," on n=",n1))
```

```
## [1] "Distribution 1 has mean: 2.19 with std 1.09 on n=100"
```

```r
print(paste0("Distribution 2 has mean: ",round(x2Hat,2)," with std ",round(s2,2)," on n=",n2))
```

```
## [1] "Distribution 2 has mean: 2.44 with std 1.59 on n=120"
```

```r
## Run this as an unequal variance approach
unPSE <- sqrt(s1^2/n1 + s2^2/n2)
unDF <- (s1^2/n1 + s2^2/n2)^2 / ((s1^2/n1)^2 / (n1-1) + (s2^2/n2)^2 / (n2-1))
print(paste0("Assuming unequal variance, we have difference: ",round(x1Hat - x2Hat,2),
             " with pooled SE: ",round(unPSE,2)," and df = ",round(unDF,1)
             )
      )
```

```
## [1] "Assuming unequal variance, we have difference: -0.26 with pooled SE: 0.18 and df = 210.3"
```

```r
unTest <- (x1Hat - x2Hat) / unPSE
unPTwoSided <- 1 - 2 * abs(pt(unTest,df=unDF) - 0.5)
print(paste0("The t-statistic ",round(unTest,3)," has two-sided significance ",round(unPTwoSided,3)))
```

```
## [1] "The t-statistic -1.416 has two-sided significance 0.158"
```

```r
critT <- qt(.05/2,df=unDF,lower.tail=FALSE)
print(paste0("The 95% CI for difference in means is ",
             paste(round(x1Hat - x2Hat + c(-1,1) * critT * unPSE, 3), collapse=" , ")
             )
      )
```

```
## [1] "The 95% CI for difference in means is -0.615 , 0.101"
```

```r
## Comparison to the R results
t.test(norm1,norm2,paired=FALSE,var.equal=FALSE)
```

```
## 
## 	Welch Two Sample t-test
## 
## data:  norm1 and norm2
## t = -1.4156, df = 210.29, p-value = 0.1584
## alternative hypothesis: true difference in means is not equal to 0
## 95 percent confidence interval:
##  -0.6149437  0.1009042
## sample estimates:
## mean of x mean of y 
##  2.187555  2.444575
```

```r
## Run this as an equal variance approach
eqS <- sqrt( ( (n1-1) * s1^2 + (n2-1) * s2^2 ) / ( (n1 - 1) + (n2 - 1) ) )
eqPSE <- eqS * sqrt(1/n1 + 1/n2)
eqDF <- n1 + n2 - 2
print(paste0("Assuming equal variance, we have difference: ",round(x1Hat - x2Hat,2),
             " with S: ",round(eqS,2)," and pooled SE: ",round(eqPSE,2)," with df = ",round(eqDF,1)
             )
      )
```

```
## [1] "Assuming equal variance, we have difference: -0.26 with S: 1.39 and pooled SE: 0.19 with df = 218"
```

```r
eqTest <- (x1Hat - x2Hat) / eqPSE
eqPTwoSided <- 1 - 2 * abs(pt(eqTest,df=eqDF) - 0.5)
print(paste0("The t-statistic ",round(eqTest,3)," has two-sided significance ",round(eqPTwoSided,3)))
```

```
## [1] "The t-statistic -1.369 has two-sided significance 0.172"
```

```r
critT <- qt(.05/2,df=eqDF,lower.tail=FALSE)
print(paste0("The 95% CI for difference in means is ",
             paste(round(x1Hat - x2Hat + c(-1,1) * critT * eqPSE, 3), collapse=" , ")
             )
      )
```

```
## [1] "The 95% CI for difference in means is -0.627 , 0.113"
```

```r
## Comparison to the R results
t.test(norm1,norm2,paired=FALSE,var.equal=TRUE)
```

```
## 
## 	Two Sample t-test
## 
## data:  norm1 and norm2
## t = -1.3693, df = 218, p-value = 0.1723
## alternative hypothesis: true difference in means is not equal to 0
## 95 percent confidence interval:
##  -0.6269680  0.1129285
## sample estimates:
## mean of x mean of y 
##  2.187555  2.444575
```

And, we observe that the findings from R match the hand calculations.  
  
####_Two dependent proportions_  
With two dependent proportions, you have a McNemar test.  Note that R will run this as a chi-squared (worry about the direction for one-sided in interpretation) while UvA runs this as a z-test (worry about the direction in the statistic set-up).  
  
Essentially, there are four things that can happen:  Good -> Good, Good -> Bad, Bad -> Good, and Bad -> Bad.  We are only actually interested in Good -> Bad (GB) and Bad -> Good (BG).  

* z = (BG - GB) / sqrt(BG + GB)  
* chiSq = (BG - GB)^2 / (BG + GB)  

Suppose we have a grid of 311, 34 (BG), 17 (GB), 14.  We only care about the 34 and the 17:  


```r
zTest <- (34 - 17) / sqrt(34 + 17)
zPStat <- 1 - 2 * abs(pnorm(zTest) - 0.5)
print(paste0("Using z-test, we have z: ",round(zTest,3)," with two-sided p: ",round(zPStat,3)))
```

```
## [1] "Using z-test, we have z: 2.38 with two-sided p: 0.017"
```

```r
## Compare to R
mcnemar.test(x=matrix(data=c(311,34,17,14),nrow=2,byrow=TRUE),correct=FALSE)
```

```
## 
## 	McNemar's Chi-squared test
## 
## data:  matrix(data = c(311, 34, 17, 14), nrow = 2, byrow = TRUE)
## McNemar's chi-squared = 5.6667, df = 1, p-value = 0.01729
```

Except for the use of chi-squared vs. z-statistic, the outputs are substantially the same.  
  
####_Two dependent means_  
This test is much simpler to run since you have paired observations.  Essentially:  
* Test-statistic t = (x1-bar - x2-bar) / ( Sd / sqrt(n) )  
* Sd is the standard deviation of the differences  
* df = n-1  
* CI = (x1-bar - x2-bar) +/- t(alpha/2) * Sd / sqrt(n)  

Below is an example for a random normal added to a random normal:  


```r
myVar1 <- norm1
myVar2 <- norm1 + rnorm(100,mean=0.5,sd=5)
myDiff <- myVar1 - myVar2

muDiff <- mean(myDiff)
sDiff <- sd(myDiff)
nDiff <- length(myDiff)
seDiff <- sDiff / sqrt(nDiff)
dfDiff <- nDiff - 1

tStatDiff <- muDiff / seDiff
pStatDiff <- 1 - 2 * abs(pnorm(tStatDiff) - 0.5)

print(paste0("The difference in means is: ",round(muDiff,2)," with standard error: ",round(seDiff,2)))
```

```
## [1] "The difference in means is: -1.55 with standard error: 0.49"
```

```r
print(paste0("The t-statistic ",round(tStatDiff,2)," with df = ",dfDiff,
             " has two-sided significance ",round(pStatDiff,3)
             )
      )
```

```
## [1] "The t-statistic -3.16 with df = 99 has two-sided significance 0.002"
```

```r
tCritical <- qt(.05/2,lower.tail=FALSE,df=dfDiff)
print(paste0("The 95% CI for difference in means is: ",
             paste(round(muDiff + c(-1,1) * seDiff * tCritical,3),collapse=" , ")
             )
      )
```

```
## [1] "The 95% CI for difference in means is: -2.515 , -0.576"
```

```r
## Compare with R
t.test(myVar1,myVar2,paired=TRUE)
```

```
## 
## 	Paired t-test
## 
## data:  myVar1 and myVar2
## t = -3.1638, df = 99, p-value = 0.002069
## alternative hypothesis: true difference in means is not equal to 0
## 95 percent confidence interval:
##  -2.5150923 -0.5762888
## sample estimates:
## mean of the differences 
##               -1.545691
```

As expected, the results from R match the associated hand calculations.  
  
  
### Module 2: Categorical Association  
