### Lesson 13 - Thursday 12/15/22

* Today, we turn our attention to the multinomial logistic regression model (sometimes also called the multinomial logit model). This model is appropriate when we have a categorical outcome variable. The dataset we will consider is based on a sample of nearly 70,000 auto accidents and comes from Agresti (1997). Measures included in the data are: 

* *y* - 1=level of injury (1=not injured, 2=injured but not transported by EMS, 3=injured and transported by EMS, 4=injured and hospitalized but survived, and 5=fatal injury
* *sb* - no = person not wearing a seatbelt at time of accident, yes = person was wearing a seatbelt at the time of the accident
* *ur* - u = accident occurred in an urban area, r = accident occurred in a rural area
* *sx* - m = male, f = female

* The outcome variable *y* could be construed as either a categorical or an ordinal variable. Multinomial logistic regression models can be estimated with either type of variable. There are some restrictions on the use of specialized ordinal level variables which is a separate topic. For today, we will treat the outcome as a categorical variable with 5 categories.

* We begin our analysis by reading the dataset into memory:

```r
df <- read.csv(file="sb.csv",sep=",",header=T)

# we can select a simple random sample of 25 cases
# to look at the dataset

df[sample(nrow(df),size=25,replace=F), ]
```

* Here is our output (you will get a different sample of cases):

```rout
> df <- read.csv(file="sb.csv",sep=",",header=T)
> 
> # we can select a simple random sample of 25 cases
> # to look at the dataset
> 
> df[sample(nrow(df),size=25,replace=F), ]
      y  sb ur sx
67601 1 yes  r  m
32933 1  no  u  m
47646 1 yes  u  m
49569 1 yes  u  m
36416 1  no  u  m
4037  1  no  u  f
62798 1 yes  r  m
63082 1 yes  r  m
38785 1  no  u  m
67798 1 yes  r  m
37626 1  no  u  m
19387 1 yes  u  f
35081 1  no  u  m
12076 1 yes  u  f
19161 1 yes  u  f
7413  2  no  u  f
14650 1 yes  u  f
6523  1  no  u  f
63810 1 yes  r  m
48465 1 yes  u  m
34531 1  no  u  m
8392  1 yes  u  f
31545 3 yes  r  f
39366 1  no  u  m
1658  1  no  u  f
> 
```

* Next, we look at a crosstable of the outcome variable and seatbelt usage at the time of the accident:

```r
# multinomial logit 
# outcome variable: response (y)
# independent variable: seatbelt use (sb)

table(df$y,df$sb,exclude=NULL)
```

* Here is the resulting table:

```rout
> # multinomial logit 
> # outcome variable: response (y)
> # independent variable: seatbelt use (sb)
> 
> table(df$y,df$sb,exclude=NULL)
   
       no   yes
  1 27037 35383
  2   525   377
  3  2706  1753
  4   534   241
  5   100    38
> 
```

* To aid in interpreting the table, we can convert it to column percentage form:

```r
sb.table <- table(df$y,df$sb,exclude=NULL)
sb.table
col.pct <- prop.table(sb.table,margin=2)
col.pct
```

* Here is our output:

```rout
> sb.table <- table(df$y,df$sb,exclude=NULL)
> sb.table
   
       no   yes
  1 27037 35383
  2   525   377
  3  2706  1753
  4   534   241
  5   100    38
> col.pct <- prop.table(sb.table,margin=2)
> col.pct
   
             no         yes
  1 0.874927189 0.936256351
  2 0.016989192 0.009975656
  3 0.087567148 0.046385478
  4 0.017280435 0.006377011
  5 0.003236037 0.001005504
> 
```

* It looks like seatbelt usage coincides with lower injury risk.
* Let's convert our seatbelt usage variable into a numeric form.
* We also need to verify that the conversion worked properly.

```r
# independent variable

df$sbn <- rep(NA,nrow(df))
df$sbn[df$sb=="yes"] <- 1
df$sbn[df$sb=="no"] <- 0

table(df$sb,df$sbn,exclude=NULL)
```

* Here are the results:

```rout
> # independent variable
> 
> df$sbn <- rep(NA,nrow(df))
> df$sbn[df$sb=="yes"] <- 1
> df$sbn[df$sb=="no"] <- 0
> 
> table(df$sb,df$sbn,exclude=NULL)
     
          0     1
  no  30902     0
  yes     0 37792
> 
```

* Next, let's recode the outcome variable into a set of 5 dummy variables:

```r
df$y1 <- rep(NA,nrow(df))
df$y1[df$y==1] <- 1
df$y1[df$y==2 | df$y==3 | df$y==4 | df$y==5] <- 0
table(df$y1,df$y,exclude=NULL)

df$y2 <- rep(NA,nrow(df))
df$y2[df$y==2] <- 1
df$y2[df$y==1 | df$y==3 | df$y==4 | df$y==5] <- 0
table(df$y2,df$y,exclude=NULL)

df$y3 <- rep(NA,nrow(df))
df$y3[df$y==3] <- 1
df$y3[df$y==1 | df$y==2 | df$y==4 | df$y==5] <- 0
table(df$y3,df$y,exclude=NULL)

df$y4 <- rep(NA,nrow(df))
df$y4[df$y==4] <- 1
df$y4[df$y==1 | df$y==2 | df$y==3 | df$y==5] <- 0
table(df$y4,df$y,exclude=NULL)

df$y5 <- rep(NA,nrow(df))
df$y5[df$y==5] <- 1
df$y5[df$y==1 | df$y==2 | df$y==3 | df$y==4] <- 0
table(df$y5,df$y,exclude=NULL)
```

* Here are the results:

```rout
> df$y1 <- rep(NA,nrow(df))
> df$y1[df$y==1] <- 1
> df$y1[df$y==2 | df$y==3 | df$y==4 | df$y==5] <- 0
> table(df$y1,df$y,exclude=NULL)
   
        1     2     3     4     5
  0     0   902  4459   775   138
  1 62420     0     0     0     0
> 
> df$y2 <- rep(NA,nrow(df))
> df$y2[df$y==2] <- 1
> df$y2[df$y==1 | df$y==3 | df$y==4 | df$y==5] <- 0
> table(df$y2,df$y,exclude=NULL)
   
        1     2     3     4     5
  0 62420     0  4459   775   138
  1     0   902     0     0     0
> 
> df$y3 <- rep(NA,nrow(df))
> df$y3[df$y==3] <- 1
> df$y3[df$y==1 | df$y==2 | df$y==4 | df$y==5] <- 0
> table(df$y3,df$y,exclude=NULL)
   
        1     2     3     4     5
  0 62420   902     0   775   138
  1     0     0  4459     0     0
> 
> df$y4 <- rep(NA,nrow(df))
> df$y4[df$y==4] <- 1
> df$y4[df$y==1 | df$y==2 | df$y==3 | df$y==5] <- 0
> table(df$y4,df$y,exclude=NULL)
   
        1     2     3     4     5
  0 62420   902  4459     0   138
  1     0     0     0   775     0
> 
> df$y5 <- rep(NA,nrow(df))
> df$y5[df$y==5] <- 1
> df$y5[df$y==1 | df$y==2 | df$y==3 | df$y==4] <- 0
> table(df$y5,df$y,exclude=NULL)
   
        1     2     3     4     5
  0 62420   902  4459   775     0
  1     0     0     0     0   138
> 
```

* Now, we are ready to estimate the parameters of the multinomial logistic regression model. Here is the model where we have the 5-category outcome defined above and a single independent variable - seat belt use at the time of the accident.

```r
library(maxLik)

ll1 <- function(parms)
  {
    a2 <- parms[1]
    b2 <- parms[2]
    a3 <- parms[3]
    b3 <- parms[4]
    a4 <- parms[5]
    b4 <- parms[6]
    a5 <- parms[7]
    b5 <- parms[8]

    l2 <- a2+b2*df$sbn
    l3 <- a3+b3*df$sbn
    l4 <- a4+b4*df$sbn
    l5 <- a5+b5*df$sbn

    py1 <- 1/(1+exp(l2)+exp(l3)+exp(l4)+exp(l5))
    py2 <- exp(l2)/(1+exp(l2)+exp(l3)+exp(l4)+exp(l5))
    py3 <- exp(l3)/(1+exp(l2)+exp(l3)+exp(l4)+exp(l5))
    py4 <- exp(l4)/(1+exp(l2)+exp(l3)+exp(l4)+exp(l5))
    py5 <- exp(l5)/(1+exp(l2)+exp(l3)+exp(l4)+exp(l5))

    pmf <- df$y1*py1+df$y2*py2+df$y3*py3+df$y4*py4+df$y5*py5
    lpmf <- log(pmf)
    return(lpmf)
  }

m1 <- maxLik(ll1,start=c(-3.57239423,-0.13240324,
                         -3.57239423,-0.13240324,
                         -3.57239423,-0.13240324,
                         -3.57239423,-0.13240324),
  method="BHHH",finalHessian="BHHH")
summary(m1)
```

* Here is the output I got:

```rout
> library(maxLik)
Loading required package: miscTools

Please cite the 'maxLik' package as:
Henningsen, Arne and Toomet, Ott (2011). maxLik: A package for maximum likelihood estimation in R. Computational Statistics 26(3), 443-458. DOI 10.1007/s00180-010-0217-1.

If you have questions, suggestions, or comments regarding the 'maxLik' package, please use a forum or 'tracker' at maxLik's R-Forge site:
https://r-forge.r-project.org/projects/maxlik/
> 
> ll1 <- function(parms)
+   {
+     a2 <- parms[1]
+     b2 <- parms[2]
+     a3 <- parms[3]
+     b3 <- parms[4]
+     a4 <- parms[5]
+     b4 <- parms[6]
+     a5 <- parms[7]
+     b5 <- parms[8]
+ 
+     l2 <- a2+b2*df$sbn
+     l3 <- a3+b3*df$sbn
+     l4 <- a4+b4*df$sbn
+     l5 <- a5+b5*df$sbn
+ 
+     py1 <- 1/(1+exp(l2)+exp(l3)+exp(l4)+exp(l5))
+     py2 <- exp(l2)/(1+exp(l2)+exp(l3)+exp(l4)+exp(l5))
+     py3 <- exp(l3)/(1+exp(l2)+exp(l3)+exp(l4)+exp(l5))
+     py4 <- exp(l4)/(1+exp(l2)+exp(l3)+exp(l4)+exp(l5))
+     py5 <- exp(l5)/(1+exp(l2)+exp(l3)+exp(l4)+exp(l5))
+ 
+     pmf <- df$y1*py1+df$y2*py2+df$y3*py3+df$y4*py4+df$y5*py5
+     lpmf <- log(pmf)
+     return(lpmf)
+   }
> 
> m1 <- maxLik(ll1,start=c(-3.57239423,-0.13240324,
+                          -3.57239423,-0.13240324,
+                          -3.57239423,-0.13240324,
+                          -3.57239423,-0.13240324),
+   method="BHHH",finalHessian="BHHH")
> summary(m1)
--------------------------------------------
Maximum Likelihood estimation
BHHH maximisation, 15 iterations
Return code 8: successive function values within relative tolerance limit (reltol)
Log-Likelihood: -26013.69 
8  free parameters
Estimates:
     Estimate Std. error  t value  Pr(> t)    
[1,] -3.94156    0.04407  -89.448  < 2e-16 ***
[2,] -0.60018    0.06799   -8.828  < 2e-16 ***
[3,] -2.30173    0.02016 -114.158  < 2e-16 ***
[4,] -0.70317    0.03171  -22.178  < 2e-16 ***
[5,] -3.92457    0.04370  -89.808  < 2e-16 ***
[6,] -1.06462    0.07802  -13.645  < 2e-16 ***
[7,] -5.59979    0.10018  -55.895  < 2e-16 ***
[8,] -1.23661    0.19074   -6.483 8.97e-11 ***
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
--------------------------------------------
> 
```

* Based on the parameter estimates in this model, we are now able to calculate the predicted probability distribution of *y* implied by those estimates:

```r
# first, save the parameter estimates

a2 <- coef(m1)[1]
b2 <- coef(m1)[2]
a3 <- coef(m1)[3]
b3 <- coef(m1)[4]
a4 <- coef(m1)[5]
b4 <- coef(m1)[6]
a5 <- coef(m1)[7]
b5 <- coef(m1)[8]

# second, calculate the logits

l2n <- a2+b2*0
l2y <- a2+b2*1

l3n <- a3+b3*0
l3y <- a3+b3*1

l4n <- a4+b4*0
l4y <- a4+b4*1

l5n <- a5+b5*0
l5y <- a5+b5*1

# third, calculate p(y=?|sb=no) and p(y=?|sb=yes)

py1n <- 1/(1+exp(l2n)+exp(l3n)+exp(l4n)+exp(l5n))
py1y <- 1/(1+exp(l2y)+exp(l3y)+exp(l4y)+exp(l5y))

py2n <- exp(l2n)/(1+exp(l2n)+exp(l3n)+exp(l4n)+exp(l5n))
py2y <- exp(l2y)/(1+exp(l2y)+exp(l3y)+exp(l4y)+exp(l5y))

py3n <- exp(l3n)/(1+exp(l2n)+exp(l3n)+exp(l4n)+exp(l5n))
py3y <- exp(l3y)/(1+exp(l2y)+exp(l3y)+exp(l4y)+exp(l5y))

py4n <- exp(l4n)/(1+exp(l2n)+exp(l3n)+exp(l4n)+exp(l5n))
py4y <- exp(l4y)/(1+exp(l2y)+exp(l3y)+exp(l4y)+exp(l5y))

py5n <- exp(l5n)/(1+exp(l2n)+exp(l3n)+exp(l4n)+exp(l5n))
py5y <- exp(l5y)/(1+exp(l2y)+exp(l3y)+exp(l4y)+exp(l5y))

# display the results

c(py1n,py2n,py3n,py4n,py5n)
c(py1y,py2y,py3y,py4y,py5y)
```

* Here are the results:

```rout
> # display the results
> 
> c(py1n,py2n,py3n,py4n,py5n)
[1] 0.874927189 0.016989192 0.087567148 0.017280435 0.003236037
> c(py1y,py2y,py3y,py4y,py5y)
[1] 0.936256354 0.009975656 0.046385479 0.006377011 0.001005500
> 
```

* Next, we can compare these calculations to what we saw in the table we examined earlier:

```r
prop.table(table(df$y,df$sb),margin=2)
```

* which yields:

```rout
> prop.table(table(df$y,df$sb),margin=2)
   
             no         yes
  1 0.874927189 0.936256351
  2 0.016989192 0.009975656
  3 0.087567148 0.046385478
  4 0.017280435 0.006377011
  5 0.003236037 0.001005504
> 
```

* A question that now arises is whether the differences between these two columns are large enough for us to conclude that seat belt use is a significant predictor of the outcome distribution. To answer this question, we can construct a log-likelihood ratio test.
* The first step of the test is to estimate an intercept-only model:

```r
ll2 <- function(parms)
  {
    a2 <- parms[1]
    b2 <- 0
    a3 <- parms[2]
    b3 <- 0
    a4 <- parms[3]
    b4 <- 0
    a5 <- parms[4]
    b5 <- 0

    l2 <- a2+b2*df$sbn
    l3 <- a3+b3*df$sbn
    l4 <- a4+b4*df$sbn
    l5 <- a5+b5*df$sbn

    py1 <- 1/(1+exp(l2)+exp(l3)+exp(l4)+exp(l5))
    py2 <- exp(l2)/(1+exp(l2)+exp(l3)+exp(l4)+exp(l5))
    py3 <- exp(l3)/(1+exp(l2)+exp(l3)+exp(l4)+exp(l5))
    py4 <- exp(l4)/(1+exp(l2)+exp(l3)+exp(l4)+exp(l5))
    py5 <- exp(l5)/(1+exp(l2)+exp(l3)+exp(l4)+exp(l5))

    pmf <- df$y1*py1+df$y2*py2+df$y3*py3+df$y4*py4+df$y5*py5
    lpmf <- log(pmf)
    return(lpmf)
  }

m2 <- maxLik(ll2,start=c(-3.94156,-2.30173,-3.92457,-5.59979),
  method="BHHH",finalHessian="BHHH")
summary(m2)
```

* Here is a summary of the parameter estimates from the intercept-only model (model 2):

```rout
> ll2 <- function(parms)
+   {
+     a2 <- parms[1]
+     b2 <- 0
+     a3 <- parms[2]
+     b3 <- 0
+     a4 <- parms[3]
+     b4 <- 0
+     a5 <- parms[4]
+     b5 <- 0
+ 
+     l2 <- a2+b2*df$sbn
+     l3 <- a3+b3*df$sbn
+     l4 <- a4+b4*df$sbn
+     l5 <- a5+b5*df$sbn
+ 
+     py1 <- 1/(1+exp(l2)+exp(l3)+exp(l4)+exp(l5))
+     py2 <- exp(l2)/(1+exp(l2)+exp(l3)+exp(l4)+exp(l5))
+     py3 <- exp(l3)/(1+exp(l2)+exp(l3)+exp(l4)+exp(l5))
+     py4 <- exp(l4)/(1+exp(l2)+exp(l3)+exp(l4)+exp(l5))
+     py5 <- exp(l5)/(1+exp(l2)+exp(l3)+exp(l4)+exp(l5))
+ 
+     pmf <- df$y1*py1+df$y2*py2+df$y3*py3+df$y4*py4+df$y5*py5
+     lpmf <- log(pmf)
+     return(lpmf)
+   }
> 
> m2 <- maxLik(ll2,start=c(-3.94156,-2.30173,-3.92457,-5.59979),
+   method="BHHH",finalHessian="BHHH")
> summary(m2)
--------------------------------------------
Maximum Likelihood estimation
BHHH maximisation, 4 iterations
Return code 8: successive function values within relative tolerance limit (reltol)
Log-Likelihood: -26413.26 
4  free parameters
Estimates:
     Estimate Std. error t value Pr(> t)    
[1,] -4.23703    0.03354 -126.34  <2e-16 ***
[2,] -2.63896    0.01550 -170.24  <2e-16 ***
[3,] -4.38878    0.03614 -121.43  <2e-16 ***
[4,] -6.11439    0.08522  -71.75  <2e-16 ***
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
--------------------------------------------
> 
```

* To see what these parameter estimates imply about the probability distribution of *y*, we can perform the following calculations:


```r
# save the parameter estimates

a2 <- coef(m2)[1]
a3 <- coef(m2)[2]
a4 <- coef(m2)[3]
a5 <- coef(m2)[4]

# calculate logits

l2 <- a2
l3 <- a3
l4 <- a4
l5 <- a5

# calculate cell probabilities 

py1 <- 1/(1+exp(l2)+exp(l3)+exp(l4)+exp(l5))
py2 <- exp(l2)/(1+exp(l2)+exp(l3)+exp(l4)+exp(l5))
py3 <- exp(l3)/(1+exp(l2)+exp(l3)+exp(l4)+exp(l5))
py4 <- exp(l4)/(1+exp(l2)+exp(l3)+exp(l4)+exp(l5))
py5 <- exp(l5)/(1+exp(l2)+exp(l3)+exp(l4)+exp(l5))

# display the results

c(py1,py2,py3,py4,py5)
```

* We get these results:


```rout
> # display the results
> 
> c(py1,py2,py3,py4,py5)
[1] 0.908667424 0.013130695 0.064911055 0.011281917 0.002008909
> 
```

* Now, we can check this against the frequency distribution in the raw data:

```r
# check against the frequency table

table(df$y)/nrow(df)
```

which yields:

```rout
> # check against the frequency table
> 
> table(df$y)/nrow(df)

          1           2           3           4           5 
0.908667424 0.013130696 0.064911055 0.011281917 0.002008909 
> 
```

* Notice that the constrained model generates "compromise" estimates of each cell probabilities.
* We are now ready to calculate the log-likelihood ratio test:

```r
# log-likelihood ratio test

# note that this is a comparison of an 8-parameter model 
# and a 4-parameter model. Here is the log-likelihood ratio test:

logLik(m1)
logLik(m2)

ts <- -2*(-26413.26-(-26013.69))
ts
1-pchisq(q=ts,df=4)
```

* Here are the results:

```rout
> # log-likelihood ratio test
> 
> # note that this is a comparison of an 8-parameter model 
> # and a 4-parameter model. Here is the log-likelihood ratio test:
> 
> logLik(m1)
[1] -26013.69
attr(,"df")
[1] 8
> logLik(m2)
[1] -26413.26
attr(,"df")
[1] 4
> 
> ts <- -2*(-26413.26-(-26013.69))
> ts
[1] 799.14
> 1-pchisq(q=ts,df=4)
[1] 0
> 
```

* Based on this evidence, we conclude that the model adjusting the distribution of *y* for seat belt use is more consistent with the data.
* Let us now turn to the issue of whether our conclusion is altered in any way by further adjusting for urban/rural geography.
* We begin by recoding urban/rural into a numeric variable (we will call it "urban") for analysis:

```r
df$urban <- rep(NA,nrow(df))
df$urban[df$ur=="u"] <- 1
df$urban[df$ur=="r"] <- 0
table(df$ur,df$urban,exclude=NULL)
```

* Here are the results:

```rout
> df$urban <- rep(NA,nrow(df))
> df$urban[df$ur=="u"] <- 1
> df$urban[df$ur=="r"] <- 0
> table(df$ur,df$urban,exclude=NULL)
   
        0     1
  r 25523     0
  u     0 43171
> 
```

* We now estimate model 3 which is an elaboration of model 1 -- allowing for main effects of seat belt use and urban rural geography:

```r
ll3 <- function(parms)
  {
    a2 <- parms[1]
    b2 <- parms[2]
    c2 <- parms[3]
    a3 <- parms[4]
    b3 <- parms[5]
    c3 <- parms[6]
    a4 <- parms[7]
    b4 <- parms[8]
    c4 <- parms[9]
    a5 <- parms[10]
    b5 <- parms[11]
    c5 <- parms[12]

    l2 <- a2+b2*df$sbn+c2*df$urban
    l3 <- a3+b3*df$sbn+c3*df$urban
    l4 <- a4+b4*df$sbn+c4*df$urban
    l5 <- a5+b5*df$sbn+c5*df$urban

    py1 <- 1/(1+exp(l2)+exp(l3)+exp(l4)+exp(l5))
    py2 <- exp(l2)/(1+exp(l2)+exp(l3)+exp(l4)+exp(l5))
    py3 <- exp(l3)/(1+exp(l2)+exp(l3)+exp(l4)+exp(l5))
    py4 <- exp(l4)/(1+exp(l2)+exp(l3)+exp(l4)+exp(l5))
    py5 <- exp(l5)/(1+exp(l2)+exp(l3)+exp(l4)+exp(l5))

    pmf <- df$y1*py1+df$y2*py2+df$y3*py3+df$y4*py4+df$y5*py5
    lpmf <- log(pmf)
    return(lpmf)
  }

m3 <- maxLik(ll3,start=c(-3.94156,-0.60018,0.00372,
                               -2.30173,-0.70317,0.00372,
                               -3.92457,-1.06462,0.00372,
                               -5.5998,-1.2366,0.00372),
  method="BHHH",finalHessian="BHHH")
summary(m3)
```

* Here are the results:

```rout
> ll3 <- function(parms)
+   {
+     a2 <- parms[1]
+     b2 <- parms[2]
+     c2 <- parms[3]
+     a3 <- parms[4]
+     b3 <- parms[5]
+     c3 <- parms[6]
+     a4 <- parms[7]
+     b4 <- parms[8]
+     c4 <- parms[9]
+     a5 <- parms[10]
+     b5 <- parms[11]
+     c5 <- parms[12]
+ 
+     l2 <- a2+b2*df$sbn+c2*df$urban
+     l3 <- a3+b3*df$sbn+c3*df$urban
+     l4 <- a4+b4*df$sbn+c4*df$urban
+     l5 <- a5+b5*df$sbn+c5*df$urban
+ 
+     py1 <- 1/(1+exp(l2)+exp(l3)+exp(l4)+exp(l5))
+     py2 <- exp(l2)/(1+exp(l2)+exp(l3)+exp(l4)+exp(l5))
+     py3 <- exp(l3)/(1+exp(l2)+exp(l3)+exp(l4)+exp(l5))
+     py4 <- exp(l4)/(1+exp(l2)+exp(l3)+exp(l4)+exp(l5))
+     py5 <- exp(l5)/(1+exp(l2)+exp(l3)+exp(l4)+exp(l5))
+ 
+     pmf <- df$y1*py1+df$y2*py2+df$y3*py3+df$y4*py4+df$y5*py5
+     lpmf <- log(pmf)
+     return(lpmf)
+   }
> 
> m3 <- maxLik(ll3,start=c(-3.94156,-0.60018,0.00372,
+                                -2.30173,-0.70317,0.00372,
+                                -3.92457,-1.06462,0.00372,
+                                -5.5998,-1.2366,0.00372),
+   method="BHHH",finalHessian="BHHH")
> summary(m3)
--------------------------------------------
Maximum Likelihood estimation
BHHH maximisation, 5 iterations
Return code 8: successive function values within relative tolerance limit (reltol)
Log-Likelihood: -25587.14 
12  free parameters
Estimates:
      Estimate Std. error t value  Pr(> t)    
 [1,] -3.75859    0.05976 -62.895  < 2e-16 ***
 [2,] -0.60474    0.06805  -8.887  < 2e-16 ***
 [3,] -0.29589    0.06799  -4.352 1.35e-05 ***
 [4,] -1.90206    0.02547 -74.691  < 2e-16 ***
 [5,] -0.71430    0.03182 -22.445  < 2e-16 ***
 [6,] -0.70252    0.03126 -22.472  < 2e-16 ***
 [7,] -3.30597    0.05150 -64.193  < 2e-16 ***
 [8,] -1.08394    0.07817 -13.867  < 2e-16 ***
 [9,] -1.22625    0.07584 -16.169  < 2e-16 ***
[10,] -4.81938    0.11129 -43.305  < 2e-16 ***
[11,] -1.26318    0.19085  -6.619 3.63e-11 ***
[12,] -1.77132    0.19980  -8.865  < 2e-16 ***
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
--------------------------------------------
> 
```

* We can calculate a log-likelihood ratio test comparing model 1 (simpler 8-parameter model) to model 3 (more complicated 12-parameter model). Here is the R code for this comparison:

```r
# compare model 1 to model 3

logLik(m1)
logLik(m3)

ts <- -2*(-26013.69-(-25587.14))
ts
1-pchisq(q=ts,df=4)
```

* Here are the results:

```rout
> # compare model 1 to model 3
> 
> logLik(m1)
[1] -26013.69
attr(,"df")
[1] 8
> logLik(m3)
[1] -25587.14
attr(,"df")
[1] 12
> 
> ts <- -2*(-26013.69-(-25587.14))
> ts
[1] 853.1
> 1-pchisq(q=ts,df=4)
[1] 0
> 
```

* Based on this evidence, we conclude that urban rural geography does coincide with the outcome variable. We can judge the magnitude of this effect by calculating the predicted probability distribution of *y* conditional on both seat belt use and urban/rural geography:

```r
# calculate predicted probabilities based on model 3
# first, save the parameter estimates

a2 <- coef(m3)[1]
b2 <- coef(m3)[2]
c2 <- coef(m3)[3]

a3 <- coef(m3)[4]
b3 <- coef(m3)[5]
c3 <- coef(m3)[6]

a4 <- coef(m3)[7]
b4 <- coef(m3)[8]
c4 <- coef(m3)[9]

a5 <- coef(m3)[10]
b5 <- coef(m3)[11]
c5 <- coef(m3)[12]

# calculate predicted probabilities

# 1. p(y=?|sb=no,rural)

l2.00 <- a2+b2*0+c2*0
l3.00 <- a3+b3*0+c3*0
l4.00 <- a4+b4*0+c4*0
l5.00 <- a5+b5*0+c5*0

py1.00 <- 1/(1+exp(l2.00)+exp(l3.00)+exp(l4.00)+exp(l5.00))
py2.00 <- exp(l2.00)/(1+exp(l2.00)+exp(l3.00)+exp(l4.00)+exp(l5.00))
py3.00 <- exp(l3.00)/(1+exp(l2.00)+exp(l3.00)+exp(l4.00)+exp(l5.00))
py4.00 <- exp(l4.00)/(1+exp(l2.00)+exp(l3.00)+exp(l4.00)+exp(l5.00))
py5.00 <- exp(l5.00)/(1+exp(l2.00)+exp(l3.00)+exp(l4.00)+exp(l5.00))

# 2. p(y=?|sb=no,urban)

l2.01 <- a2+b2*0+c2*1
l3.01 <- a3+b3*0+c3*1
l4.01 <- a4+b4*0+c4*1
l5.01 <- a5+b5*0+c5*1

py1.01 <- 1/(1+exp(l2.01)+exp(l3.01)+exp(l4.01)+exp(l5.01))
py2.01 <- exp(l2.01)/(1+exp(l2.01)+exp(l3.01)+exp(l4.01)+exp(l5.01))
py3.01 <- exp(l3.01)/(1+exp(l2.01)+exp(l3.01)+exp(l4.01)+exp(l5.01))
py4.01 <- exp(l4.01)/(1+exp(l2.01)+exp(l3.01)+exp(l4.01)+exp(l5.01))
py5.01 <- exp(l5.01)/(1+exp(l2.01)+exp(l3.01)+exp(l4.01)+exp(l5.01))

# 3. p(y=?|sb=yes,rural)

l2.10 <- a2+b2*1+c2*0
l3.10 <- a3+b3*1+c3*0
l4.10 <- a4+b4*1+c4*0
l5.10 <- a5+b5*1+c5*0

py1.10 <- 1/(1+exp(l2.10)+exp(l3.10)+exp(l4.10)+exp(l5.10))
py2.10 <- exp(l2.10)/(1+exp(l2.10)+exp(l3.10)+exp(l4.10)+exp(l5.10))
py3.10 <- exp(l3.10)/(1+exp(l2.10)+exp(l3.10)+exp(l4.10)+exp(l5.10))
py4.10 <- exp(l4.10)/(1+exp(l2.10)+exp(l3.10)+exp(l4.10)+exp(l5.10))
py5.10 <- exp(l5.10)/(1+exp(l2.10)+exp(l3.10)+exp(l4.10)+exp(l5.10))

# 4. p(y=?|sb=yes,urban)

l2.11 <- a2+b2*1+c2*1
l3.11 <- a3+b3*1+c3*1
l4.11 <- a4+b4*1+c4*1
l5.11 <- a5+b5*1+c5*1

py1.11 <- 1/(1+exp(l2.11)+exp(l3.11)+exp(l4.11)+exp(l5.11))
py2.11 <- exp(l2.11)/(1+exp(l2.11)+exp(l3.11)+exp(l4.11)+exp(l5.11))
py3.11 <- exp(l3.11)/(1+exp(l2.11)+exp(l3.11)+exp(l4.11)+exp(l5.11))
py4.11 <- exp(l4.11)/(1+exp(l2.11)+exp(l3.11)+exp(l4.11)+exp(l5.11))
py5.11 <- exp(l5.11)/(1+exp(l2.11)+exp(l3.11)+exp(l4.11)+exp(l5.11))

# display the results

c(py1.00,py2.00,py3.00,py4.00,py5.00)
c(py1.01,py2.01,py3.01,py4.01,py5.01)
c(py1.10,py2.10,py3.10,py4.10,py5.10)
c(py1.11,py2.11,py3.11,py4.11,py5.11)
```

* Here are the results:

```rout
> # display the results
> 
> c(py1.00,py2.00,py3.00,py4.00,py5.00)
[1] 0.82148091 0.01915419 0.12261549 0.03011857 0.00663084
> c(py1.01,py2.01,py3.01,py4.01,py5.01)
[1] 0.906282555 0.015719050 0.067005312 0.009748677 0.001244405
> c(py1.10,py2.10,py3.10,py4.10,py5.10)
[1] 0.90868689 0.01157297 0.06639678 0.01126944 0.00207392
> c(py1.11,py2.11,py3.11,py4.11,py5.11)
[1] 0.9526584084 0.0090253548 0.0344800396 0.0034663331 0.0003698641
> 
```

* Let's think about what this information is telling us. 
* First, p(y=1|sb=no, rural) = 0.821.
* Second, p(y=1|sb=no, urban) = 0.906
* These results suggest that accidents in urban areas are less likely to involve injuries compared to those that occurred in rural areas.
* Similar results are evident when seat belts are used: p(y=1|sb=yes,rural) = 0.909 and p(y=1|sb=yes,urban) = 0.953.
* This model does impose an important constraint on the analysis: it assumes there is no interaction between seat belt use and urban/rural geography.
* We can relax this constraint to see if a more general model is warranted.

```r
# now relax the constraint that seatbelt usage has the
# same coefficient for urban and rural persons

ll4 <- function(parms)
  {

    a2 <- parms[1]
    b2 <- parms[2]
    c2 <- parms[3]
    d2 <- parms[4]
    a3 <- parms[5]
    b3 <- parms[6]
    c3 <- parms[7]
    d3 <- parms[8]
    a4 <- parms[9]
    b4 <- parms[10]
    c4 <- parms[11]
    d4 <- parms[12]
    a5 <- parms[13]
    b5 <- parms[14]
    c5 <- parms[15]
    d5 <- parms[16]

    l2 <- a2+b2*df$sbn+c2*df$urban+d2*df$urban*df$sbn
    l3 <- a3+b3*df$sbn+c3*df$urban+d3*df$urban*df$sbn
    l4 <- a4+b4*df$sbn+c4*df$urban+d4*df$urban*df$sbn
    l5 <- a5+b5*df$sbn+c5*df$urban+d5*df$urban*df$sbn

    py1 <- 1/(1+exp(l2)+exp(l3)+exp(l4)+exp(l5))
    py2 <- exp(l2)/(1+exp(l2)+exp(l3)+exp(l4)+exp(l5))
    py3 <- exp(l3)/(1+exp(l2)+exp(l3)+exp(l4)+exp(l5))
    py4 <- exp(l4)/(1+exp(l2)+exp(l3)+exp(l4)+exp(l5))
    py5 <- exp(l5)/(1+exp(l2)+exp(l3)+exp(l4)+exp(l5))

    pmf <- df$y1*py1+df$y2*py2+df$y3*py3+df$y4*py4+df$y5*py5
    lpmf <- log(pmf)
    return(lpmf)
  }

m4 <- maxLik(ll4,start=c(-3.75859,-0.60474,-0.29589,0.00328402,
                               -1.90206,-0.71430,0.70252,0.00328402,
                               -3.30597,-1.08394,-1.22625,0.00328402,
                               -4.81938,-1.26318,-1.77132,0.00328402),
  method="BHHH",finalHessian="BHHH")
summary(m4)
```

* Here are the results:

```rout
> summary(m4)
--------------------------------------------
Maximum Likelihood estimation
BHHH maximisation, 5 iterations
Return code 8: successive function values within relative tolerance limit (reltol)
Log-Likelihood: -25586.08 
16  free parameters
Estimates:
      Estimate Std. error t value  Pr(> t)    
 [1,] -3.77919    0.06913 -54.664  < 2e-16 ***
 [2,] -0.55616    0.10397  -5.349 8.84e-08 ***
 [3,] -0.26053    0.08973  -2.903  0.00369 ** 
 [4,] -0.08555    0.13752  -0.622  0.53389    
 [5,] -1.88675    0.02848 -66.254  < 2e-16 ***
 [6,] -0.75145    0.04449 -16.890  < 2e-16 ***
 [7,] -0.73347    0.04056 -18.083  < 2e-16 ***
 [8,]  0.07654    0.06367   1.202  0.22932    
 [9,] -3.29584    0.05467 -60.288  < 2e-16 ***
[10,] -1.11361    0.09735 -11.439  < 2e-16 ***
[11,] -1.25256    0.09161 -13.672  < 2e-16 ***
[12,]  0.08091    0.16337   0.495  0.62042    
[13,] -4.81443    0.11517 -41.802  < 2e-16 ***
[14,] -1.27758    0.21869  -5.842 5.16e-09 ***
[15,] -1.78703    0.23449  -7.621 2.52e-14 ***
[16,]  0.05251    0.44800   0.117  0.90670    
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
--------------------------------------------
> 
```

* We now conduct a log-likelihood ratio test comparing model 3 (more restrictive 12-parameter model) to model 4 (more general 16-parameter model):

```r
# compare model 4 to model 3

logLik(m3)
logLik(m4)

ts <- -2*(-25587.14-(-25586.08))
ts
1-pchisq(q=ts,df=4)
```

* The results of this test are not statistically significant. The more general 16-parameter model did not significantly improve on the more restrictive 12-parameter model:

```rout
> # compare model 4 to model 3
> 
> logLik(m3)
[1] -25587.14
attr(,"df")
[1] 12
> logLik(m4)
[1] -25586.08
attr(,"df")
[1] 16
> 
> ts <- -2*(-25587.14-(-25586.08))
> ts
[1] 2.12
> 1-pchisq(q=ts,df=4)
[1] 0.713699
> 
```

* We can use the parameter estimates from the more general model to calculate the probability distribution of y allowing for the possibility of an interaction:


```r
# calculate predicted probabilities based on model 4
# (allowing for the possibility that there is an
# interaction between urban/rural and seatbelt usage

a2 <- coef(m4)[1]
b2 <- coef(m4)[2]
c2 <- coef(m4)[3]
d2 <- coef(m4)[4]

a3 <- coef(m4)[5]
b3 <- coef(m4)[6]
c3 <- coef(m4)[7]
d3 <- coef(m4)[8]

a4 <- coef(m4)[9]
b4 <- coef(m4)[10]
c4 <- coef(m4)[11]
d4 <- coef(m4)[12]

a5 <- coef(m4)[13]
b5 <- coef(m4)[14]
c5 <- coef(m4)[15]
d5 <- coef(m4)[16]

# 1. p(y=?|sb=no,rural)

l2.00 <- a2+b2*0+c2*0+d2*0*0
l3.00 <- a3+b3*0+c3*0+d3*0*0
l4.00 <- a4+b4*0+c4*0+d4*0*0
l5.00 <- a5+b5*0+c5*0+d5*0*0

py1.00.i <- 1/(1+exp(l2.00)+exp(l3.00)+exp(l4.00)+exp(l5.00))
py2.00.i <- exp(l2.00)/(1+exp(l2.00)+exp(l3.00)+exp(l4.00)+exp(l5.00))
py3.00.i <- exp(l3.00)/(1+exp(l2.00)+exp(l3.00)+exp(l4.00)+exp(l5.00))
py4.00.i <- exp(l4.00)/(1+exp(l2.00)+exp(l3.00)+exp(l4.00)+exp(l5.00))
py5.00.i <- exp(l5.00)/(1+exp(l2.00)+exp(l3.00)+exp(l4.00)+exp(l5.00))

# 2. p(y=?|sb=no,urban)

l2.01 <- a2+b2*0+c2*1+d2*0*1
l3.01 <- a3+b3*0+c3*1+d3*0*1
l4.01 <- a4+b4*0+c4*1+d4*0*1
l5.01 <- a5+b5*0+c5*1+d5*0*1

py1.01.i <- 1/(1+exp(l2.01)+exp(l3.01)+exp(l4.01)+exp(l5.01))
py2.01.i <- exp(l2.01)/(1+exp(l2.01)+exp(l3.01)+exp(l4.01)+exp(l5.01))
py3.01.i <- exp(l3.01)/(1+exp(l2.01)+exp(l3.01)+exp(l4.01)+exp(l5.01))
py4.01.i <- exp(l4.01)/(1+exp(l2.01)+exp(l3.01)+exp(l4.01)+exp(l5.01))
py5.01.i <- exp(l5.01)/(1+exp(l2.01)+exp(l3.01)+exp(l4.01)+exp(l5.01))

# 3. p(y=?|sb=yes,rural)

l2.10 <- a2+b2*1+c2*0+d2*1*0
l3.10 <- a3+b3*1+c3*0+d3*1*0
l4.10 <- a4+b4*1+c4*0+d4*1*0
l5.10 <- a5+b5*1+c5*0+d5*1*0

py1.10.i <- 1/(1+exp(l2.10)+exp(l3.10)+exp(l4.10)+exp(l5.10))
py2.10.i <- exp(l2.10)/(1+exp(l2.10)+exp(l3.10)+exp(l4.10)+exp(l5.10))
py3.10.i <- exp(l3.10)/(1+exp(l2.10)+exp(l3.10)+exp(l4.10)+exp(l5.10))
py4.10.i <- exp(l4.10)/(1+exp(l2.10)+exp(l3.10)+exp(l4.10)+exp(l5.10))
py5.10.i <- exp(l5.10)/(1+exp(l2.10)+exp(l3.10)+exp(l4.10)+exp(l5.10))

# 4. p(y=?|sb=yes,urban)

l2.11 <- a2+b2*1+c2*1+d2*1*1
l3.11 <- a3+b3*1+c3*1+d3*1*1
l4.11 <- a4+b4*1+c4*1+d4*1*1
l5.11 <- a5+b5*1+c5*1+d5*1*1

py1.11.i <- 1/(1+exp(l2.11)+exp(l3.11)+exp(l4.11)+exp(l5.11))
py2.11.i <- exp(l2.11)/(1+exp(l2.11)+exp(l3.11)+exp(l4.11)+exp(l5.11))
py3.11.i <- exp(l3.11)/(1+exp(l2.11)+exp(l3.11)+exp(l4.11)+exp(l5.11))
py4.11.i <- exp(l4.11)/(1+exp(l2.11)+exp(l3.11)+exp(l4.11)+exp(l5.11))
py5.11.i <- exp(l5.11)/(1+exp(l2.11)+exp(l3.11)+exp(l4.11)+exp(l5.11))
```

* Then, we can compare the two probability distributions:

```r
# re-display the results - without interaction

c(py1.00,py2.00,py3.00,py4.00,py5.00)
c(py1.01,py2.01,py3.01,py4.01,py5.01)
c(py1.10,py2.10,py3.10,py4.10,py5.10)
c(py1.11,py2.11,py3.11,py4.11,py5.11)

# display the results - with interaction

c(py1.00.i,py2.00.i,py3.00.i,py4.00.i,py5.00.i)
c(py1.01.i,py2.01.i,py3.01.i,py4.01.i,py5.01.i)
c(py1.10.i,py2.10.i,py3.10.i,py4.10.i,py5.10.i)
c(py1.11.i,py2.11.i,py3.11.i,py4.11.i,py5.11.i)
```

* Here are the results:

```rout
> # re-display the results - without interaction
> 
> c(py1.00,py2.00,py3.00,py4.00,py5.00)
[1] 0.82148091 0.01915419 0.12261549 0.03011857 0.00663084
> c(py1.01,py2.01,py3.01,py4.01,py5.01)
[1] 0.906282555 0.015719050 0.067005312 0.009748677 0.001244405
> c(py1.10,py2.10,py3.10,py4.10,py5.10)
[1] 0.90868689 0.01157297 0.06639678 0.01126944 0.00207392
> c(py1.11,py2.11,py3.11,py4.11,py5.11)
[1] 0.9526584084 0.0090253548 0.0344800396 0.0034663331 0.0003698641
> 
> # display the results - with interaction
> 
> c(py1.00.i,py2.00.i,py3.00.i,py4.00.i,py5.00.i)
[1] 0.819971993 0.018729215 0.124277963 0.030369333 0.006651497
> c(py1.01.i,py2.01.i,py3.01.i,py4.01.i,py5.01.i)
[1] 0.907167797 0.015968371 0.066029986 0.009601561 0.001232286
> c(py1.10.i,py2.10.i,py3.10.i,py4.10.i,py5.10.i)
[1] 0.909909910 0.011917429 0.065049301 0.011066184 0.002057175
> c(py1.11.i,py2.11.i,py3.11.i,py4.11.i,py5.11.i)
[1] 0.9519307881 0.0088204264 0.0352817040 0.0035872546 0.0003798269
> 
```

* We can see from these results that there is barely any difference in the two sets of probability distributions. 
* It is also worth noting that the results from the more general model are identical to what we see in the frequency tables:

```r
table(df$y,df$sb,df$ur)
```

which yields:

```rout
> table(df$y,df$sb,df$ur)
, ,  = r

   
       no   yes
  1  9369 12827
  2   214   168
  3  1420   917
  4   347   156
  5    76    29

, ,  = u

   
       no   yes
  1 17668 22556
  2   311   209
  3  1286   836
  4   187    85
  5    24     9

> 
```

* Consider, for example, the following calculation based on rural persons who did not wear a seatbelt:

```rout
> 9369/(9369+214+1420+347+76)
[1] 0.819972
> 
```

which is exactly the number we see in the interaction model. So, the simpler model slightly (but not significantly) distorts the joint distribution of these three variables.

* Next, we use a canned procedure in R to verify that we can get the same results:

```r
library(nnet)
mlogit <- multinom(y~1+sbn,data=df)
summary(mlogit)
logLik(mlogit)
```

which gives us the following output (please compare to output above):

```rout
> library(nnet)
> 
> mlogit <- multinom(y~1+sbn,data=df)
# weights:  15 (8 variable)
initial  value 110558.727957 
iter  10 value 26273.408126
iter  20 value 26016.606470
final  value 26013.686582 
converged
> summary(mlogit)
Call:
multinom(formula = y ~ 1 + sbn, data = df)

Coefficients:
  (Intercept)        sbn
2   -3.941561 -0.6001812
3   -2.301734 -0.7031680
4   -3.924569 -1.0646196
5   -5.599792 -1.2366376

Std. Errors:
  (Intercept)        sbn
2  0.04406523 0.06798918
3  0.02016272 0.03170564
4  0.04369955 0.07802104
5  0.10018477 0.19074012

Residual Deviance: 52027.37 
AIC: 52043.37 
> logLik(mlogit)
'log Lik.' -26013.69 (df=8)
> 
```

* Now, let's post-process the results of this analysis and verify that we get the same predicted distribution of *y* that we obtained last week:

```r
a2 <- coef(mlogit)[1,1]
b2 <- coef(mlogit)[1,2]
a3 <- coef(mlogit)[2,1]
b3 <- coef(mlogit)[2,2]
a4 <- coef(mlogit)[3,1]
b4 <- coef(mlogit)[3,2]
a5 <- coef(mlogit)[4,1]
b5 <- coef(mlogit)[4,2]

l2n <- a2+b2*0
l2y <- a2+b2*1

l3n <- a3+b3*0
l3y <- a3+b3*1

l4n <- a4+b4*0
l4y <- a4+b4*1

l5n <- a5+b5*0
l5y <- a5+b5*1

py1n <- 1/(1+exp(l2n)+exp(l3n)+exp(l4n)+exp(l5n))
py1y <- 1/(1+exp(l2y)+exp(l3y)+exp(l4y)+exp(l5y))

py2n <- exp(l2n)/(1+exp(l2n)+exp(l3n)+exp(l4n)+exp(l5n))
py2y <- exp(l2y)/(1+exp(l2y)+exp(l3y)+exp(l4y)+exp(l5y))

py3n <- exp(l3n)/(1+exp(l2n)+exp(l3n)+exp(l4n)+exp(l5n))
py3y <- exp(l3y)/(1+exp(l2y)+exp(l3y)+exp(l4y)+exp(l5y))

py4n <- exp(l4n)/(1+exp(l2n)+exp(l3n)+exp(l4n)+exp(l5n))
py4y <- exp(l4y)/(1+exp(l2y)+exp(l3y)+exp(l4y)+exp(l5y))

py5n <- exp(l5n)/(1+exp(l2n)+exp(l3n)+exp(l4n)+exp(l5n))
py5y <- exp(l5y)/(1+exp(l2y)+exp(l3y)+exp(l4y)+exp(l5y))

# display the results

no.vector <- c(py1n,py2n,py3n,py4n,py5n)
no.vector
yes.vector <- c(py1y,py2y,py3y,py4y,py5y)
yes.vector
yes.vector-no.vector
```

* Here are the results:

```rout
> # display the results
> 
> no.vector <- c(py1n,py2n,py3n,py4n,py5n)
> no.vector
[1] 0.874927140 0.016989227 0.087567212 0.017280386 0.003236036
> yes.vector <- c(py1y,py2y,py3y,py4y,py5y)
> yes.vector
[1] 0.936256334 0.009975648 0.046385520 0.006377022 0.001005475
> yes.vector-no.vector
[1]  0.061329195 -0.007013578 -0.041181692 -0.010903364 -0.002230561
> 
```

which tell us that wearing a seat belt is indeed associated with better outcomes throughout the range of the distribution of *y*.
