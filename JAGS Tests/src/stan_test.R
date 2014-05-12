---
title: "Simulated RIT Equating"
author: "Chris Haid"
date: "April 7, 2014"
output:
  html_document:
    fig_height: 6
---
This is a reprodiction of the `JAGS` test I did two weeks ago, but using `RStan` (and thus `Stan`) rather than `JAGS`.  I nice feature of `RStan` is that is automagically installs `Stan` for you, so you don't have to go through the machinations of install a separate MCMC sampler outside of `R` (as you do with `WinBUGS`/`JAGS`) First, let's load the packages we'll use
```{r prerequisites, message=FALSE, warning=FALSE}
require(rstan) #rather than rjags
require(Rgraphviz)
require(data.table)
```
Create some simulated data:
```{r norm_sim_data}
set.seed(432104)
```

# Simulated RIT Equating Example (All of this is the same as the `JAGS` example)

## First pass at a model
OK. So let's define a simple model is the basis for  our simulated data and our probabilistic model of inference.  First we'll assume that there is in fact a latent ability attribute, $\theta_i$, for each student $i \in {1,\dots,.n}$ in our sample.  Further, we'll assume that this latent trait is accurately measured by an *unobserved* RIT score, which we'll denote $r_{i}$. In other words, RIT scores are a direct measure of $\theta_i$. Luckily for us, students take a test that gives us an estimate of RIT scores, the MAP.  So we assume that the observed RIT, $\hat{r}$ is a realization from a distribution  centered on $r$
Finally, we assume that all other academic measures (interim assessments, Khan objectives, individual test items, even Do Nows/Exit Tickets).  So we have observed realization of a RIT score for a student as well as all the other academic indicators.  All other RVs and parameters need to be estimated.  And our goal here is to make inferences about $r_i$ given our observation of other academic indicators.  

Here's a graphical representation of our probability model:

```{r equat_graph}


rit.graph <- new("graphNEL", 
                 nodes=c("theta[i]",
                         "r[i]",
                         "rhat[i]",
                         "a[i]",
                         "k[i]",
                         "t[i]",
                         "ahat[i]",
                         "khat[i]",
                         "that[i]"
                         ),
                 edgemode="directed")

rit.graph <- addEdge("theta[i]", "r[i]", rit.graph, 1)
rit.graph <- addEdge("r[i]", "rhat[i]" , rit.graph, 1)
rit.graph <- addEdge("rhat[i]", "a[i]" , rit.graph, 1)
rit.graph <- addEdge("rhat[i]", "k[i]" , rit.graph, 1)
rit.graph <- addEdge("rhat[i]", "t[i]" , rit.graph, 1)
rit.graph <- addEdge("a[i]", "ahat[i]" , rit.graph, 1)
rit.graph <- addEdge("k[i]", "khat[i]" , rit.graph, 1)
rit.graph <- addEdge("t[i]", "that[i]" , rit.graph, 1)

plot(rit.graph)

```
Now, let's simulate some data based on this basic probability model. The $\theta_i$'s need a prior and I'll use the school level norms (because I'm on a plane and don't have the student level norms with me; we can change these later) for fall 5th grade math.  Thinking about this, we can collapse the $\theta_i$s and $r_i$, since we are assuming theta exists on the the RIT scale. I'll use $\theta_i$ to denote the RIT-scaled latent variable. 

```{r simulate_thetas}
set.seed(432104)

N <- 30 # number of students

r<-rnorm(n = N, mean = 211.39, sd =5.83)
```
The $\theta_i$s are then noisily measured by the MAP test.  I'll assume a standard deviation of about 3, but that is completely arbitrary.
```{r simulate_rhats}
rhat<-rnorm(N, r, sd=rep(3, times=N))

head(rhat)
```
OK. let's make some assumptions about how $\hat{r}$ is related to each of the three realizations of our academic indicators. Let's start with some arbitrary assessment that is graded on a zero to one scale (or zero to 100%) a realization of which is denoted $\hat{a}_i$. We'll keep it simple and assume that the $\hat{r}_i$ are linear predictors of $z.a_{i}$ in the inverse-logistic function.  That is,

$$
\begin{aligned}
  z.a_{i}   & = \alpha + \beta \cdot \hat{r}_i \\
  \hat{a}_{i} & = \frac{1}{1+\exp^{-z.a{i}}}
\end{aligned}
$$


Assuming $\alpha = -1$ and $\beta = 0.01$, we simulating the data like so:
```{r simulate_ahats}
alpha<- -1
beta<-.01
z.a<- alpha + beta*rhat
head(z.a)

a<-1/(1+exp(-z.a))
head(a)

ahat <- rnorm(n=N, mean=a,sd=.025)
head(ahat)
```

Let's pretend, for now, that this the only non-MAP assessment that we have.  Consequently, all we have observed are the $\hat{a}_i$ and the current RIT scores $\hat{r}_i$.  Here's the simplified model in `JAGS/BUGS` syntax:

```{r stan_model}
model.stan<-'
  data {
    int<lower=0> N; // number of students
    real<lower=.6, upper=.9> ahat[N]; // observed assessment scores
    real<lower=175, upper=250> rhat[N]; // observed RIT scores
  }

  parameters {
    real r[N]; 
    real alpha;
    real beta;
    real<lower=0> sigma;
    real<lower=0> sigma_a;
    
  }

  transformed parameters {
  // vector[N] z;
   real<lower=0, upper=1> a[N];

    for (i in 1:N) {
      // z[i] <- alpha + (beta * rhat[i]);
      a[i] <- 1 / (1 + exp(-(alpha + (beta * rhat[i]))));
    }

  }

  model {
  
    beta ~ normal(0, 1000);
    alpha ~ normal(0, 1000);
    //sigma ~ uniform(0,1000);
    //sigma_a ~ uniform(0,1000);

    r  ~ normal(211.39, 5.39);

    rhat ~ normal(r,sigma);
    ahat ~ normal(a, sigma_a);
  }
'
```


Now run the mode and take a look at $\alpha$ and $\beta$ and see that they are relative the estimated values are relatively close to the acutal values (-1 and 0.01, respectivly):
```{r model1}
stan.data<-list('ahat' = ahat,
                'rhat' = rhat,
                'N' = N)

fit1 <- stan(model_code = model.stan,
                   data = stan.data,
                   #pars = c('a','r', 'alpha', 'beta'),
                   chains=2,
                   iter=100)


fit2 <- stan(fit = fit1,
             data = stan.data,
             pars = c('a','r', 'alpha', 'beta'),
             chains=2,
             iter=1000)

samps.jags
```

No let's look at the estimated $a_i$'s and $r_i$'s (the latter being our variable of interest)

```{r coda, fig.height=10}
samps.coda <- coda.samples(jags,
             c('a', 'r', 'beta', 'alpha'),
             10000)
summary.coda<-summary(samps.coda)

plot(samps.coda)
```

Let's extract the estimated $r_i$'s from the coda samples

```{r r_estimates}
r.estimated<-as.vector(summary.coda$statistics[93:nrow(summary.coda$statistics),"Mean"]) 

rs<-data.table(Actual=r, Estimated=r.estimated)
rs[,Residual:=Estimated-Actual]

rs
```
Nicley, the mean residual is `r mean(rs$Residual)` and the mean squared residual is `r mean(rs$Residual^2)`. Not too shabby!



