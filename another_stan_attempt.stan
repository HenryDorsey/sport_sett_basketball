//
// This Stan program defines a simple model, with a
// vector of values 'y' modeled as normally distributed
// with mean 'mu' and standard deviation 'sigma'.
//
// Learn more about model development with Stan at:
//
//    http://mc-stan.org/users/interfaces/rstan.html
//    https://github.com/stan-dev/rstan/wiki/RStan-Getting-Started
//
data {
  // Non-hierarchical
  int<lower=0> nt; //number of teams
  int<lower=0> ng; //number of games
  int<lower=0> toi[ng]; //toi index
  int<lower=0> opp[ng]; //opp index
  int<lower=0> s1[ng]; //toi score
  int<lower=0> s2[ng]; //opp score
  int<lower=0> toinew[np]; //toi index for pred
  int<lower=0> oppnew[np]; //opp index for pred
  
  // Hierarchical portion
 
}

parameters{
  real mu_t[nt]; //average
}