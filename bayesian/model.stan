data {
  int<lower=1> team; // number of teams
  int<lower=0> match; // number of matches
  int<lower=1> team1[match]; // team number 
  int<lower=1> team2[match]; // team number
  int out[match]; // outcome
}
parameters {
  real<lower=0,upper=140> str[team]; //strength by team 
  real<lower=0> vari[team]; //variance by team
}
model {
  for(i in 1:match){
    out[i] ~ normal(str[team1[i]] - str[team2[i]], vari[team1[i]] + vari[team2[i]]);
  }
}