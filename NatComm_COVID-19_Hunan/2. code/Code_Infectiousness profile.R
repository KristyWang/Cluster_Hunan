#set your working directory
setwd("...\\NatComm_COVID-19_Hunan\\")   
data<-data.frame(read.csv("1. input\\data_Table_S4.csv"))
data1<-data[data$prd==1 & !is.na(data$prd),]
data2<-data[data$prd==2 & !is.na(data$prd),]

##################proportion of pre-symptomatic transmission########################
#--- functions ---

#--- CDF of serial interval ---
p.Z  = function(z, gpar, wpar) {
  
  #--- infectiousness, gamma distribution ---
  # gpar[1:2]: hyper-parameters (gamma)
  # x        : infection time of infectee w.r.t onset time of infector
  f.Xc = function(x, gpar) { dgamma(x, gpar[1], gpar[2]) }
  
  #--- incubation, weibull distribution ---
  # wpar[1:2]: hyper-parameter (gamma)
  # y         : length of incubation period of infectee
  f.Y  = function(y, wpar) { dweibull(y, wpar[1], wpar[2]) }
  
  #--- convolution between incubation and infectiousness profile ---
  # gpar[3]: shift c days before symptom onset of infector
  # z      : length of serial interval
  f.Z = function(z, gpar, wpar) {
    integrate(
      f = function(x, z, gpar, wpar) { f.Y(z+gpar[3]-x, wpar)*f.Xc(x, gpar) },
      lower = -Inf, 
      upper = Inf,
      z     = z,
      gpar  = gpar,
      wpar = wpar,
      rel.tol = 0.001,
      subdivisions = 3000
    )$value
  } 
  f.Z2 = Vectorize(f.Z, vectorize.args = "z")
  
  #--- p.Z ---
  integrate(
    f = function(x, gpar, wpar) { f.Z2(x, gpar, wpar) },
    lower = -Inf,
    upper = z,
    gpar  = gpar,
    wpar = wpar,
    rel.tol = 0.001,
    subdivisions = 3000
  )$value
}
p.Z2 = Vectorize(p.Z, vectorize.args = c("z"))


#--- logLikelihood for the observed serial intervals ---
# x.lb: lower bound of infectors symtpom onset dates
# x.ub: upper bound of infectors symtpom onset dates
# y   : symptom onset dates of infectee
# 0.5 : continuity correction
lli.fx = function(gpar, x.lb, x.ub, y, wpar) {
  lli = pmax(p.Z2(y-(x.lb-0.5), gpar, wpar) - p.Z2(y-(x.ub+0.5), gpar, wpar),0)
  log_lli<-log(lli)
  return(-sum(log_lli))
}

#--- incubation period ---
# from Table S3 
# weibull distribution
w.par1 = 1.578104
w.par2 = 7.112123 

#
#--- estimation ---
#

#######overall#############
inf.fit = optim(
  c(19, 2.2, 17.5), lli.fx, 
  x.lb = data[, "x.lb"], 
  x.ub = data[, "x.ub"],
  y    = data[, "y"],
  wpar = c(w.par1, w.par2)
)

inf.par = inf.fit$par
#inf.par = c(18.1896828,0.9095662,20.7310048)
set.seed(20201007)
pgamma(inf.par[3], inf.par[1], inf.par[2]) # proportion of pre-symptomatic transmission  59.19007%
sim<-rgamma(100000,inf.par[1],inf.par[2])-inf.par[3]
quantile(sim,c(0.025,0.975))  #quantiles(0.025,0.975): -8.8 ~ 9.4 days

x=seq(0,40,by=0.01)
y<-data.frame(days=x-inf.par[3],p=dgamma(x,inf.par[1],inf.par[2]))
y[y$p==max(y$p),]$days   #peaking point: -1.8 days


#######period 1#############
inf.fit = optim(
  c(19, 2.2, 17.5), lli.fx, 
  x.lb = data1[, "x.lb"], 
  x.ub = data1[, "x.ub"],
  y    = data1[, "y"],
  wpar = c(w.par1, w.par2)
)

inf.par = inf.fit$par
#inf.par = c(21.139574,1.179925,17.715240)
pgamma(inf.par[3], inf.par[1], inf.par[2]) # proportion of pre-symptomatic transmission  50.83469%

#######period 2#############
inf.fit = optim(
  c(18, 2.2, 17.5), lli.fx, 
  x.lb = data2[, "x.lb"], 
  x.ub = data2[, "x.ub"],
  y    = data2[, "y"],
  wpar = c(w.par1, w.par2)
)

inf.par = inf.fit$par
#inf.par = c(17.608431, 1.088618, 18.807569)
pgamma(inf.par[3], inf.par[1], inf.par[2]) # proportion of pre-symptomatic transmission  76.65664%
