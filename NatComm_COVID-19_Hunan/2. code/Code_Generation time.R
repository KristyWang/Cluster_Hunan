require(readxl)

#set your working directory
setwd("...\\NatComm_COVID-19_Hunan\\")   


#data used for main analysis
# data: (y.lb, y.ub): lower and upper bounds of exposure duration
data<-data.frame(read_excel("1. input\\data_Table S3.xlsx",sheet = "Main analysis")) #n=268 
colnames(data)<-c("y.lb","y.ub")
max(data[,"y.lb"])

#--- functions ---

#--- CDF of generation time ---
p.Z  = function(y, gpar1, gpar2) {
  
  #--- infectiousness, gamma distribution ---
  # gpar[1:2]: hyper-parameters (gamma)
  # x        : infection time of infectee w.r.t onset time of infector
  f.Xc = function(x, gpar1) { dgamma(x, gpar1[1], gpar1[2]) }
  
  #--- generation time
  f.Ge = function(x, gpar2) { dgamma(x, gpar2[1], gpar2[2]) }
  
  #--- convolution between serial interval and generation time ---
  # y      : length of incubation period
  f.Z = function(y, gpar1, gpar2) {
    integrate(
      f = function(x, y, gpar1, gpar2) {
        f.Xc((x-y+gpar1[3]), gpar1)*f.Ge(x, gpar2)},  
      lower = -Inf, 
      upper = Inf,
      y   = y,
      gpar1  = gpar1,
      gpar2  = gpar2,
      rel.tol = 1.5e-12,
      subdivisions=3000
    )$value
  } 
  f.Z2 = Vectorize(f.Z, vectorize.args = "y")
  
  
  #--- p.Z ---
  integrate(
    f = function(x, gpar1, gpar2) { f.Z2(x, gpar1, gpar2) },
    lower = 0,
    upper = y,
    gpar1  = gpar1,
    gpar2  = gpar2,
    subdivisions=3000
  )$value
}
p.Z2 = Vectorize(p.Z, vectorize.args = c("y"))


#--- logLikelihood for the observed incubation periods ---
# x.lb: lower bound of infectors exposure dates
# x.ub: upper bound of infectors exposure dates
# 0.5 : continuity correction
lli.fx = function(y.lb, y.ub, gpar1, gpar2) {
  Z.lb = p.Z2(pmax((y.lb-0.5),0.00001), gpar1, gpar2)
  Z.ub = p.Z2((y.ub+0.5), gpar1, gpar2)
  lli = pmax(Z.ub - Z.lb,0)
  log_lli = log(lli)
  return(-sum(log_lli))
}

#--- infectiousness ---
# from personal estimations ---

g.par1 = 18.1896828 #shape 
g.par2 = 0.9095662 #rate 
g.par3 = 20.7310048 # shift


inf.fit = optim(
  c(4.5, 0.85), lli.fx, 
  y.lb = as.double(data[, "y.lb"]), 
  y.ub = as.double(data[, "y.ub"]),
  gpar1 = c(g.par1, g.par2, g.par3)#,
)

# inf.fit$par would give the estimated parameters for the generation time
inf.fit$par

#--- results from fit ---

inf.par = inf.fit$par
#inf.par = c(10.555266,1.845717)

set.seed(20201007)
simtg = rgamma(100000, inf.par[1], inf.par[2])    
inf.par[1] #shape:10.55527  
inf.par[2] #rate:1.845717
inf.par[1]/inf.par[2]                             # mean: 5.7
sqrt(inf.par[1]/inf.par[2]^2)                     #sd: 1.8
quantile(simtg, c(0.025,0.25,0.5,0.75,0.975))    # quantile: 2.809461 4.455253 5.539712 6.798732 9.639950 

