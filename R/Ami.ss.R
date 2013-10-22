Ami.ss<-function(a,b,c,d,e,f,g,h,i,j){
dataList= list(                                             # produce a JAGS data file and name it Phedata
c=a,     
tau=b,   
ts=c,    
tin=d,   
D=e,     
bw=f,    
Ht=g,    
Scr=h,   
Gender=i,
age=j    
)
params = c("cl" , "v")                                          # The parameter(s) to be monitored.
initsList = list(cl=(0.01+0.0024*(293-2.03*j)*(0.5175-0.01685*h)/h*(0.86+0.14*i))*16.5, ## here 16.5 is 'v' which is 0.25 l/kg bw*65 (= 16.5)
 v=0.25*(0.4*f+0.6*((0.73*g-59.42)*i+(0.65*g-50.74)*(1-i))))       # initialize priors; cannot use 'age', 'Gender', etc. here.  --YJ
adaptSteps = 500                                                   # Number of steps to "tune" the samplers.
burnInSteps = 6000                                                 # Number of steps to "burn-in" the samplers.
nChains = 3                                                        # Number of chains to run.
numSavedSteps=100000                                               # Total number of steps in chains to save.
thinSteps=1                                                        # Number of steps to "thin" (1=keep every step).
nIter = ceiling(( numSavedSteps * thinSteps ) / nChains )          # Steps per chain.
# Create, initialize, and adapt the model:
model.file <- system.file("PK", "AmiSSmodel.txt", package="tdm")   ### give the file location to find model file  -YJ
jagsModel = jags.model(model.file, data=dataList ,    
n.chains=nChains , n.adapt=adaptSteps, inits=initsList)            # inits=initsList ,
# Burn-in:
cat("Burning in the MCMC chain...\n")
update(jagsModel, n.iter=burnInSteps)
# The saved MCMC chain:
cat("Sampling final MCMC chain...\n")
codaSamples <- coda.samples(jagsModel, params, n.iter=nIter)
### codaSamples <- autojags(jagsModel, params, n.iter=nIter)              ### still not work!  figure out how. -YJ 
checkConvergence = TRUE
if (checkConvergence) {
  ### openGraph(width=7,height=7)   ### there is a openGraphSaveGraph.R file; not to use for now.
  show(summary(codaSamples))
  ### str(codaSamples)
  ### show(gelman.diag(codaSamples))
  ### effectiveChainLength = effectiveSize(codaSamples) 
  ### show(effectiveChainLength)
  dev.new()
  plot(codaSamples) 
  ### dev.new()
  ### autocorr.plot(codaSamples)
  ### dev.new()
  ### caterplot(codaSamples)
  ### dev.new()
  ### traplot(codaSamples)
  dev.new()
  gelman.plot(codaSamples)               ### from 'R2jags' package? work fine now.  -YJ
  # dev.new()
  # denplot(codaSamples)
  # dev.new()
  # traceplot(codaSamples)
  ### mcmcplot(codaSamples)              ### uhh... it outputs the plots as .html format. -YJ
}
###
### show prediction/calc Cp here
###
cat("\n")
cl.mat <- as.matrix(codaSamples[[1]])
v.mat <- as.matrix(codaSamples[[2]])
posterior.estimates <- rbind(cl.mat, v.mat)
vars <-t(as.matrix(posterior.estimates))
params.mean <- apply(vars, 1, mean)
### cl.mean <- as.matrix(params.mean[[1]])                   ### for testing purpose here! -YJ
### v.mean <- as.matrix(params.mean[[2]])
### show(Km.mean);show(Vmax.mean)                            ### Yes, it works now. --YJ
write.table(params.mean,file="params.csv",col.names=FALSE)
X <- read.table("params.csv",header=FALSE)
cl <- X[1,2]
vd <- X[2,2]
half_life<-log(2)/(cl/vd)
Params <- data.frame(Estimated_Parameters=c("Cl (L/hr)","Vd (L)", "Half-life (hr)"),value=c(cl,vd,half_life))
show(Params);cat("\n\n")
}
