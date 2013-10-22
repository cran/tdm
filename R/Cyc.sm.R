Cyc.sm<-function(a,b,c,d,e,f,g,h){
dataList= list(                                             # produce a JAGS data file and name it Phedata
T=a,    
c=c(b), 
ts=c(c),
PTD=d,  
tau=e,  
D=f,    
bw=g,   
Dia=h   
)
### *********************************************************************************************
params = c("cl")                                            # The parameter(s) to be monitored.
initsList = list(cl=0.281*g*(0.629*h+1-h))
                                                            # initialize prior here; not working if use 'bw' instead of 'd' here.  --YJ
adaptSteps = 500                                            # Number of steps to "tune" the samplers.
burnInSteps = 6000                                          # Number of steps to "burn-in" the samplers.
nChains = 3                                                 # Number of chains to run.
numSavedSteps=100000                                        # Total number of steps in chains to save.
thinSteps=1                                                 # Number of steps to "thin" (1=keep every step).
nIter = ceiling(( numSavedSteps * thinSteps ) / nChains )   # Steps per chain.
# Create, initialize, and adapt the model:
model.file <- system.file("PK", "CycSMmodel.txt", package="tdm")   ### give the file location to find model file  -YJ
jagsModel = jags.model(model.file, data=dataList,    
n.chains=nChains, n.adapt=adaptSteps, inits=initsList)            # inits=initsList ,
# Burn-in:
cat("Burning in the MCMC chain...\n")
update(jagsModel, n.iter=burnInSteps)
# The saved MCMC chain:
cat("Sampling final MCMC chain...\n")
codaSamples <- coda.samples(jagsModel, params, n.iter=nIter)
### codaSamples <- autojags(jagsModel, params, n.iter=nIter)              ### still not work!  figure out how. -YJ 

checkConvergence = TRUE
if (checkConvergence) {
  show(summary(codaSamples))
  dev.new()
  plot(codaSamples) 
  dev.new()
  gelman.plot(codaSamples)             ### why is this line not working?  -YJ
}
###
### show prediction/calc Cp obtained from JAGS here
###
cat("\n")
cl.mat <- as.matrix(codaSamples[[1]])
posterior.estimates <- rbind(cl.mat)
vars <- t(as.matrix(posterior.estimates))
params.mean <- apply(vars, 1, mean)
### cat("\n Estimated PK Parameters:\n")
### cat("---------------------------\n")
### show(params.mean)
### cat("\n\n")
write.table(params.mean,file="params.csv",col.names=FALSE)
### cl_F.mean <- as.matrix(params.mean[[1]])               ### for testing purpose here! -YJ
### vd_F.mean <- as.matrix(params.mean[[2]])
### ka.mean   <- as.matrix(params.mean[[3]])
### half_life.mean <-log(2)/(cl_F.mean/vd_F.mean)          ### secondary PK param. -YJ
### show(Km.mean);show(Vmax.mean)                          ### Yes, it works now.  -YJ
X <- read.table("params.csv",header=FALSE)
cl <- X[1,2]
vd <- 4.*g    ## assumed vd = 4.0 L/kg of bw here and in the model. --YJ
half_life<-log(2)/(cl/vd)
Params <- data.frame(Estimated_Parameters=c("cl (L/hr)","Vd (L)", "Half-life (hr)"),value=c(cl,vd,half_life))
cat("\n\n");show(Params);cat("\n\n")
}