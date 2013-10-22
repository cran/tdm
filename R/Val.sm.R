Val.sm<-function(a,b,c,d,e,f,g,h){
dataList=list(
T=a,      ### 'T' is # of samples as input. see ValSMmodel.txt.
c=c(b),   
ts=c(c),  
tau=d,    
D=e,      
age=f,    
CBZ=g,    
INDI=h
)         
params = c("cl_F","ka","vd_F")                              # The parameter(s) to be monitored.
initsList = list(cl_F=0.105+0.151*g+0.000248*e/d*24+0.0968*f/20+0.0803*h,
                 ka=1.9, vd_F=6.)                         # initialize prior here; not working if use 'bw' instead of 'd' here.  --YJ
adaptSteps = 500                                            # Number of steps to "tune" the samplers.
burnInSteps = 6000                                          # Number of steps to "burn-in" the samplers.
nChains = 3                                                 # Number of chains to run.
numSavedSteps=100000                                        # Total number of steps in chains to save.
thinSteps=1                                                 # Number of steps to "thin" (1=keep every step).
nIter = ceiling(( numSavedSteps * thinSteps ) / nChains )   # Steps per chain.
# Create, initialize, and adapt the model:
model.file <- system.file("PK", "ValSMmodel.txt", package="tdm")   ### give the file location to find model file  -YJ
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
cl_F.mat <- as.matrix(codaSamples[[1]])
ka.mat  <- as.matrix(codaSamples[[2]])
vd_F.mat   <- as.matrix(codaSamples[[3]])
posterior.estimates <- rbind(cl_F.mat,ka.mat,vd_F.mat)
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
### half_life.mean <-log(2)/(cl_F.mean/vd_F.mean)          ### secondary PK param.  -YJ
### show(Km.mean);show(Vmax.mean)                          ### Yes, it works now. -YJ
X <- read.table("params.csv",header=FALSE)
cl_F <- X[1,2]
vd_F <- X[3,2] 
ka   <- X[2,2] 
half_life <- log(2)/(cl_F/vd_F)
X <- data.frame(Estimated_Parameters=c("cl_F","vd_F","ka","Half-life"),value=c(X[1,2],X[3,2],X[2,2],half_life))
cat("\n\n");show(X);cat("\n\n")
}



