ChiVal.ss<-function(a,b,c,d,e,f){
dataList= list(                                             # produce a JAGS data file and name it Phedata
c=a,  
tau=b,
ts=c, 
D=d,  
TBW=e,
CBZ=f 
)
params = c("cl_F","ka","vd_F")                              # The parameter(s) to be monitored.
initsList = list(cl_F=0.012*(e^0.715)*((d/e*24/b)^0.306)*(1+0.359*f),
                 ka=4., vd_F=0.24*e)                       # initialize prior here; not working if use 'bw' instead of 'd' here.  --YJ
adaptSteps = 500                                            # Number of steps to "tune" the samplers.
burnInSteps = 6000                                          # Number of steps to "burn-in" the samplers.
nChains = 3                                                 # Number of chains to run.
numSavedSteps=100000                                        # Total number of steps in chains to save.
thinSteps=1                                                 # Number of steps to "thin" (1=keep every step).
nIter = ceiling(( numSavedSteps * thinSteps ) / nChains )   # Steps per chain.
# Create, initialize, and adapt the model:
model.file <- system.file("PK", "ChiValSSmodel.txt", package="tdm")   ### give the file location to find model file  -YJ
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
ka.mat   <- as.matrix(codaSamples[[2]])
vd_F.mat <- as.matrix(codaSamples[[3]])
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