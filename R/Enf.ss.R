Enf.ss<-function(a,b,c,d,e,f){
dataList= list(
c=a,
tau=b,
ts=c,
D=d,
bw=e,
Gender=f
)
params = c("cl_F","v_F","ka")                                 # The parameter(s) to be monitored.
initsList = list(cl_F=(0.99+e/70*0.833)*(1-f*0.203),v_F=4.43,ka=0.113)  # initialize prior here; not working if use 'bw' instead of 'd' here.  --YJ
adaptSteps = 500                                            # Number of steps to "tune" the samplers.
burnInSteps = 6000                                          # Number of steps to "burn-in" the samplers.
nChains = 3                                                 # Number of chains to run.
numSavedSteps=100000                                        # Total number of steps in chains to save.
thinSteps=1                                                 # Number of steps to "thin" (1=keep every step).
nIter = ceiling(( numSavedSteps * thinSteps ) / nChains )   # Steps per chain.
# Create, initialize, and adapt the model:
model.file <- system.file("PK", "EnfSSmodel.txt", package="tdm")   ### give the file location to find model file  -YJ
jagsModel = jags.model(model.file, data=dataList ,    
n.chains=nChains , n.adapt=adaptSteps, inits=initsList)            # inits=initsList ,
# Burn-in:
cat("Burning in the MCMC chain...\n")
update(jagsModel, n.iter=burnInSteps)
# The saved MCMC chain:
cat("Sampling final MCMC chain...\n")
codaSamples <- coda.samples(jagsModel, params, n.iter=nIter)

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
v_F.mat  <- as.matrix(codaSamples[[3]])
posterior.estimates <- rbind(cl_F.mat)
vars <- t(as.matrix(posterior.estimates))
params.mean <- apply(vars, 1, mean)
### cat("\n Estimated PK Parameters:\n")
### cat("---------------------------\n")
### show(params.mean)
### cat("\n\n")
write.table(params.mean,file="params.csv",col.names=FALSE)
### cl_F.mean <- as.matrix(params.mean[[1]])               ### for testing purpose here! -YJ
### v_F.mean <- as.matrix(params.mean[[2]])
### half_life.mean <-log(2)/(cl_F.mean/vd_F.mean)          ### secondary PK param.  -YJ
### show(Km.mean);show(Vmax.mean)                          ### Yes, it works now. -YJ
X <- read.table("params.csv",header=FALSE)
cl_F <- X[1,2]
ka   <- X[2,2]
v_F  <- X[3,2]
half_life <- log(2)/(cl_F/v_F)
X <- data.frame(Estimated_Parameters=c("ka","Cl/F","Vd/F","Half-life"),value=c(X[2,2],X[1,2],X[3,2],half_life))
show(X);cat("\n\n")
}