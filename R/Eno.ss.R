Eno.ss<-function(a,b,c,d,e,f){                         
dataList= list(                                             # produce a JAGS data file and name it Phedata
Amax=a,  
tau=b,   
D=c,     
TBW=d,   
Gender=e,
Scr=f    
)
params = c("cl_F","v_F")                                   # The parameter(s) to be monitored.
initsList = list(cl_F= 0.74*((d/f/0.79)^0.24)*(0.75^(1-e)),
                 v_F=5.29*((d/73)^1.49))                    # initialize prior here; not working if use 'bw' instead of 'd' here.  --YJ
adaptSteps = 500                                            # Number of steps to "tune" the samplers.
burnInSteps = 6000                                          # Number of steps to "burn-in" the samplers.
nChains = 3                                                 # Number of chains to run.
numSavedSteps=100000                                        # Total number of steps in chains to save.
thinSteps=1                                                 # Number of steps to "thin" (1=keep every step).
nIter = ceiling(( numSavedSteps * thinSteps ) / nChains )   # Steps per chain.
# Create, initialize, and adapt the model:
model.file <- system.file("PK", "EnoSSmodel.txt", package="tdm")   ### give the file location to find model file  -YJ
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
### show prediction/calc Cp here
###
cat("\n")
cl_F.mat <- as.matrix(codaSamples[[1]])
v_F.mat  <- as.matrix(codaSamples[[2]])
posterior.estimates <- rbind(cl_F.mat, v_F.mat)
vars <-t(as.matrix(posterior.estimates))
params.mean <- apply(vars, 1, mean)
write.table(params.mean,file="params.csv",col.names=FALSE)
### cl_F.mean <- as.matrix(params.mean[[1]])               ### for testing purpose here! -YJ
### v_F.mean  <- as.matrix(params.mean[[2]])
### show(Km.mean);show(Vmax.mean)                      ### Yes, it works now. --YJ
X <- read.table("params.csv",header=FALSE)
cl <- X[1,2]
vd <- X[2,2]
half_life<-log(2)/(cl/vd)
Params <- data.frame(Estimated_Parameters=c("Cl_F (L/hr)","Vd_F (L)", "Half-life (hr)"),value=c(cl,vd,half_life))
show(Params);cat("\n\n")
}
