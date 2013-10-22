Van.sm<-function(a,b,c,d,e,f,g,h,i,j){
dataList= list(                                             # produce a JAGS data file and name it Phedata
T=a,
c=c(b),  
ts=c(c), 
tau=d,   
tin=e,   
D=f,     
bw=g,    
Scr=h,   
Gender=i,
age=j    
)
###
params = c("cl" , "v")                                      # The parameter(s) to be monitored.
initsList = list(cl=1.08*(140-j)*g/(72*h)*(0.85^(1-i))*60/1000,
                 v=0.98*g)                                  # initialize prior here; not working if use 'bw' instead of 'd' here.  --YJ
adaptSteps = 500                                            # Number of steps to "tune" the samplers.
burnInSteps = 6000                                          # Number of steps to "burn-in" the samplers.
nChains = 3                                                 # Number of chains to run.
numSavedSteps=100000                                        # Total number of steps in chains to save.
thinSteps=1                                                 # Number of steps to "thin" (1=keep every step).
nIter = ceiling(( numSavedSteps * thinSteps ) / nChains )   # Steps per chain.
# Create, initialize, and adapt the model:
model.file <- system.file("PK", "VanSMmodel.txt", package="tdm")   ### give the file location to find model file  -YJ
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
### show prediction/calc Cp here
###
cat("\n")
cl.mat <- as.matrix(codaSamples[[1]])
v.mat  <- as.matrix(codaSamples[[2]])
posterior.estimates <- rbind(cl.mat, v.mat)
vars <-t(as.matrix(posterior.estimates))
params.mean <- apply(vars, 1, mean)
cat("\n Estimated PK Parameters:\n")
cat("---------------------------\n")
show(params.mean)
cat("\n\n")
write.table(params.mean,file="params.csv",col.names=FALSE)
### cl.mean <- as.matrix(params.mean[[1]])             ### for testing purpose here! -YJ
### v.mean  <- as.matrix(params.mean[[2]])
### show(Km.mean);show(Vmax.mean)                      ### Yes, it works now. --YJ
}