Car.sm<-function(a,b,c,d,e,f,g,h,i,j){
dataList= list(                                             # produce a JAGS data file and name it Phedata
T=a,  
c=c(b), 
ts=c(c),
tau=d,  
D=e,    
TBW=f,  
PB=g,   
VPA=h,  
PHT=i,  
E=j     
)
params = c("cl_F","v_F")                                  # The parameter(s) to be monitored.
initsList = list(cl_F= 0.0734*((e/f/d*24)^0.406)*(f^0.694)*(1.45^i)*(1.17^g)*(1.21^h)*(0.849^j),
                  v_F=1.91*f)                               # initialize prior here; not working if use 'bw' instead of 'd' here.  --YJ
adaptSteps = 500                                            # Number of steps to "tune" the samplers.
burnInSteps = 6000                                          # Number of steps to "burn-in" the samplers.
nChains = 3                                                 # Number of chains to run.
numSavedSteps=100000                                        # Total number of steps in chains to save.
thinSteps=1                                                 # Number of steps to "thin" (1=keep every step).
nIter = ceiling(( numSavedSteps * thinSteps ) / nChains )   # Steps per chain.
# Create, initialize, and adapt the model:
model.file <- system.file("PK", "CBZSMmodel.txt", package="tdm")   ### give the file location to find model file  -YJ
jagsModel = jags.model(model.file, data=dataList ,    
n.chains=nChains , n.adapt=adaptSteps, inits=initsList)            # inits=initsList ,
# Burn-in:
cat("Burning in the MCMC chain...\n")
update(jagsModel, n.iter=burnInSteps)
# The saved MCMC chain:
cat("Sampling final MCMC chain...\n")
codaSamples <- coda.samples(jagsModel, params, n.iter=nIter)
### codaSamples <- autojags(jagsModel, params, n.iter=nIter)              ### still not work!  figure out how. -YJ 

show(summary(codaSamples))
dev.new()
plot(codaSamples) 
dev.new()
gelman.plot(codaSamples)             ### why is this line not working?  -YJ
cat("\n")
cl_F.mat <- as.matrix(codaSamples[[1]])
v_F.mat <- as.matrix(codaSamples[[2]])
posterior.estimates <- rbind(cl_F.mat, v_F.mat)
vars <-t(as.matrix(posterior.estimates))
params.mean <- apply(vars, 1, mean)
cat("\n Estimated PK Parameters:\n")
cat("---------------------------\n")
show(params.mean)
cat("\n\n")
write.table(params.mean,file="params.csv",col.names=FALSE)
### cl_F.mean <- as.matrix(params.mean[[1]])           ### for testing purpose here! -YJ
### v_F.mean <- as.matrix(params.mean[[2]])
### show(Km.mean);show(Vmax.mean)                      ### Yes, it works now. --YJ
X <- read.table("params.csv",header=FALSE)
cl <- X[1,2]
vd <- X[2,2]
half_life<-log(2)/(cl/vd)
Params <- data.frame(Estimated_Parameters=c("Cl_F (L/hr)","Vd_F (L)", "Half-life (hr)"),value=c(cl,vd,half_life))
cat("\n\n");show(Params);cat("\n\n")
}
