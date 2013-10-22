Amianhinfusion.sm<-function(a,b,c,g,h,i,j,k,L,m,n){
dataList= list(                                             # produce a JAGS data file and name it Phedata
T=a,
c=c(b),
ts=c(c),
tinf=n,
R=m,
DL=L,
ht=g,      
age=h,     
smoke=i,
Gender=j,
CHF=k
)
IBW<-j*(50+(2.3*(g/2.4-60)))+(1-j)*(45+(2.3*(g/2.4-60))) 
params = c("cl","v")                                        # The parameter(s) to be monitored.
initsList = list(cl=(0.037*IBW-0.006*h)*(1.284^i)*(0.751^k),v=0.42*IBW)  # initialize prior here; not working if use 'bw' instead of 'd' here.  --YJ
adaptSteps = 500                                            # Number of steps to "tune" the samplers.
burnInSteps = 6000                                          # Number of steps to "burn-in" the samplers.
nChains = 3                                                 # Number of chains to run.
numSavedSteps=100000                                        # Total number of steps in chains to save.
thinSteps=1                                                 # Number of steps to "thin" (1=keep every step).
nIter = ceiling(( numSavedSteps * thinSteps ) / nChains )   # Steps per chain.
# Create, initialize, and adapt the model:
model.file <- system.file("PK", "AmianhinfusionSMmodel.txt", package="tdm")   ### give the file location to find model file  -YJ
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
cl.mat <- as.matrix(codaSamples[[1]])
v.mat  <- as.matrix(codaSamples[[2]])
posterior.estimates <- rbind(cl.mat,v.mat)
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
cl <- X[1,2]
vd <- X[2,2] 
half_life <- log(2)/(cl/vd)
X <- data.frame(Estimated_Parameters=c("Cl/F","Vd/F","Half-life"),value=c(X[1,2],X[2,2],half_life))
show(X);cat("\n\n")
}

