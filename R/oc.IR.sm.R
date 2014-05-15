oc.IR.sm<-function(a,b,c,d,e,f,g){
dataList= list(                                             # produce a JAGS data file and name it Phedata
T=a,
c=c(b),
ts=c(c),
D=d,
cl_F_init=e,
v_F_init=f,
ka_value=g
)
### IBW<-i*(50+(2.3*(f/2.4-60)))+(1-i)*(45+(2.3*(f/2.4-60))) 
params = c("cl_F","v_F")                                    # The parameter(s) to be monitored.
initsList = list(cl_F=e,v_F=f)  # initialize prior here; not working if use 'bw' instead of 'd' here.  --YJ
adaptSteps = 500                                            # Number of steps to "tune" the samplers.
burnInSteps = 6000                                          # Number of steps to "burn-in" the samplers.
nChains = 3                                                 # Number of chains to run.
numSavedSteps=100000                                        # Total number of steps in chains to save.
thinSteps=1                                                 # Number of steps to "thin" (1=keep every step).
nIter = ceiling(( numSavedSteps * thinSteps ) / nChains )   # Steps per chain.
# Create, initialize, and adapt the model:
model.file <- system.file("PK", "oc.IR.SMmodel.txt", package="tdm")   ### give the file location to find model file  -YJ
jagsModel = jags.model(model.file, data=dataList ,    
n.chains=nChains , n.adapt=adaptSteps, inits=initsList)            # inits=initsList ,
# Burn-in:
cat("Burning in the MCMC chain...\n")
update(jagsModel, n.iter=burnInSteps, progress.bar="gui")
# The saved MCMC chain:
cat("Sampling final MCMC chain...\n")
codaSamples <- coda.samples(jagsModel, params, n.iter=nIter)
### dicSamples<-dic.samples(jagsModel, n.iter=nIter,thin=1, type="pD")

checkConvergence = TRUE
if (checkConvergence) {
  show(summary(codaSamples))
  ### show(summary(dicSamples))
  dev.new()
  plot(codaSamples) 
  dev.new()
  gelman.plot(codaSamples)
}
###
### show prediction/calc Cp obtained from JAGS here
###
cat("\n")
cl_F.mat <- as.matrix(codaSamples[[1]])
v_F.mat  <- as.matrix(codaSamples[[2]])
posterior.estimates <- rbind(cl_F.mat,v_F.mat)
vars <- t(as.matrix(posterior.estimates))
params.mean <- apply(vars, 1, mean)
write.table(params.mean,file="params.csv",col.names=FALSE)
X <- read.table("params.csv",header=FALSE)
cl_F <<- X[1,2]
v_F  <<- X[2,2] 
cl_F<-cl_F
v_F<-v_F
k<<-cl_F/v_F
k<-k
unlink("params.csv")
### k    <- cl_F/vd_F
half_life <- log(2)/(cl_F/v_F)
X <- data.frame(Estimated_Parameters=c("Half-life (hr)","kel(/hr)","Cl/F (L/hr)","Vd/F (L)"),value=c(half_life,k,cl_F,v_F))
show(X);cat("\n\n")
}
