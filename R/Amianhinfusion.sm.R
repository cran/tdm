Amianhinfusion.sm<-function(a,b,c,g,h,i,j,k,L,m,n){
### library(BRugs)                                        # active BRugs
library(R2jags)
### oldwd<-getwd()
### setwd(system.file("PK",package="tdm"))                # set working directory
### modelCheck("AmianhinfusionSMmodel.txt")               # Load model
### bugsData(                                             # porduce a BUGS data file and name it Amianhinfusiondata
### list(T=a,
### c=c(b),
### ts=c(c),
### tinf=n,
### R=m,
### DL=L,
### ht=g,
### age=h,
### smoke=i,
### Gender=j,
### CHF=k
### )
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
### IBW<-Gender*(50+(2.3*(ht/2.54-60)))+(1-Gender)*(45+(2.3*(ht/2.54-60)))
IBW<-j*(50+(2.3*(g/2.4-60)))+(1-j)*(45+(2.3*(g/2.4-60))) 
### , fileName=file.path(getwd(),"Amianhinfusiondata.txt"),digits=5)
### modelData("Amianhinfusiondata.txt")                   # Load data
### modelCompile(numChains=1)                             # compile
### modelGenInits()                                       # gen intis
### modelUpdate(10000)                                     # burn in 4000
### samplesSet(c("cl"))                                   # set monitored PK parameters
### samplesSet(c("v"))                                    # set monitored PK parameters
### modelUpdate(10000)                                    # update 10000
params = c("cl","v")                                        # The parameter(s) to be monitored.
### theta1<-(0.037*IBW-0.006*age)*pow(1.284,smoke)*pow(0.751,CHF)
### theta2<-(0.42*IBW)                                                         # population estimate of Aminophylline anhydrous volume of distribution
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

# resulting codaSamples object has these indices:
# codaSamples[[ chainIdx ]][ stepIdx , paramIdx ]
#------------------------------------------------------------------------------
# EXAMINE THE RESULTS
# Convert coda-object codaSamples to matrix object for easier handling.
# But note that this concatenates the different chains into one long chain.
# Result is mcmcChain[ stepIdx , paramIdx ]

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
  gelman.plot(codaSamples)             ### why is this line not working?  -YJ
  # dev.new()
  # denplot(codaSamples)
  # dev.new()
  # traceplot(codaSamples)
  ### mcmcplot(codaSamples)              ### uhh... it outputs the plots as .html format. -YJ
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

