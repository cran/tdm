# concentration calculation of oral opiods
OOsscpr<-function(D,tau,ts){
### X <- read.table("params.csv",header=FALSE)
### cl_F <- X[1,2]
### v_F  <- X[2,2]
cl_F<-cl_F
v_F <-v_F
ka  <-ka
k<-cl_F/v_F

OOsscpr<-ka*D/(v_F*(ka-k))*((1/(1-exp(-k*tau)))*
            exp(-k*ts)-(1/(1-exp(-ka*tau)))*exp(-ka*ts))     ### for conc. at time = ts at steady-state
}

OOsdcpr<-function(D,ts){
### X <- read.table("params.csv",header=FALSE)
### cl_F <- X[1,2]
### v_F  <- X[2,2]
cl_F<-cl_F
v_F <-v_F
ka  <-ka
k<-cl_F/v_F

OOsdcpr<-(ka*D)/(v_F*(ka-k))*(exp(-k*ts)-exp(-ka*ts))  ### for conc. at time = ts after the 1st-dose
}
