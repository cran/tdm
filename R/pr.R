# concentration of intermediate iv infusion - for Ami.ss() & Van.ss()
infcpr<-function(D,tin,tau,ts){
    X <- read.table("params.csv",header=FALSE)
    cl <- X[1,2]
    v  <- X[2,2]
    infcpr<-(D*(1-exp(-cl/v*tin))/(tin*cl*(1-exp(-cl/v*tau))))*exp(-cl/v*(ts-tin))
}

# concentration of oral tablet of CBZ!  --YJ
pocpr<-function(ka,D,tau,ts){
X <- read.table("params.csv",header=FALSE)
cl_F <- X[1,2]
v_F  <- X[2,2]
pocpr<-ka*D/((v_F)*(ka-(cl_F)/(v_F)))*((1/(1-exp(-(cl_F)/(v_F)*tau)))*
            exp(-(cl_F)/(v_F)*ts)-(1/(1-exp(-ka*tau)))*exp(-ka*ts))
}

enfcpr<-function(D,tau,ts){     # for Enf (T20)  -YJ
X <- read.table("params.csv",header=FALSE)
cl_F <- X[1,2]
ka   <- X[2,2]
v_F  <- X[3,2]
pocpr<-ka*D/((v_F)*(ka-(cl_F)/(v_F)))*((1/(1-exp(-(cl_F)/(v_F)*tau)))*
            exp(-(cl_F)/(v_F)*ts)-(1/(1-exp(-ka*tau)))*exp(-ka*ts))
}
# concentration of Digoxin
Digcpr<-function(D,tau){
X <- read.table("params.csv",header=FALSE)
cl_F <- X[1,2]     ### where cl = Cl/F for oral digoxin -YJ
     Digcpr<-D*1000/(cl_F*tau)
}

# concentration of Lithium corbonate
Litcpr<-function(D,tau){
X <- read.table("params.csv",header=FALSE)
cl_F <- X[1,2]
     Litcpr<-(D/36.9458)/((cl_F)*tau)
}

# concentration of Lithium citrate
Litcitcpr<-function(D,tau){
X <- read.table("params.csv",header=FALSE)
cl_F <- X[1,2]
     Litcitcpr<-(D/94)/((cl_F)*tau)
}

# concentration of phenytoin
Phecpr<-function(D,tau){
    X <- read.table("params.csv",header=FALSE)
    km   <- X[1,2]
    vmax <- X[2,2]     
    ### Phecpr<-(D*24/tau)*(samplesStats("Km"))/((samplesStats("Vmax"))-D*24/tau)   ### for BUGS
    Phecpr<-(D*24/tau)*km/(vmax-D*24/tau)         ### for JAGS. -YJ
}

# concentration of children valproate
ChiValcpr<-function(D,tau,ts){
X <- read.table("params.csv",header=FALSE)
cl_F <- X[1,2]
vd_F <- X[3,2] 
ka   <- X[2,2] 
ChiValcpr<-ka*D/(vd_F*(ka-(cl_F/vd_F)))*((1/(1-exp(-(cl_F/vd_F)*tau)))*exp(-(cl_F/vd_F)*ts)-(1/(1-exp(-ka*tau)))*exp(-ka*ts))
}

# concentration of valproate
Valcpr<-function(D,tau,ts){
X <- read.table("params.csv",header=FALSE)
cl_F <- X[1,2]
vd_F <- X[3,2] 
ka   <- X[2,2] 
Valcpr<-ka*D/(vd_F*(ka-(cl_F/vd_F)))*((1/(1-exp(-(cl_F/vd_F)*tau)))*exp(-(cl_F/vd_F)*ts)-(1/(1-exp(-ka*tau)))*exp(-ka*ts))
}

# concentration of indinavir
Indcpr<-function(D,tau,ts){
X <- read.table("params.csv",header=FALSE)
cl_F <- X[1,2]
ka   <- X[2,2] 
     Indcpr<-ka*D/(65.7*(ka-(cl_F)/65.7))*((1/(1-exp(-(cl_F)/65.7*tau)))*exp(-(cl_F)/65.7*ts)-(1/(1-exp(-ka*tau)))*exp(-ka*ts))
}

# concentration of Ritonavir
Ritcpr<-function(D,tau,ts){
X <- read.table("params.csv",header=FALSE)
cl_F <- X[1,2]
ka   <- X[2,2]
v_F  <- X[3,2]
     Ritcpr<-ka*D/((v_F)*(ka-(cl_F)/(v_F)))*((1/(1-exp(-(cl_F)/(v_F)*tau)))*exp(-(cl_F)/(v_F)*(ts-0.778))-(1/(1-exp(-ka*tau)))*exp(-ka*(ts-0.778)))
}
# concentration of cyclosporine
Cyccpr<-function(PTD,bw,D,tau,ts){
X <- read.table("params.csv",header=FALSE)
cl <- X[1,2]
Cyccpr<-(0.2+10*abs(PTD-7)/((PTD+10)*60))*D*1000*0.3/((4*bw)*(0.3-(cl/(4*bw))))*((1/(1-exp(-(cl/(4*bw))*tau)))*exp(-(cl)/(4*bw)*ts)-(1/(1-exp(-0.3*tau)))*exp(-0.3*ts))
}

# concentration of Everolimus
Evecpr<-function(ka,D,tau,ts){
X <- read.table("params.csv",header=FALSE)
cl_F <- X[1,2]
v_F  <- X[2,2]
Evecpr<-ka*D/((v_F)*(ka-(cl_F)/(v_F)))*((1/(1-exp(-(cl_F)/(v_F)*tau)))*exp(-(cl_F)/(v_F)*ts)-(1/(1-exp(-ka*tau)))*exp(-ka*ts))*1000
}

# concentration of Tacrolimus
Taccpr<-function(ka,D,tau,ts){
X <- read.table("params.csv",header=FALSE)
cl_F <- X[1,2]
Taccpr<-ka*D/(314*(ka-(cl_F)/314))*((1/(1-exp(-(cl_F)/314*tau)))*exp(-(cl_F)/314*ts)-(1/(1-exp(-ka*tau)))*exp(-ka*ts))*1000
}

# concentration of Enoxaparin
Enocpr<-function(D,tau){
X <- read.table("params.csv",header=FALSE)
cl_F <- X[1,2]
v_F  <- X[2,2]
Enocpr<-D*exp(-(cl_F)/(v_F)*(log(0.34*(1-exp(-(cl_F)/(v_F)*tau))/((cl_F)/(v_F)*(1-exp(-0.34*tau)))))/(0.34-(cl_F)/(v_F)))/(v_F)*(1-exp(-(cl_F)/(v_F)*tau))/1000
}

# concentration of imatinib mesylate
Imacpr<-function(n,D,tau,ts){
X <- read.table("params.csv",header=FALSE)
cl_F <- X[1,2]
v_F  <- X[2,2] 
     Imacpr<-((D/(1.5*cl_F))*(1-exp(-(cl_F)/(v_F)*1.5))*(exp(-(cl_F)/(v_F)*(ts-1.5)))*((exp(-(cl_F)/(v_F)*n*tau))-1))/
             ((exp(-(cl_F)/(v_F)*tau))-1)
}

# concentration Imatinib mesylate
Imasscpr<-function(D,tau){
X <- read.table("params.csv",header=FALSE)
cl_F <- X[1,2]
v_F  <- X[2,2]
     Imasscpr<-((D/(1.5*(cl_F)))*(1-exp(-(cl_F)/(v_F)*1.5))*(exp(-(cl_F)/(v_F)*(tau-1.5))))/(1-(exp(-(cl_F)/(v_F)*tau)))
}

# concentration of for all Theophylline IR dosage forms at ss
TheIRsscpr<-function(S,D,tau,ts){
X <- read.table("params.csv",header=FALSE)
cl_F <- X[1,2]
v_F  <- X[2,2]
     TheIRsscpr<-(S*1.85*D/(v_F*(1.85-(cl_F/v_F))))*(exp(-(cl_F/v_F)*ts)/(1-exp(-(cl_F/v_F)*tau))-exp(-1.85*ts)/(1-exp(-1.85*tau)))
}

# concentration of for all Theophyllinr CR dosage forms
TheCRcpr<-function(S,D,tau){
X <- read.table("params.csv",header=FALSE)
cl_F <- X[1,2]
     TheCRcpr<-S*D/(cl_F*tau)
}

# concentration of Theophylline iv infusion
Theinfusioncpr<-function(S,DL,R,T,tinf){
X <- read.table("params.csv",header=FALSE)
cl <- X[1,2]  ### IV infusion; no F term
v  <- X[2,2]  ### IV infusion; no F term
     Theinfusioncpr<-S*DL/(tinf*cl)*(1-exp(-cl/v*tinf))*exp(-cl/v*(T))+S*R/cl*(1-exp(-cl/v*(T)))
}

### # concentration of Warfain       
### Warcpr<-function(D,tau){
### Warcpr<-(((1/((-m*(cl_F/v_F))/(kc^2)*(1-(kc*tau/(1-exp(-kc*tau))))-m/kc*log((D/v_F)/(Cpmax*(1-exp(-cl_F/v_F*tau)))))+3.36)/4.368)^(1/0.383))
### }
### 
### # Dose equation of Warfarion
### Wardpr<-function(INR,tau){
### 
###     X <- read.table("params.csv",header=FALSE)
###     m     <- X[1,2]
###     Cpmax <- X[2,2]
###     kc    <- X[3,2]
###     cl_F  <- X[4,2]
###     v_F   <- X[5,2]
### 
###     Wardpr<-(exp(((-m*(cl_F/v_F))/(kc^2)*(1-(kc*tau/(1-exp(-kc*tau))))-1/(4.368*(INR^0.383)-3.36))*
###             (kc/m)))*v_F*(Cpmax*(1-exp(-cl_F/v_F*tau)))
### }

