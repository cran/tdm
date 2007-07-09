# concentration of intermediate iv infusion
infcpr<-function(D,tin,tau,ts){
    infcpr<-(D*(1-exp(-(samplesStats("cl"))/(samplesStats("v"))*tin))/(tin*(samplesStats("cl"))*(1-exp(-(samplesStats("cl"))/(samplesStats("v"))*tau))))*exp(-(samplesStats("cl"))/(samplesStats("v"))*(ts-tin))
}

# concentration of oral tablet
pocpr<-function(ka,D,tau,ts){
     pocpr<-ka*D/((samplesStats("v_F"))*(ka-(samplesStats("cl_F"))/(samplesStats("v_F"))))*((1/(1-exp(-(samplesStats("cl_F"))/(samplesStats("v_F"))*tau)))*exp(-(samplesStats("cl_F"))/(samplesStats("v_F"))*ts)-(1/(1-exp(-ka*tau)))*exp(-ka*ts))
}

# concentration of Digoxin
Digcpr<-function(D,tau){
     Digcpr<-D*1000/((samplesStats("cl_F"))*tau)
}

# concentration of Lithium corbonate
Litcpr<-function(D,tau){
     Litcpr<-(D/36.9458)/((samplesStats("cl_F"))*tau)
}

# concentration of Lithium citrate
Litcitcpr<-function(D,tau){
     Litcitcpr<-(D/94)/((samplesStats("cl_F"))*tau)
}

# concentration of phenytoin
Phecpr<-function(D,tau){
     Phecpr<-(D*24/tau)*(samplesStats("Km"))/((samplesStats("Vmax"))-D*24/tau)    
}

# concentration of children valproate
ChiValcpr<-function(D,tau,ts,TBW){
     ChiValcpr<-1.9*D/((0.24*TBW)*(1.9-(samplesStats("cl"))/(0.24*TBW)))*((1/(1-exp(-(samplesStats("cl"))/(0.24*TBW)*tau)))*exp(-(samplesStats("cl"))/(0.24*TBW)*ts)-(1/(1-exp(-1.9*tau)))*exp(-1.9*ts))
}

# concentration of valproate
Valcpr<-function(ka,D,tau,ts){
     Valcpr<-ka*D/(11.5*(ka-(samplesStats("cl"))/11.5))*((1/(1-exp(-(samplesStats("cl"))/11.5*tau)))*exp(-(samplesStats("cl"))/11.5*ts)-(1/(1-exp(-ka*tau)))*exp(-ka*ts))
}

# concentration of indinavir
Indcpr<-function(ka,D,tau,ts){
     Indcpr<-ka*D/(65.7*(ka-(samplesStats("cl_F"))/65.7))*((1/(1-exp(-(samplesStats("cl_F"))/65.7*tau)))*exp(-(samplesStats("cl_F"))/65.7*ts)-(1/(1-exp(-ka*tau)))*exp(-ka*ts))
}

# concentration of Ritonavir
Ritcpr<-function(ka,D,tau,ts){
     Ritcpr<-ka*D/((samplesStats("v_F"))*(ka-(samplesStats("cl_F"))/(samplesStats("v_F"))))*((1/(1-exp(-(samplesStats("cl_F"))/(samplesStats("v_F"))*tau)))*exp(-(samplesStats("cl_F"))/(samplesStats("v_F"))*(ts-0.778))-(1/(1-exp(-ka*tau)))*exp(-ka*(ts-0.778)))
}
# concentration of cyclosporine
Cyccpr<-function(PTD,bw,D,tau,ts){
     Cyccpr<-(0.2+10*abs(PTD-7)/((PTD+10)*60))*D*1000*0.3/((4*bw)*(0.3-(samplesStats("cl")/(4*bw))))*((1/(1-exp(-(samplesStats("cl")/(4*bw))*tau)))*exp(-(samplesStats("cl"))/(4*bw)*ts)-(1/(1-exp(-0.3*tau)))*exp(-0.3*ts))
}

# concentration of Everolimus
Evecpr<-function(ka,D,tau,ts){
     Evecpr<-ka*D/((samplesStats("v_F"))*(ka-(samplesStats("cl_F"))/(samplesStats("v_F"))))*((1/(1-exp(-(samplesStats("cl_F"))/(samplesStats("v_F"))*tau)))*exp(-(samplesStats("cl_F"))/(samplesStats("v_F"))*ts)-(1/(1-exp(-ka*tau)))*exp(-ka*ts))*1000
}

# concentration of Tacrolimus
Taccpr<-function(ka,D,tau,ts){
     Taccpr<-ka*D/(314*(ka-(samplesStats("cl_F"))/314))*((1/(1-exp(-(samplesStats("cl_F"))/314*tau)))*exp(-(samplesStats("cl_F"))/314*ts)-(1/(1-exp(-ka*tau)))*exp(-ka*ts))*1000
}

# concentration of Enoxaparin
Enocpr<-function(D,tau){
     Enocpr<-D*exp(-(samplesStats("cl_F"))/(samplesStats("v_F"))*(log(0.34*(1-exp(-(samplesStats("cl_F"))/(samplesStats("v_F"))*tau))/((samplesStats("cl_F"))/(samplesStats("v_F"))*(1-exp(-0.34*tau)))))/(0.34-(samplesStats("cl_F"))/(samplesStats("v_F"))))/(samplesStats("v_F"))*(1-exp(-(samplesStats("cl_F"))/(samplesStats("v_F"))*tau))/1000
}

# concentration of imatinib mesylate
Imacpr<-function(n,D,tau,ts){
     Imacpr<-((D/(1.5*samplesStats("cl_F")))*(1-exp(-(samplesStats("cl_F"))/(samplesStats("v_F"))*1.5))*(exp(-(samplesStats("cl_F"))/(samplesStats("v_F"))*(ts-1.5)))*((exp(-(samplesStats("cl_F"))/(samplesStats("v_F"))*n*tau))-1))/((exp(-(samplesStats("cl_F"))/(samplesStats("v_F"))*tau))-1)    
}

# concentration Imatinib mesylate
Imasscpr<-function(D,tau){
     Imasscpr<-((D/(1.5*(samplesStats("cl_F"))))*(1-exp(-(samplesStats("cl_F"))/(samplesStats("v_F"))*1.5))*(exp(-(samplesStats("cl_F"))/(samplesStats("v_F"))*(tau-1.5))))/(1-(exp(-(samplesStats("cl_F"))/(samplesStats("v_F"))*tau)))
}

# concentration of Theophylline IR at ss
TheIRsscpr<-function(S,D,tau,ts){
     TheIRsscpr<-(S*1.85*D/(samplesStats("v_F")*(1.85-(samplesStats("cl_F")/samplesStats("v_F")))))*(exp(-(samplesStats("cl_F")/samplesStats("v_F"))*ts)/(1-exp(-(samplesStats("cl_F")/samplesStats("v_F"))*tau))-exp(-1.85*ts)/(1-exp(-1.85*tau)))
}

# concentration of Theophyllinr CR
TheCRcpr<-function(S,D,tau){
     TheCRcpr<-S*D/(samplesStats("cl_F")*tau)
}

# concentration of Theophylline iv infusion
Theinfusioncpr<-function(S,DL,R,T,tinf){
     Theinfusioncpr<-S*DL/(tinf*samplesStats("cl"))*(1-exp(-samplesStats("cl")/samplesStats("v")*tinf))*exp(-samplesStats("cl")/samplesStats("v")*(T))+S*R/samplesStats("cl")*(1-exp(-samplesStats("cl")/samplesStats("v")*(T)))
}

# concentration of Warfain       
Warcpr<-function(D,tau){
Warcpr<-(((1/((-samplesStats("m")*(samplesStats("cl_F")/samplesStats("v_F")))/(samplesStats("kc")^2)*(1-(samplesStats("kc")*tau/(1-exp(-samplesStats("kc")*tau))))-samplesStats("m")/samplesStats("kc")*log((D/samplesStats("v_F"))/(samplesStats("Cpmax")*(1-exp(-samplesStats("cl_F")/samplesStats("v_F")*tau)))))+3.36)/4.368)^(1/0.383))
}

# Dose equation of Warfarion
Wardpr<-function(INR,tau){
Wardpr<-(exp(((-samplesStats("m")*(samplesStats("cl_F")/samplesStats("v_F")))/(samplesStats("kc")^2)*(1-(samplesStats("kc")*tau/(1-exp(-samplesStats("kc")*tau))))-1/(4.368*(INR^0.383)-3.36))*(samplesStats("kc")/samplesStats("m"))))*samplesStats("v_F")*(samplesStats("Cpmax")*(1-exp(-samplesStats("cl_F")/samplesStats("v_F")*tau)))
}

