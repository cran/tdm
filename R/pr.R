infcpr<-function(D,tin,tau,ts){
    infcpr<-(D*(1-exp(-(samplesStats("cl_F"))/(samplesStats("v_F"))*tin))/(tin*(samplesStats("cl_F"))*(1-exp(-(samplesStats("cl_F"))/(samplesStats("v_F"))*tau))))*exp(-(samplesStats("cl_F"))/(samplesStats("v_F"))*(ts-tin))
}


pocpr<-function(ka,D,tau,ts){
     pocpr<-ka*D/((samplesStats("v_F"))*(ka-(samplesStats("cl_F"))/(samplesStats("v_F"))))*((1/(1-exp(-(samplesStats("cl_F"))/(samplesStats("v_F"))*tau)))*exp(-(samplesStats("cl_F"))/(samplesStats("v_F"))*ts)-(1/(1-exp(-ka*tau)))*exp(-ka*ts))
}


Digcpr<-function(D,tau){
     Digcpr<-D*1000/((samplesStats("cl_F"))*tau)
}

PedDigcpr<-function(bw,D,tau){
     Digcpr<-D/(samplesStats("cl_F")*bw*tau)*1000
}


Litcpr<-function(D,tau){
     Litcpr<-(D/36.9458)/((samplesStats("cl_F"))*tau)
}


Litcitcpr<-function(D,tau){
     Litcitcpr<-(D/94)/((samplesStats("cl_F"))*tau)
}


Phecpr<-function(D,tau){
     Phecpr<-(D*24/tau)*(samplesStats("Km"))/((samplesStats("Vmax"))-D*24/tau)    
}


ChiValcpr<-function(D,tau,ts,TBW){
     ChiValcpr<-1.9*D/((0.24*TBW)*(1.9-(samplesStats("cl_F"))/(0.24*TBW)))*((1/(1-exp(-(samplesStats("cl_F"))/(0.24*TBW)*tau)))*exp(-(samplesStats("cl_F"))/(0.24*TBW)*ts)-(1/(1-exp(-1.9*tau)))*exp(-1.9*ts))
}

Valcpr<-function(ka,D,tau,ts){
     Valcpr<-ka*D/(11.5*(ka-(samplesStats("cl_F"))/11.5))*((1/(1-exp(-(samplesStats("cl_F"))/11.5*tau)))*exp(-(samplesStats("cl_F"))/11.5*ts)-(1/(1-exp(-ka*tau)))*exp(-ka*ts))
}


Indcpr<-function(ka,D,tau,ts){
     Indcpr<-ka*D/(65.7*(ka-(samplesStats("cl_F"))/65.7))*((1/(1-exp(-(samplesStats("cl_F"))/65.7*tau)))*exp(-(samplesStats("cl_F"))/65.7*ts)-(1/(1-exp(-ka*tau)))*exp(-ka*ts))
}


Ritcpr<-function(ka,D,tau,ts){
     Ritcpr<-ka*D/((samplesStats("v_F"))*(ka-(samplesStats("cl_F"))/(samplesStats("v_F"))))*((1/(1-exp(-(samplesStats("cl_F"))/(samplesStats("v_F"))*tau)))*exp(-(samplesStats("cl_F"))/(samplesStats("v_F"))*(ts-0.778))-(1/(1-exp(-ka*tau)))*exp(-ka*(ts-0.778)))
}


Cyccpr<-function(PTD,bw,D,tau,ts){
     Cyccpr<-(0.2+10*abs(PTD-7)/((PTD+10)*60))*D*1000*0.3/((4*bw)*(0.3-(samplesStats("cl_F")/(4*bw))))*((1/(1-exp(-(samplesStats("cl_F")/(4*bw))*tau)))*exp(-(samplesStats("cl_F"))/(4*bw)*ts)-(1/(1-exp(-0.3*tau)))*exp(-0.3*ts))
}


Evecpr<-function(ka,D,tau,ts){
     Evecpr<-ka*D/((samplesStats("v_F"))*(ka-(samplesStats("cl_F"))/(samplesStats("v_F"))))*((1/(1-exp(-(samplesStats("cl_F"))/(samplesStats("v_F"))*tau)))*exp(-(samplesStats("cl_F"))/(samplesStats("v_F"))*ts)-(1/(1-exp(-ka*tau)))*exp(-ka*ts))*1000
}


Taccpr<-function(ka,D,tau,ts){
     Taccpr<-ka*D/(314*(ka-(samplesStats("cl_F"))/314))*((1/(1-exp(-(samplesStats("cl_F"))/314*tau)))*exp(-(samplesStats("cl_F"))/314*ts)-(1/(1-exp(-ka*tau)))*exp(-ka*ts))*1000
}


Enocpr<-function(D,tau){
     Enocpr<-D*exp(-(samplesStats("cl_F"))/(samplesStats("v_F"))*(log(0.34*(1-exp(-(samplesStats("cl_F"))/(samplesStats("v_F"))*tau))/((samplesStats("cl_F"))/(samplesStats("v_F"))*(1-exp(-0.34*tau)))))/(0.34-(samplesStats("cl_F"))/(samplesStats("v_F"))))/(samplesStats("v_F"))*(1-exp(-(samplesStats("cl_F"))/(samplesStats("v_F"))*tau))/1000
}


Imacpr<-function(n,D,tau,ts){
     Imacpr<-((D/(1.5*samplesStats("cl_F")))*(1-exp(-(samplesStats("cl_F"))/(samplesStats("v_F"))*1.5))*(exp(-(samplesStats("cl_F"))/(samplesStats("v_F"))*(ts-1.5)))*((exp(-(samplesStats("cl_F"))/(samplesStats("v_F"))*n*tau))-1))/((exp(-(samplesStats("cl_F"))/(samplesStats("v_F"))*tau))-1)    
}



Imasscpr<-function(D,tau){
     Imasscpr<-((D/(1.5*(samplesStats("cl_F"))))*(1-exp(-(samplesStats("cl_F"))/(samplesStats("v_F"))*1.5))*(exp(-(samplesStats("cl_F"))/(samplesStats("v_F"))*(tau-1.5))))/(1-(exp(-(samplesStats("cl_F"))/(samplesStats("v_F"))*tau)))
}

 
TheIRcpr<-function(S,D,tau,ts){
     TheIRcpr<-S*1.85*D/((samplesStats("v_F"))*(1.85-(samplesStats("cl_F"))/(samplesStats("v_F"))))*((1/(1-exp(-(samplesStats("cl_F"))/(samplesStats("v_F"))*tau)))*exp(-(samplesStats("cl_F"))/(samplesStats("v_F"))*ts)-(1/(1-exp(-1.85*tau)))*exp(-1.85*ts))
}

TheCRcpr<-function(S,D,tau){
     TheCRcpr<-S*D/(samplesStats("cl_F")*tau)
}
       
Warcpr<-function(D,tau){
Warcpr<-(((1/((-samplesStats("m")*(samplesStats("cl_F")/samplesStats("v_F")))/(samplesStats("kc")^2)*(1-(samplesStats("kc")*tau/(1-exp(-samplesStats("kc")*tau))))-samplesStats("m")/samplesStats("kc")*log((D/samplesStats("v_F"))/(samplesStats("Cpmax")*(1-exp(-samplesStats("cl_F")/samplesStats("v_F")*tau)))))+3.36)/4.368)^(1/0.383))
}

Wardpr<-function(INR,tau){
Wardpr<-(exp(((-samplesStats("m")*(samplesStats("cl_F")/samplesStats("v_F")))/(samplesStats("kc")^2)*(1-(samplesStats("kc")*tau/(1-exp(-samplesStats("kc")*tau))))-1/(4.368*(INR^0.383)-3.36))*(samplesStats("kc")/samplesStats("m"))))*samplesStats("v_F")*(samplesStats("Cpmax")*(1-exp(-samplesStats("cl_F")/samplesStats("v_F")*tau)))
}

