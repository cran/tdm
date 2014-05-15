sdToss<-function(Dose){

Tau <-Tau
cl_F<-cl_F
v_F <-v_F
ka  <-ka
k<-cl_F/v_F
is.Fentanyl<-is.Fentanyl
Cav<-Cav

### below 'OOsscpr()' can be found at 'pr.oscar.R' -YJ
Tmax<-log(ka/k)/(ka-k)      # estimate Tmax first, and

Cmax_ss<-OOsscpr(Dose,Tau,Tmax)               # then calc. estimated Cpeak
Cmin_ss<-OOsscpr(Dose,Tau,Tau)
if(is.Fentanyl)
coutput<-data.frame(Parameters=c("Cmax_ss (ng/mL)","Cmin_ss (ng/mL)","Dose (mcg)","Tau (hr)"),
                        Values=c(Cmax_ss,Cmin_ss,Dose,Tau))
else
coutput<-data.frame(Parameters=c("Cmax_ss (ng/mL)","Cmin_ss (ng/mL)","Dose (mg)","Tau (hr)"),
                        Values=c(Cmax_ss,Cmin_ss,Dose/1000,Tau))
cat("\n\n");show(coutput);cat("\n")
Cav<<-round((Cmax_ss-Cmin_ss)/log(Cmax_ss/Cmin_ss),digits=2)
}     