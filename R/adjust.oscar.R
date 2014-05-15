# OC IR 
ocIR.more<-function()
{
### X <- read.table("params.csv",header=FALSE)
### cl_F <- X[1,2]
### v_F  <- X[2,2]
is.Fentanyl<-is.Fentanyl
MEC<-MEC
MSC<-MSC
Tau<-Tau
cl_F<-cl_F
v_F<-v_F
k<-cl_F/v_F
ka<-ka
Cav<-Cav
Tmax<-log(ka/(cl_F/v_F))/(ka-(cl_F/v_F))
opt_Tau<-NULL
opt_Tau<-log((MSC*0.8)/(MEC*1.2))/k + Tmax

  cat("\n")
  file.menu <- c("Cmin_ss -> Dose",
                 "exit")
  pick <- menu(file.menu, title = "<< Dose Adjustment >>") 
  if (pick == 1){
     cat("\n")
     note_for_c_to_d_OO()
     cat("\n")
     cat("\n *** Suggested therapeutic plasma conc. range ***\n\n")
     cat("        Buprenorphine .... 1-5       ng/mL\n")
     cat("        Codeine .......... 10-100    ng/mL\n")
     cat("        Fentanyl ......... 3-300     ng/mL\n")
     cat("        Hydrocodone ...... 10-100    ng/mL\n")
     cat("        Hydromorphone .... 1-30      ng/mL\n")
     cat("        Meperidine ....... 400-700   ng/mL\n")
     cat("        Methadone ........ 100-400   ng/mL\n")
     cat("        Morphine ......... 21-65     ng/mL\n")
     cat("        Oxycodone IR ..... 10-100    ng/mL\n")
     cat("        Oxymorphone ...... 0.65-8.29 ng/mL\n")
     cat("        Tramadol ......... 100-1000  ng/mL\n")
     cat("\n")
     note_for_close_window()
     ocIRdpar<-data.frame(input=c("Cmin_ss (ng/mL)","tau (hr)"),value=c(MEC*1.2,opt_Tau))
     ocIRdpar<-edit(ocIRdpar)
     d<-ocIRdpar[1,2]/(ka/((v_F)*(ka-k))*((1/(1-exp(-k*ocIRdpar[2,2])))*exp(-k*ocIRdpar[2,2])-
        (1/(1-exp(-ka*ocIRdpar[2,2])))*exp(-ka*ocIRdpar[2,2])))
     Tmax<-log(ka/k)/(ka-k)                          # estimate Tmax first, and
     ### Cmax<-OOsscpr(d,ocIRdpar[2,2],Tmax)         # then calc. estimated Cpeak
     Cmax<-ocIRdpar[1,2]/exp(-k*(ocIRdpar[2,2]-Tmax))
     Cav<<-round((Cmax-ocIRdpar[1,2])/log(Cmax/ocIRdpar[1,2]),digits=2)
     ### sim<-matrix(d[1 ,1])
     if(is.Fentanyl)
     doutput<-data.frame(Parameters=c("**calc Cmax_ss (ng/mL)","target Cmin_ss (ng/mL)","tau (hr)"," -> Dose (mcg)"),
                              value=c(Cmax,ocIRdpar[1,2],ocIRdpar[2,2],d))       ### for fentanyl
     else
     doutput<-data.frame(Parameters=c("**calc Cmax_ss (ng/mL)","target Cmin_ss (ng/mL)","tau (hr)"," -> Dose (mg)"),
                              value=c(Cmax,ocIRdpar[1,2],ocIRdpar[2,2],d/1000))  ### d/1000: mcg -> mg for d (Dose)
     cat("\n --- Conc. -> Dose ---\n\n");show(doutput);cat("\n")
     ### MD<- FALSE
     ### plots_tdm(1,d,3.7,v_F,cl_F/v_F,12,ocIRdpar[2,2],MD)
     
     MD<- TRUE
     plots_tdm(1,d,ka,v_F,k,18,ocIRdpar[2,2],MD)
     
   file.menu <- c("Dose -> Cmin_ss",
                  "exit")
   pick <- menu(file.menu, title = "<< D -> C Adjustment >>") 
    if (pick == 1){
     cat("\n")
     note_for_d_to_c_OO()
     cat("\n")
     cat("\n *** Suggested therapeutic plasma conc. range ***\n\n")
     cat("        Buprenorphine .... 1-5       ng/mL\n")
     cat("        Codeine .......... 10-100    ng/mL\n")
     cat("        Fentanyl ......... 3-300     ng/mL\n")
     cat("        Hydrocodone ...... 10-100    ng/mL\n")
     cat("        Hydromorphone .... 1-30      ng/mL\n")
     cat("        Meperidine ....... 400-700   ng/mL\n")
     cat("        Methadone ........ 100-400   ng/mL\n")
     cat("        Morphine ......... 21-65     ng/mL\n")
     cat("        Oxycodone IR ..... 10-100    ng/mL\n")
     cat("        Oxymorphone ...... 0.65-8.29 ng/mL\n")
     cat("        Tramadol ......... 100-1000  ng/mL\n")
     cat("\n")
     note_for_close_window()
     if(is.Fentanyl)
     ocIRcpar<-data.frame(input=c("D (mcg)","tau (hr)"),value=c(d,ocIRdpar[2,2]))
     else
     ocIRcpar<-data.frame(input=c("D (mg)","tau (hr)"),value=c(d/1000,ocIRdpar[2,2]))
     ocIRcpar<-edit(ocIRcpar)
     if(is.Fentanyl)
     d<-ocIRcpar[1,2]       ### for fentanyl
     else
     d<-ocIRcpar[1,2]*1000  ### mg -> mcg for Dose
     C<-ka*d/((v_F)*(ka-k))*((1/(1-exp(-k*ocIRcpar[2,2])))*    ### here C = Cmin_ss
        exp(-k*ocIRcpar[2,2])-(1/(1-exp(-ka*ocIRcpar[2,2])))*exp(-ka*ocIRcpar[2,2]))
     Tmax<-log(ka/k)/(ka-k)        # estimate Tmax first, and
     ### Cmax<-OOsscpr(d,ocIRcpar[2,2],Tmax)         # then calc. estimated Cpeak
     Cmax<-C/exp(-k*(ocIRcpar[2,2]-Tmax))
     Cav<<-round((Cmax-C)/log(Cmax/C),digits=2)
     ### sim<-matrix(C[1 ,1])
     if(is.Fentanyl){
     ### d<-d        ### no conversion for fentanyl
     coutput<-data.frame(Parameters=c("D (mcg)","tau (hr)","calc Cmin_ss (ng/mL)","calc Cmax_ss (ng/mL)"),
                              value=c(d,ocIRcpar[2,2],C,Cmax))}
     else{
     ### d<-d/1000   ### mcg -> mg for Dose
     coutput<-data.frame(Parameters=c("D (mg)","tau (hr)","calc Cmin_ss (ng/mL)","calc Cmax_ss (ng/mL)"),
                              value=c(d/1000,ocIRcpar[2,2],C,Cmax))}
     cat("\n --- Dose -> Conc. ---\n\n");show(coutput);cat("\n")
     
     MD<- TRUE
     plots_tdm(1,d,ka,v_F,k,18,ocIRcpar[2,2],MD)  ### where '12' is dosing #. --YJ
     
     ocIR.more()
       } else {
             if (pick == 2){
             	oscar()
        }
  }} else {
        if (pick == 2){
             oscar()
    }  
  }
}

