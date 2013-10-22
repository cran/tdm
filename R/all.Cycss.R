all.Cycss<-function(){
     cat("\n")
     note_for_Cycss_input()
     cat("\n Warning: here 'c' is Cys conc. at time = ts;\n If ts = 2 hr, then 'c' is C(2)ss.\n\n")
     note_for_close_window()
     ### CycSSpar<-data.frame(parameter=c("bw (kg)","PTD (day)","Dia","D (mg)","tau (hr)","ts (hr)","c (mcg/L)"),value=c(0))
     CycSSpar<-data.frame(parameter=c("bw (kg)","PTD (day)","Dia","D (mg)","tau (hr)","ts (hr)","c (mcg/L)"),
                          value=c(67,30,0,500,12,10,900))  ### for model testing. -YJ
     CycSSpar<-edit(CycSSpar)   ### for model testing
##     CycSSpar<-ycheck(CycSSpar)
     cat("\n")
     cat("\n Input data are as follows:\n")
     cat(" --------------------------\n")
     show(CycSSpar);cat("\n\n")
     Cyc.ss(CycSSpar[7,2],CycSSpar[2,2],CycSSpar[5,2],CycSSpar[6,2],CycSSpar[4,2],CycSSpar[1,2],CycSSpar[3,2])
     note_for_convergence_plots()
     ### convergence_plots_sep()
     note_for_Cycss_output()
     ### show(samplesStats("*"))
     cat("\n") 
     C1<-Cyccpr(CycSSpar[2,2],CycSSpar[1,2],CycSSpar[4,2],CycSSpar[5,2],CycSSpar[6,2])
     ### here we can also calculate C(2) =?  --YJ
     C2<-Cyccpr(CycSSpar[2,2],CycSSpar[1,2],CycSSpar[4,2],CycSSpar[5,2],2)  ### set 'CycSSpar[6,2] = 2' to calc. C(2) here
     C3<-Cyccpr(CycSSpar[2,2],CycSSpar[1,2],CycSSpar[4,2],CycSSpar[5,2],CycSSpar[5,2])
     ### sim<-matrix(C[1 ,1])
     coutput<-data.frame(Parameters=c("Cmss_pr (mcg/L)","C(2)ss_pr (mcg/L)","Ctss_pr (mcg/L)"),Values=c(C1,C2,C3))
     cat("\n\n");show(coutput);cat("\n")
     ### Cyc.more(CycSSpar[2,2],CycSSpar[1,2])
     Cyc.more(30,CycSSpar[1,2])   ### set 'CycSSpar[2,2] =30' for PTD as SS has been reached.
}