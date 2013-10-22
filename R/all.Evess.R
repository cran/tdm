all.Evess<-function(){
     cat("\n")
     note_for_Evess_input()
     cat("\n")
     note_for_close_window()
     ### EveSSpar<-data.frame(parameter=c("bw (kg)","age (yr)","race","Ery","D (mg)","tau (hr)","ts (hr)","c (mcg/L)"),value=c(0))
     EveSSpar<-data.frame(parameter=c("bw (kg)","age (yr)","race","Ery","D (mg)","tau (hr)","ts (hr)","c (mcg/L)"),
                          value=c(76,68,0,0,1.5,24,20,13.1))             ### for model testing -YJ
     EveSSpar<-edit(EveSSpar)                                        ### for model testing -YJ
##     EveSSpar<-ycheck(EveSSpar)
     cat("\n")
     cat("\n Input data are as follows:\n")
     cat(" --------------------------\n")
     show(EveSSpar);cat("\n\n")
     Eve.ss(EveSSpar[8,2],EveSSpar[6,2],EveSSpar[7,2],EveSSpar[5,2],EveSSpar[1,2],EveSSpar[2,2],EveSSpar[3,2],EveSSpar[4,2])
     note_for_convergence_plots()
     ### convergence_plots_sep()
     note_for_Evess_output()          
     ### show(samplesStats("*"))
     cat("\n") 
     C1<-Evecpr(6.07,EveSSpar[5,2],EveSSpar[6,2],EveSSpar[7,2])  ### 6.07 is 'ka' as a constant in model. -YJ
     C2<-Evecpr(6.07,EveSSpar[5,2],EveSSpar[6,2],EveSSpar[6,2])  ### 6.07 is 'ka' as a constant in model. -YJ
     ### sim<-matrix(C[1 ,1])
     coutput<-data.frame(Parameters=c("Cmss_pr (mcg/L)","Ctss_pr (mcg/L)"),Values=c(C1,C2))
     cat("\n\n");show(coutput);cat("\n")
     Eve.more()
}