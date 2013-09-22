all.Enoss<-function(){
     cat("\n")
     note_for_Eno_input()
     cat("\n")
     note_for_close_window()
     EnoSSpar<-data.frame(parameter=c("Gender","TBW (kg)","Scr (mg/dL)","D (mg)","tau (hr)","Amax (IU/mL)"),
                              value=c(1,65,2.1,65,12,1.4))
     EnoSSpar<-edit(EnoSSpar)
     show(EnoSSpar);cat("\n\n")
##     EnoSSpar<-ycheck(EnoSSpar)
     cat("\n")
     ### convert D (mg) into D (IU) here as "1 mg = 100 IU" before running JAGS  --YJ
     EnoSSpar[4,2] <- EnoSSpar[4,2]*100
     Eno.ss(EnoSSpar[6,2],EnoSSpar[5,2],EnoSSpar[4,2],EnoSSpar[2,2],EnoSSpar[1,2],EnoSSpar[3,2])
     note_for_convergence_plots()
     ### convergence_plots_sep()
     note_for_Eno_output()
     ### show(samplesStats("*"))
     cat("\n") 
     C<-Enocpr(EnoSSpar[4,2],EnoSSpar[5,2])
     ### sim<-matrix(C[1 ,1])
     coutput<-data.frame(Parameter=c("obs. Amax (IU/mL)", "calc. Amax (IU/mL)"),Values=c(EnoSSpar[6,2],C))
     show(coutput)
     cat("\n") 
     Eno.more()
}