all.Enfss<-function(){
     cat("\n")
     note_for_Enfss_input()
     cat("\n")
     note_for_close_window()
     EnfSSpar<-data.frame(parameter=c("Gender","bw (kg)","D (mg)","tau (hr)","ts (hr)","c (mg/L)"),
                          value=c(1,78,90,12,10,2.0))
     EnfSSpar<-edit(EnfSSpar)
##     EnfSSpar<-ycheck(EnfSSpar)
     cat("\n Input data are as follows:\n")
     cat(" --------------------------\n")
     show(EnfSSpar);cat("\n\n")
     Enf.ss(EnfSSpar[6,2],EnfSSpar[4,2],EnfSSpar[5,2],EnfSSpar[3,2],EnfSSpar[2,2],EnfSSpar[1,2])
     note_for_convergence_plots()
     ### convergence_plots_sep()
     X <- read.table("params.csv",header=FALSE)
     cl_F <- X[1,2]
     v_F  <- X[3,2]
     ka   <- X[2,2]
     note_for_Enfss_output()
     ### show(samplesStats("*"))
     cat("\n") 
     C1<-enfcpr(EnfSSpar[3,2],EnfSSpar[4,2],EnfSSpar[5,2])
     C2<-enfcpr(EnfSSpar[3,2],EnfSSpar[4,2],EnfSSpar[4,2])
     ### sim<-matrix(C[1 ,1])
     coutput<-data.frame(conc=c("Cmss_obs","Cmss_pr","Ctss_pr"),value=c(EnfSSpar[6,2],C1,C2))
     show(coutput)    
     cat("\n")  
     Enf.more()
}