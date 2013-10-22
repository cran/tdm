all.Imasm<-function(){
     cat("\n")
     note_for_Imasm_input()
     cat("\n")
     note_for_close_window()
     ImaSMMpar<-data.frame(parameter=c("BW (kg)","OCC","Hb (g/dL)","WBC (10^9/L)","D (mg)","tau (hr)"),value=c(67,1,12.1,110,400,24))  # for model testing
     ### ImaSMMpar<-data.frame(parameter=c("BW (kg)","OCC","Hb (g/dL)","WBC (10^9/L)","D (mg)","tau (hr)"),value=c(0))
     ImaSMMpar<-edit(ImaSMMpar)   ### for model testing
     ### ImaSMMpar<-ycheck(ImaSMMpar)
     cat("\n Input data are as follows:\n")
     cat(" --------------------------\n")
     show(ImaSMMpar);cat("\n\n")
     note_for_Imasm_conc_input()
     cat("\n")
     cat("****************************************************************\n")
     cat("      ATTENTION :                                               \n")
     cat("      Sampling Time (ts) must be greater than 1.5 hr.           \n")
     cat("****************************************************************\n")
     cat("\n\n")
     cat("          Pressing Enter to continue..                          \n")
     readline()
     cat("\n")
     note_for_close_window()
     ImaSMpar<-data.frame(n=c(1,5,8),ts=c(1.2,5,23),conc=c(5.6,5.8,2.1))  ### for model testing
     ### ImaSMpar<-data.frame(n=c(0),ts=c(0),conc=c(0))
     ### ImaSMpar<-edit(ImaSMpar)   ### for model testing
     for(i in 1:length(ImaSMpar$ts)){
       while(ImaSMpar$ts[i]<1.5) {
        readline(" The parameter, ts (hr), must be greater than 1.5.\n Press Enter to fix it...")
        ImaSMpar<-edit(ImaSMpar)
        }
     }
##     ImaSMpar<-mscheck(ImaSMpar)
     cat("\n Input conc are as follows:\n")
     cat(" --------------------------\n")
     show(ImaSMpar);cat("\n\n")
     Ima.sm(length(ImaSMpar$ts),ImaSMpar$conc,ImaSMpar$ts,ImaSMMpar[6,2],ImaSMpar$n,ImaSMMpar[5,2],ImaSMMpar[2,2],ImaSMMpar[1,2],ImaSMMpar[3,2],ImaSMMpar[4,2])
     note_for_convergence_plots()
     ### convergence_plots_sep()
     note_for_Imasm_output()
     ### show(samplesStats("*"))
     cat("\n") 
     C<-Imasscpr(ImaSMMpar[5,2],ImaSMMpar[6,2])
     ### sim<-matrix(C[1 ,1])
     coutput<-data.frame(C)
     colnames(coutput)<-list("Ctss_pr (mg/L)")
     show(coutput)    
     cat("\n") 
     Ima.more()
}