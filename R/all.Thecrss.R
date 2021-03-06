all.Thecrss<-function(){
     cat("\n")                                                                      
     cat("-----------------------------------------------------\n")
     cat("    --Theophylline CR input data information--       \n")
     note_for_Thecrss_input()
     cat("-----------------------------------------------------\n")
     cat("\n")
     note_for_close_window()
     ThecrSSpar<-data.frame(parameter=c("Gender","age (yr)","ht (cm)","CHF","smoke","D (mg)","tau (hr)","c (mg/L)"),
                            value=c(1,65,167,0,1,600,12,7.34))
     ThecrSSpar<-edit(ThecrSSpar)
     cat("\n Input data are as follows:\n")
     cat(" --------------------------\n")
     show(ThecrSSpar);cat("\n\n")
##     ThecrSSpar<-ycheck(ThecrSSpar) 
     cat("\n")                                                                                                                   
     Thecr.ss(ThecrSSpar[8,2],ThecrSSpar[7,2],ThecrSSpar[6,2],ThecrSSpar[3,2],ThecrSSpar[2,2],ThecrSSpar[5,2],ThecrSSpar[1,2],ThecrSSpar[4,2])
     note_for_convergence_plots()
     ### convergence_plots_sep()
     cat("---------------------------------------\n")
     cat("        -- Output Annotations --       \n")
     note_for_Thecr_output()
     cat("---------------------------------------\n")
     ### show(samplesStats("*"))
     cat("\n") 
     C<-TheCRcpr(1,ThecrSSpar[6,2],ThecrSSpar[7,2])   ### '1' here mean salt for theophylline. (non salt form)
     ### sim<-matrix(C[1 ,1])
     coutput<-data.frame(conc=c("Css_obs","Css_pr"),value=c(ThecrSSpar[8,2],C))
     show(coutput)
     cat("\n")   
     Thecr.more()                          
}