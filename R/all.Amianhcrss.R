all.Amianhcrss<-function(){
     cat("\n")   
     cat("--------------------------------------------------------------\n")
     cat("    --Aminophylline anhydrous CR input data information--     \n")
     note_for_Thecrss_input()
     cat("--------------------------------------------------------------\n")                                                                   
     cat("\n")
     note_for_close_window()
     AmianhcrSSpar<-data.frame(parameter=c("Gender","age (yr)","ht (cm)","CHF","smoke","D (mg)","tau (hr)","c (mg/L)"),
                               value=c(1,65,167,0,1,600,12,7.34))
     AmianhcrSSpar<-edit(AmianhcrSSpar)
     cat("\n")
     cat("\n Input data are as follows:\n")
     cat(" --------------------------\n")
     show(AmianhcrSSpar);cat("\n\n")                                                                                                                                           
##     AmianhcrSSpar<-ycheck(AmianhcrSSpar)       
     Amianhcr.ss(AmianhcrSSpar[8,2],AmianhcrSSpar[7,2],AmianhcrSSpar[6,2],AmianhcrSSpar[3,2],AmianhcrSSpar[2,2],AmianhcrSSpar[5,2],AmianhcrSSpar[1,2],AmianhcrSSpar[4,2])
     note_for_convergence_plots()
     ### convergence_plots_sep()
     cat("---------------------------------------\n")
     cat("        -- Output Annotations --       \n")
     note_for_Thecr_output()
     cat("---------------------------------------\n")
     ### show(samplesStats("*"))
     cat("\n") 
     C<-TheCRcpr(0.85,AmianhcrSSpar[6,2],AmianhcrSSpar[7,2])
     ### sim<-matrix(C[1 ,1])
     coutput<-data.frame(conc=c("Css_obs","Css_pr"),value=c(AmianhcrSSpar[8,2],C))
     show(coutput) 
     cat("\n")   
     Amianhcr.more()                            
}