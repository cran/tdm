all.Amidihirss<-function(){
     cat("\n")                                                                       
     cat("--------------------------------------------------------------\n")
     cat("    --Aminophylline dihydrous IR input data information--     \n")
     note_for_Theirss_input()
     cat("--------------------------------------------------------------\n")
     cat("\n")
     note_for_close_window()
     AmidihirSSpar<-data.frame(parameter=c("Gender","age (yr)","ht (cm)","CHF","smoke","D (mg)","tau (hr)","ts (hr)","c (mg/L)"),
                               value=c(1,65,167,0,1,300,8,8,5.67))                                            
     AmidihirSSpar<-edit(AmidihirSSpar)                                                                                                                                           
##     AmidihirSSpar<-ycheck(AmidihirSSpar)    
     cat("\n")
     cat("\n Input data are as follows:\n")
     cat(" --------------------------\n")
     show(AmidihirSSpar);cat("\n\n")
     Amidihir.ss(AmidihirSSpar[9,2],AmidihirSSpar[7,2],AmidihirSSpar[8,2],AmidihirSSpar[6,2],AmidihirSSpar[3,2],AmidihirSSpar[2,2],AmidihirSSpar[5,2],AmidihirSSpar[1,2],AmidihirSSpar[4,2])
     note_for_convergence_plots()
     ### convergence_plots_sep()
     cat("------------------------------------------------------------------\n")
     cat("                       -- Output annotations --                   \n")
     note_for_Theirss_output()
     cat("------------------------------------------------------------------\n")   
     ### show(samplesStats("*"))
     X <- read.table("params.csv",header=FALSE)
     cl_F <- X[1,2]
     v_F  <- X[2,2]
     cat("\n") 
     C1<-TheIRsscpr(0.8,AmidihirSSpar[6,2],AmidihirSSpar[7,2],AmidihirSSpar[8,2])  ### C1 = Cmss_pr
     Tmax<-log(1.85/(cl_F/v_F))/(1.85-(cl_F/v_F))
     C2<-TheIRsscpr(0.8,AmidihirSSpar[6,2],AmidihirSSpar[7,2],Tmax)  ### salt = 0.8 for aminophylline dihydrate!; C2 = Cmax_ss or Cpeak_ss
     C3<-TheIRsscpr(0.8,AmidihirSSpar[6,2],AmidihirSSpar[7,2],AmidihirSSpar[7,2])  ### C3 = trough conc. at SS
     coutput<-data.frame(conc=c("Cmss_obs","** Cmss_pr","** Cpss_pr","** Ctss_pr"),value=c(AmidihirSSpar[9,2],C1,C2,C3))
     show(coutput)     
     cat("\n")       
     Amidihir.more()                                     
}