all.Amidihinfusionss<-function(){
     cat("\n")                                                                        
     cat("-----------------------------------------------------------------------\n")
     cat("    --Aminophylline dihydrous iv infusion input data information--     \n")
     note_for_Theinfusionss_input()
     cat("-----------------------------------------------------------------------\n")                                                            
     cat("\n")
     note_for_close_window()
     AmidihinfusionSSpar<-data.frame(parameter=c("Gender","age (yr)","ht (cm)","CHF","smoke","DL (mg)","tinf (hr)","Ro (mg/hr)","ts (hr)","c (mg/L)"),
                                     value=c(0,65,187,1,1,400,24,20,36,7.61))
     AmidihinfusionSSpar<-edit(AmidihinfusionSSpar)                                                                                                                                                                                                              
##     AmidihinfusionSSpar<-ycheck(AmidihinfusionSSpar)     
     cat("\n")
     cat("\n Input data are as follows:\n")
     cat(" --------------------------\n")
     show(AmidihinfusionSSpar);cat("\n\n")
     ### original of the following line was wrong! -YJ
     Amidihinfusion.ss(AmidihinfusionSSpar[10,2],AmidihinfusionSSpar[9,2],AmidihinfusionSSpar[3,2],AmidihinfusionSSpar[2,2],
     AmidihinfusionSSpar[5,2],AmidihinfusionSSpar[1,2],AmidihinfusionSSpar[4,2],AmidihinfusionSSpar[6,2],AmidihinfusionSSpar[8,2],
     AmidihinfusionSSpar[7,2])
     note_for_convergence_plots()
     ### convergence_plots_sep()
     cat("-----------------------------------------------------------------------\n")
     cat("    --Aminophylline dihydrous iv infusion input data information--     \n")
     note_for_Theinfusionss_output()
     cat("-----------------------------------------------------------------------\n")
     ### show(samplesStats("*"))
     cat("\n") 
     X <- read.table("params.csv",header=FALSE)
     cl <- X[1,2]
     C1<-Theinfusioncpr(0.8,AmidihinfusionSSpar[6,2],AmidihinfusionSSpar[8,2],AmidihinfusionSSpar[9,2],AmidihinfusionSSpar[7,2])
     C2<-0.8*AmidihinfusionSSpar[8,2]/(cl)
     coutput<-data.frame(conc=c("Cm_obs","Cm_pr","Css_pr"),value=c(AmidihinfusionSSpar[10,2],C1,C2))
     show(coutput)
     Amidihinfusion.more()                         
}