all.Amianhirsm<-function(){
     cat("\n")                   
     cat("-------------------------------------------------------------\n")
     cat("    --Aminophylline anhydrous IR input data information--    \n")
     note_for_Theirsm_input()
     cat("-------------------------------------------------------------\n")                                                    
     cat("\n")
     note_for_close_window()
     AmianhirSMMpar<-data.frame(parameter=c("Gender","age (yr)","ht (cm)","CHF","smoke","D (mg)","tau (hr)"),
                                value=c(1,67,176,1,0,300,8))       # edit table of aminophylline anhydrous input data information except ts and conc 
     AmianhirSMMpar<-edit(AmianhirSMMpar)
     cat("\n")
     cat("\n Input data are as follows:\n")
     cat(" --------------------------\n")
     show(AmianhirSMMpar);cat("\n\n")
##     AmianhirSMMpar<-ycheck(AmianhirSMMpar)
     cat("\n")
     cat("---------------------------------------\n")
     cat("             -- Output --              \n")
     note_for_Thesm_conc_input()
     cat("---------------------------------------\n")
     cat("\n")
     note_for_close_window()
     AmianhirSMpar<-data.frame(ts=c(4,8),conc=c(8.56,5.67))                               # edit table of aminophylline anhydrous input data information including ts and conc (此表格的呈現方式跟上面表格呈現方式不同)                                                                                                                                                   
     AmianhirSMpar<-edit(AmianhirSMpar)                                                   # show table of aminophylline anhydrous input data information for user editing                                            
     ### AmianhirSMpar<-mscheck(AmianhirSMpar)                                                # avoid user missing input information                                         
     cat("\n")
     cat("\n Input conc. are as follows:\n")
     cat(" --------------------------\n")
     show(AmianhirSMpar);cat("\n\n")
     Amianhir.sm(length(AmianhirSMpar$ts),AmianhirSMpar$conc,AmianhirSMpar$ts,AmianhirSMMpar[7,2],AmianhirSMMpar[6,2],AmianhirSMMpar[3,2],AmianhirSMMpar[2,2],AmianhirSMMpar[5,2],AmianhirSMMpar[1,2],AmianhirSMMpar[4,2])   # calculate individual aminophylline anhydrous PK parameters and show its prediction    
     note_for_convergence_plots()
     ### convergence_plots_sep()
     X <- read.table("params.csv",header=FALSE)
     cl_F <- X[1,2]
     v_F  <- X[2,2]
     cat("------------------------------------------------------------------\n")
     cat("    --Aminophylline anhydrous IR output data information--        \n")
     note_for_Theirsm_output()
     cat("------------------------------------------------------------------\n")
     cat("\n")
     ### show obs. and calc. conc. here  -YJ
     cat("\n\n")
     for(i in 1:length(AmianhirSMpar$ts)){
         Cx<-TheIRsscpr(0.85,AmianhirSMMpar[6,2],AmianhirSMMpar[7,2],AmianhirSMpar$ts[i]) 
         coutput<-data.frame(conc=c("obs. conc.","calc. conc."), values=c(AmianhirSMpar$conc[i],Cx))
         cat("--- Theophylline plasma conc. (mg/L) at Time =",AmianhirSMpar$ts[i],"---\n")
         show(coutput);cat("\n")
     }
     cat("\n") 
     C1<-TheIRsscpr(0.85,AmianhirSMMpar[6,2],AmianhirSMMpar[7,2],AmianhirSMMpar[7,2])
     Tmax<-log(1.85/(cl_F/v_F))/(1.85-(cl_F/v_F))  # r is the Tmax.  -YJ
     C2<-TheIRsscpr(0.85,AmianhirSMMpar[6,2],AmianhirSMMpar[7,2],Tmax)
     coutput<-data.frame(conc=c("Cpss_pr (mg/L)","Ctss_pr (mg/L)"),calc=c(C2,C1))
     show(coutput) 
     cat("\n")   
     Amianhir.more()                                                                                                                                                      
}