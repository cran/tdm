all.Amianhirsm<-function(){
     cat("\n")                   
     cat("-------------------------------------------------------------\n")
     cat("    --Aminophylline anhydrous IR input data information--    \n")
     note_for_Theirsm_input()
     cat("-------------------------------------------------------------\n")                                                    
     cat("\n")
     note_for_close_window()
     AmianhirSMMpar<-data.frame(parameter=c("Gender","age (yr)","ht (cm)","CHF","smoke","D (mg)","tau (hr)"),value=c(0))       # edit table of aminophylline anhydrous input data information except ts and conc 
     AmianhirSMMpar<-edit(AmianhirSMMpar)
     AmianhirSMMpar<-ycheck(AmianhirSMMpar)
     cat("\n")
     cat("-------------------------------------------------------------\n")
     cat("    --Aminophylline anhydrous IR input data information--    \n")
     note_for_Thesm_conc_input()
     cat("-------------------------------------------------------------\n")
     cat("\n")
     note_for_close_window()
     AmianhirSMpar<-data.frame(ts=c(0),conc=c(0))                                         # edit table of aminophylline anhydrous input data information including ts and conc (此表格的呈現方式跟上面表格呈現方式不同)                                                                                                                                                   
     AmianhirSMpar<-edit(AmianhirSMpar)                                                   # show table of aminophylline anhydrous input data information for user editing                                            
     AmianhirSMpar<-mscheck(AmianhirSMpar)                                                # avoid user missing input information                                         
     cat("\n")
     Amianhir.sm(length(AmianhirSMpar$ts),AmianhirSMpar$conc,AmianhirSMpar$ts,AmianhirSMMpar[7,2],AmianhirSMMpar[6,2],AmianhirSMMpar[3,2],AmianhirSMMpar[2,2],AmianhirSMMpar[5,2],AmianhirSMMpar[1,2],AmianhirSMMpar[4,2])   # calculate individual aminophylline anhydrous PK parameters and show its prediction    
     note_for_convergence_plots()
     convergence_plots_sep()
     cat("------------------------------------------------------------------\n")
     cat("    --Aminophylline anhydrous IR output data information--        \n")
     note_for_Theirsm_output()
     cat("------------------------------------------------------------------\n")
     show(samplesStats("*"))
     cat("\n")
     C<-TheIRsscpr(0.85,AmianhirSMMpar[6,2],AmianhirSMMpar[7,2],AmianhirSMMpar[7,2])
     sim<-matrix(C[1 ,1])
     coutput<-data.frame(sim)
     colnames(coutput)<-list("Ctss_pr (mg/L)")
     output1<-coutput
     show(coutput)     
     r=log(1.85-(samplesStats("cl_F")/samplesStats("v_F")))/(1.85-(samplesStats("cl_F")/samplesStats("v_F")))
     C<-TheIRsscpr(0.85,AmianhirSMMpar[6,2],AmianhirSMMpar[7,2],r)
     sim<-matrix(C[1 ,1])
     coutput<-data.frame(sim)
     colnames(coutput)<-list("Cpss_pr (mg/L)")
     output2<-coutput
     show(coutput) 
     cat("\n")   
     Amianhir.more()                # calculate dose adjustment of Aminoflycoside with single subject and each multiple concentrations                                                                                                                                                              
}