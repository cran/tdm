all.Amism<-function(){
     cat("\n")
     note_for_Amism_input()
     cat("\n")
     note_for_close_window()
     AmiSMMpar<-data.frame(parameter=c("Gender","age (yr)","bw (kg)","Ht (cm)","Scr (mg/dL)","D (mg)","tau (hr)","tin (hr)"),value=c(0))   # edit table of Aminoglycoside input data information except ts and conc 
     AmiSMMpar<-edit(AmiSMMpar)
     AmiSMMpar<-ycheck(AmiSMMpar)
     cat("\n")
     note_for_Amism_conc_input()
     cat("\n")
     note_for_close_window()
     AmiSMpar<-data.frame(ts=c(0),conc=c(0))                                         # edit table of Aminoglycoside input data information including ts and conc (此表格的呈現方式跟上面表格呈現方式不同)                                                                                              
     AmiSMpar<-edit(AmiSMpar)                                                        # show table of Aminoglycoside input data information for user editing
     AmiSMpar<-mscheck(AmiSMpar)                                                     # avoid user missing input information
     cat("\n")
     Ami.sm(length(AmiSMpar$ts),AmiSMpar$conc,AmiSMpar$ts,AmiSMMpar[7,2],AmiSMMpar[8,2],AmiSMMpar[6,2],AmiSMMpar[3,2],AmiSMMpar[4,2],AmiSMMpar[5,2],AmiSMMpar[1,2],AmiSMMpar[2,2])   # calculate individual Aminoglycoside PK parameters and show its prediction
     note_for_convergence_plots()
     convergence_plots_sep()
     note_for_Amism_output()
     show(samplesStats("*"))
     cat("\n")
     C<-infcpr(AmiSMMpar[6,2],AmiSMMpar[8,2],AmiSMMpar[7,2],AmiSMMpar[8,2])
     sim<-matrix(C[1 ,1])
     coutput<-data.frame(sim)
     colnames(coutput)<-list("Cpss_pr (mg/L)")
     output1<-coutput
     show(coutput)  
     C<-infcpr(AmiSMMpar[6,2],AmiSMMpar[8,2],AmiSMMpar[7,2],AmiSMMpar[7,2])
     sim<-matrix(C[1 ,1])
     coutput<-data.frame(sim)
     colnames(coutput)<-list("Ctss_pr (mg/L)")
     output2<-coutput
     show(coutput)     
     cat("\n")
     Ami.more()                # calculate dose adjustment of Aminoflycoside with single subject and each multiple concentrations
}
