all.Amism<-function(){
     cat("\n")
     note_for_Amism_input()
     cat("\n")
     note_for_close_window()
     AmiSMMpar<-data.frame(parameter=c("Gender","age (yr)","bw (kg)","Ht (cm)","Scr (mg/dL)",
                "D (mg)","tau (hr)","tin (hr)"),value=c(1,67,78,176,1.7,250,12,0.5))   # edit table of Aminoglycoside input data information except ts and conc 
     AmiSMMpar<-edit(AmiSMMpar)
     cat("\n Input data are as follows:\n")
     cat(" --------------------------\n")
     show(AmiSMMpar);cat("\n\n")
##     AmiSMMpar<-ycheck(AmiSMMpar)
     cat("\n")
     note_for_Amism_conc_input()
     cat("\n")
     note_for_close_window()
     AmiSMpar<-data.frame(ts=c(6,10),conc=c(16.1,12.5))         # edit table of Aminoglycoside input data information including ts and conc (此表格的呈現方式跟上面表格呈現方式不同)                                                                                              
     AmiSMpar<-edit(AmiSMpar)                                   # show table of Aminoglycoside input data information for user editing
     cat("\n Input conc. are as follows:\n")
     cat(" --------------------------\n")
     show(AmiSMpar);cat("\n\n")
##     AmiSMpar<-mscheck(AmiSMpar)                             # avoid user missing input information
     cat("\n")
     Ami.sm(length(AmiSMpar$ts),AmiSMpar$conc,AmiSMpar$ts,AmiSMMpar[7,2],AmiSMMpar[8,2],AmiSMMpar[6,2],
            AmiSMMpar[3,2],AmiSMMpar[4,2],AmiSMMpar[5,2],AmiSMMpar[1,2],AmiSMMpar[2,2])   # calculate individual Aminoglycoside PK parameters and show its prediction
     note_for_convergence_plots()
     ### convergence_plots_sep()
     note_for_Amism_output()
     ### show(samplesStats("*"))
     ### show obs. & calc. conc. here  -YJ
     cat("\n\n")
     for(i in 1:length(AmiSMpar$ts)){
         Cx<-infcpr(AmiSMMpar[6,2],AmiSMMpar[8,2],AmiSMMpar[7,2],AmiSMpar$ts[i])
         coutput<-data.frame(conc=c("obs. conc.","calc. conc."), values=c(AmiSMpar$conc[i],Cx))
         cat("--- Drug plasma conc. (mg/L) at Time =",AmiSMpar$ts[i],"---\n")
         show(coutput);cat("\n")
     }
     cat("\n")
     ### below 'infcpr()' can be found at 'pr.R' -YJ
     C2<-infcpr(AmiSMMpar[6,2],AmiSMMpar[8,2],AmiSMMpar[7,2],AmiSMMpar[8,2])  ### for peak conc.?  correct! same as following line. --YJ
     ### C<-infcpr(AmiSMMpar[6,2],AmiSMMpar[8,2],AmiSMMpar[7,2],AmiSMMpar[7,2])/exp(-cl/v*(AmiSMMpar[7,2]-AmiSMMpar[8,2]))  ### Ctrough=Cpeak*exp(-kel*(tau-tin)) --YJ
     ### sim<-matrix(C[1 ,1])
     C3<-infcpr(AmiSMMpar[6,2],AmiSMMpar[8,2],AmiSMMpar[7,2],AmiSMMpar[7,2])
     ### sim<-matrix(C[1 ,1])
     coutput<-data.frame(Parameters=c("Cpss_pr (mg/L)","Ctss_pr (mg/L)"),Values=c(C2,C3))
     cat("\n\n");show(coutput);cat("\n")
     Ami.more()                # in 'adjustss.more()'; doing 'C -> D' or 'D -> C'
}
