all.ChiValmm<-function(){
     cat("\n")
     note_for_ChiValsm_input()
     cat("\n")
     note_for_close_window()
     ChiValMMpar<-data.frame(subject=c(1,2),TBW=c(0),CBZ=c(0),D=c(0),tau=c(0))
     ChiValMMpar<-edit(ChiValMMpar)
##     ChiValMMpar<-zmscheck(ChiValMMpar)
     cat("\n")
     note_for_ChiValsm_conc_input()
     cat("\n")
     note_for_close_window()
     ChiValMMMpar<-data.frame(subject=c(1),ts=c(0),conc=c(0))
     ChiValMMMpar<-edit(ChiValMMMpar)
##     ChiValMMMpar<-mscheck(ChiValMMMpar)
     for(i in 1:length(unique(ChiValMMpar$subject))){
     k=length(ChiValMMMpar$ts[ChiValMMMpar$subject==i])
     A=ChiValMMMpar$conc[ChiValMMMpar$subject==i]
     B=ChiValMMMpar$ts[ChiValMMMpar$subject==i]
     d=ChiValMMpar$tau[ChiValMMpar$subject==i]
     e=ChiValMMpar$D[ChiValMMpar$subject==i]
     f=ChiValMMpar$TBW[ChiValMMpar$subject==i]
     g=ChiValMMpar$CBZ[ChiValMMpar$subject==i]
     ChiVal.mm(k,A,B,d,e,f,g,i)
     cat("\n")
     note_for_convergence_plots()
     cat("==========================================================================\n")
     cat("                       << Subject",i,">>                           \n\n" )
     note_for_ChiValsm_output()
     convergence_plots_sep()
     show(samplesStats("*"))
     cat("\n") 
     C<-ChiValcpr(e,d,d,f)
     sim<-matrix(C[1 ,1])
     coutput<-data.frame(sim)
     colnames(coutput)<-list("Ctss_pr (mg/L)")
     show(coutput)    
     cat("==========================================================================\n")
     cat(date(),"\n")
     cat("\n\n\n\n")
     }
     cal.again()
}