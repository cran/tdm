all.Ritmm<-function(){
     cat("\n")
     note_for_Ritsm_input()
     cat("\n")
     note_for_close_window()
     RitMMpar<-data.frame(subject=c(1,2),LPV=c(0),D=c(0),tau=c(0))
     RitMMpar<-edit(RitMMpar)
##     RitMMpar<-wmscheck(RitMMpar)
     cat("\n")
     note_for_Ritsm_conc_input()
     cat("\n")
     note_for_close_window()
     RitMMMpar<-data.frame(subject=c(1),ts=c(0),conc=c(0))
     RitMMMpar<-edit(RitMMMpar)
     RitMMMpar<-mscheck(RitMMMpar)
     for(i in 1:length(unique(RitMMpar$subject))){
     g=length(RitMMMpar$ts[RitMMMpar$subject==i])
     A=RitMMMpar$conc[RitMMMpar$subject==i]
     B=RitMMMpar$ts[RitMMMpar$subject==i]
     d=RitMMpar$tau[RitMMpar$subject==i]
     e=RitMMpar$D[RitMMpar$subject==i]
     f=RitMMpar$LPV[RitMMpar$subject==i]
     Rit.mm(g,A,B,d,e,f,i)
     note_for_convergence_plots()
     cat("\n")
     cat("=============================================================================\n")
     cat("                       << Subject",i,">>                        \n\n" )
     note_for_Ritsm_output()
     convergence_plots_sep()
     show(samplesStats("*"))
     cat("\n") 
     C<-Ritcpr(samplesStats("ka"),e,d,d)
     sim<-matrix(C[1 ,1])
     coutput<-data.frame(sim)
     colnames(coutput)<-list("Ctss_pr (mg/L)")
     show(coutput)    
     cat("=============================================================================\n")
     cat(date(),"\n")
     cat("\n\n\n\n")
     }
     cal.again()
}