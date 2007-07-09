all.Indmm<-function(){
     cat("\n")
     note_for_Indsm_input()
     cat("\n")
     note_for_close_window()
     IndMMpar<-data.frame(subject=c(1,2),Gender=c(0),bw=c(0),Rit=c(0),D=c(0),tau=c(0))
     IndMMpar<-edit(IndMMpar)
     IndMMpar<-zmscheck(IndMMpar)
     cat("\n")
     note_for_Indsm_conc_input()
     cat("\n")
     note_for_close_window()
     IndMMMpar<-data.frame(subject=c(1),ts=c(0),conc=c(0))
     IndMMMpar<-edit(IndMMMpar)
     IndMMMpar<-mscheck(IndMMMpar)
     for(i in 1:length(unique(IndMMpar$subject))){
     J=length(IndMMMpar$ts[IndMMMpar$subject==i])
     A=IndMMMpar$conc[IndMMMpar$subject==i]
     B=IndMMMpar$ts[IndMMMpar$subject==i]
     d=IndMMpar$tau[IndMMpar$subject==i]
     e=IndMMpar$D[IndMMpar$subject==i]
     f=IndMMpar$bw[IndMMpar$subject==i]
     g=IndMMpar$Rit[IndMMpar$subject==i]
     h=IndMMpar$Gender[IndMMpar$subject==i]
     Ind.mm(J,A,B,d,e,f,g,h,i)
     note_for_convergence_plots()
     cat("\n")
     cat("=============================================================================\n")
     cat("                       << Subject",i,">>                            \n\n" )
     note_for_Indsm_output()
     convergence_plots_sep()
     show(samplesStats("*"))
     cat("\n") 
     C<-Indcpr(samplesStats("ka"),e,d,d)
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