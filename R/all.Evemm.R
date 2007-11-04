all.Evemm<-function(){
     cat("\n")
     note_for_Evesm_input()
     cat("\n")
     note_for_close_window()
     EveMMpar<-data.frame(subject=c(1,2),bw=c(0),age=c(0),race=c(0),Ery=c(0),D=c(0),tau=c(0))
     EveMMpar<-edit(EveMMpar)
     EveMMpar<-ymscheck(EveMMpar)
     cat("\n")
     note_for_Evesm_conc_input()
     cat("\n")
     note_for_close_window()
     EveMMMpar<-data.frame(subject=c(1),ts=c(0),conc=c(0))
     EveMMMpar<-edit(EveMMMpar)
     EveMMMpar<-mscheck(EveMMMpar)
     for(i in 1:length(unique(EveMMpar$subject))){
     a=length(EveMMMpar$ts[EveMMMpar$subject==i])
     b=EveMMMpar$conc[EveMMMpar$subject==i]
     C=EveMMMpar$ts[EveMMMpar$subject==i]
     d=EveMMpar$tau[EveMMpar$subject==i]
     e=EveMMpar$D[EveMMpar$subject==i]
     f=EveMMpar$bw[EveMMpar$subject==i]
     g=EveMMpar$age[EveMMpar$subject==i]
     h=EveMMpar$race[EveMMpar$subject==i]
     J=EveMMpar$Ery[EveMMpar$subject==i]
     Eve.mm(a,b,C,d,e,f,g,h,J,i)
     note_for_convergence_plots()
     cat("\n")
     cat("=============================================================================\n")
     cat("                        << Subject",i,">>                           \n\n" )
     note_for_Evesm_output()
     convergence_plots_sep()
     show(samplesStats("*"))
     cat("\n") 
     C<-Evecpr(6.07,e,d,d)
     sim<-matrix(C[1 ,1])
     coutput<-data.frame(sim)
     colnames(coutput)<-list("Ctss_pr (mcg/L)")
     show(coutput)    
     cat("=============================================================================\n")
     cat(date(),"\n")
     cat("\n\n\n\n")
     }
     cal.again()
}