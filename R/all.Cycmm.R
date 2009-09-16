all.Cycmm<-function(){
     cat("\n")
     note_for_Cycsm_input()
     cat("\n")
     note_for_close_window()
     CycMMpar<-data.frame(subject=c(1,2),bw=c(0),PTD=c(0),Dia=c(0),D=c(0),tau=c(0))
     CycMMpar<-edit(CycMMpar)
##     CycMMpar<-zmscheck(CycMMpar)
     cat("\n")
     note_for_Cycsm_conc_input()
     cat("\n")
     note_for_close_window()
     CycMMMpar<-data.frame(subject=c(1),ts=c(0),conc=c(0))
     CycMMMpar<-edit(CycMMMpar)
##     CycMMMpar<-mscheck(CycMMMpar)
     for(i in 1:length(unique(CycMMpar$subject))){
     J=length(CycMMMpar$ts[CycMMMpar$subject==i])
     A=CycMMMpar$conc[CycMMMpar$subject==i]
     B=CycMMMpar$ts[CycMMMpar$subject==i]
     d=CycMMpar$PTD[CycMMpar$subject==i]
     e=CycMMpar$tau[CycMMpar$subject==i]
     f=CycMMpar$D[CycMMpar$subject==i]
     g=CycMMpar$bw[CycMMpar$subject==i]
     h=CycMMpar$Dia[CycMMpar$subject==i]
     Cyc.mm(J,A,B,d,e,f,g,h,i)
     note_for_convergence_plots()
     cat("\n")
     cat("=============================================================================\n")
     cat("                 << Subject",i,">>                           \n\n" )
     note_for_Cycsm_output()
     convergence_plots_sep()
     show(samplesStats("*"))
     cat("\n") 
     C<-Cyccpr(d,g,f,e,e)
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