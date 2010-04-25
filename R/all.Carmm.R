all.Carmm<-function(){
     cat("\n")
     note_for_CBZsm_input()
     cat("\n")
     note_for_close_window()
     CBZMMpar<-data.frame(subject=c(1,2),TBW=c(0),PB=c(0),VPA=c(0),PHT=c(0),E=c(0),D=c(0),tau=c(0))
     CBZMMpar<-edit(CBZMMpar)
##     CBZMMpar<-ymscheck(CBZMMpar)
     cat("\n")
     note_for_CBZsm_conc_input()
     cat("\n")
     note_for_close_window()
     CBZMMMpar<-data.frame(subject=c(1),ts=c(0),conc=c(0))
     CBZMMMpar<-edit(CBZMMMpar)
     CBZMMMpar<-mscheck(CBZMMMpar)
     for(i in 1:length(unique(CBZMMpar$subject))){
     J=length(CBZMMMpar$ts[CBZMMMpar$subject==i])
     A=CBZMMMpar$conc[CBZMMMpar$subject==i]
     B=CBZMMMpar$ts[CBZMMMpar$subject==i]
     C=CBZMMpar$tau[CBZMMpar$subject==i]
     d=CBZMMpar$D[CBZMMpar$subject==i]
     e=CBZMMpar$TBW[CBZMMpar$subject==i]
     F=CBZMMpar$PB[CBZMMpar$subject==i]
     G=CBZMMpar$VPA[CBZMMpar$subject==i]
     H=CBZMMpar$PHT[CBZMMpar$subject==i]
     K=CBZMMpar$E[CBZMMpar$subject==i]
     Car.mm(J,A,B,C,d,e,F,G,H,K,i)
     note_for_convergence_plots()
     cat("\n")
     cat("==========================================================================\n")
     cat("                        << Subject",i,">>                           \n\n" )
     note_for_CBZsm_output()
     convergence_plots_sep()
     show(samplesStats("*"))
     cat("\n")
     C<-pocpr(1.2,d,C,C)
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