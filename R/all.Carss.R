all.Carss<-function(){
     cat("\n")
     note_for_CBZss_input()                               ### can be found in note() -YJ
     cat("\n")
     note_for_close_window()
     CBZSSpar<-data.frame(parameter=c("TBW (kg)","PB","VPA","PHT",">65 y/o","D (mg)","tau (hr)","ts (hr)","c (mg/L)"),value=c(0))
     CBZSSpar<-edit(CBZSSpar)
     show(CBZSSpar);cat("\n\n")
##     CBZSSpar<-ycheck(CBZSSpar)
     cat("\n")
     Car.ss(CBZSSpar[9,2],CBZSSpar[7,2],CBZSSpar[8,2],CBZSSpar[6,2],CBZSSpar[1,2],CBZSSpar[2,2],CBZSSpar[3,2],CBZSSpar[4,2],CBZSSpar[5,2])
     note_for_convergence_plots()                          ### can be found in note() -YJ
     ### convergence_plots_sep()
     note_for_CBZss_output()                               ### can be found in note() -YJ
     ### show(samplesStats("*"))
     cat("\n") 
     ### pocpr() can be found in pr() -YJ
     C<-pocpr(1.2,CBZSSpar[6,2],CBZSSpar[7,2],CBZSSpar[8,2])     # calculate predicted steady-state measured concentration of Carbamazepine (equation of oral concentration)
     ### sim<-matrix(C[1 ,1])
     Final<-c(CBZSSpar[9,2],C)
     coutput<-data.frame(Final)
     row.names(coutput)<-list("obs. Cmss","pred. Cmss (mg/L)")
     output1<-coutput
     show(coutput);cat("\n\n")
     C<-pocpr(1.2,CBZSSpar[6,2],CBZSSpar[7,2],CBZSSpar[7,2])
     ### sim<-matrix(C[1 ,1])
     coutput<-data.frame(C)
     colnames(coutput)<-list("estimated Ctss_pr (mg/L)")
     output2<-coutput
     show(coutput)     
     cat("\n")   
     Car.more()                  ### this function can be found in 'adjustss.more()'; doing "C -> D" or "D -> C" here  
}