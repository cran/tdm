all.Tacss<-function(){
     cat("\n")
     note_for_Tacss_input()
     cat("\n")
     note_for_close_window()
     ### TacSSpar<-data.frame(parameter=c("Hem","Alb","Dil","Flu","D (mg)","tau (hr)","ts (hr)","c (mcg/mL)"),value=c(0))
     TacSSpar<-data.frame(parameter=c("Hem","Alb","Dil","Flu","D (mg)","tau (hr)","ts (hr)","c (mcg/mL)"),
                              value=c(1,1,0,1,9.75,12,10,17.5))
     TacSSpar<-edit(TacSSpar)
     cat("\n")
     cat("\n Input data are as follows:\n")
     cat(" --------------------------\n")
     show(TacSSpar);cat("\n\n")
##     TacSSpar<-ycheck(TacSSpar)
     cat("\n")
     Tac.ss(TacSSpar[8,2],TacSSpar[6,2],TacSSpar[7,2],TacSSpar[5,2],TacSSpar[1,2],TacSSpar[2,2],TacSSpar[3,2],TacSSpar[4,2])
     note_for_convergence_plots()
     ### convergence_plots_sep()
     note_for_Tacss_output()
     ### show(samplesStats("*"))
     cat("\n") 
     C<-Taccpr(4.5,TacSSpar[5,2],TacSSpar[6,2],TacSSpar[7,2])  ### here 4.5 is 'ka'& 'vd' = 314 L (see TacSSmode.txt); assumed they are constants in the model. -YJ
     ### sim<-matrix(C[1 ,1])
     ### colnames(coutput)<-list("Cmss_pr (mcg/mL)")
     ### output1<-coutput
     ### show(coutput);cat("\n")
     Css<-Taccpr(4.5,TacSSpar[5,2],TacSSpar[6,2],TacSSpar[6,2])  ### here 4.5 is 'ka'& 'vd' = 314 L (see TacSSmode.txt); assumed they are constants in the model. -YJ
     ### sim<-matrix(C[1 ,1])
     ### coutput<-data.frame(C)
     ### colnames(coutput)<-list("Ctss_pr (mcg/mL)")
     coutput<-data.frame(conc=c("obs. conc. (mcg/mL)","calc. conc. (mcg/mL)","**Ctss_pr (mcg/mL)"),values=c(TacSSpar[8,2],C,Css))
     ### output2<-coutput
     show(coutput)    
     cat("\n")   
     Tac.more()
}