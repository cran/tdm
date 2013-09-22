all.Amiss<-function(){
     cat("\n")                                                                       # show input and output information make user convenience to use 
     note_for_Amiss_input()
     cat("\n")
     note_for_close_window()
     AmiSSpar<-data.frame(parameter=c("Gender","age (yr)","bw (kg)","Ht (cm)","Scr (mg/dL)","D (mg)","tau (hr)","tin (hr)",
               "ts (hr)","c (mg/L)"),value=c(1,82,67,178,1.7,250,12,0.45,10,12.6)) # edit table of Aminoglycoside input data information     
     AmiSSpar<-edit(AmiSSpar) 
     show(AmiSSpar);cat("\n\n")                                                    # show table of Aminoglycoside input data information for user editing
     ### AmiSSpar<-ycheck(AmiSSpar)                                                                                                                                   # avoid user missing input information
     cat("\n")                                                                                                                                                    # 空一行
     Ami.ss(AmiSSpar[10,2],AmiSSpar[7,2],AmiSSpar[9,2],AmiSSpar[8,2],AmiSSpar[6,2],AmiSSpar[3,2],AmiSSpar[4,2],
            AmiSSpar[5,2],AmiSSpar[1,2],AmiSSpar[2,2])                           # calculate individual Aminoglycoside PK parameters and show its prediction
     note_for_convergence_plots()
     ### convergence_plots_sep()
     note_for_Amiss_output()                                                     # show Aminoglycoside output data information
     ### show(samplesStats("*"))                                                 # show estimated PK parameters of Aminoglycoside
     cat("\n") 
     C<-infcpr(AmiSSpar[6,2],AmiSSpar[8,2],AmiSSpar[7,2],AmiSSpar[9,2]+AmiSSpar[8,2])     # calculate predicted steady-state measured concentration of Aminoglycoside (equation of intermediate iv infusion concentration)
     ### sim<-matrix(C[1 ,1])                                                             # take the entry of form[1,1] as the input of sim
     coutput<-data.frame(C)                                                               # 命名所取出來的[1,1]為couput
     colnames(coutput)<-list("Cmss_pr (mg/L)")                                            # 並命名此欄為為Cmss_pr(mg/L)
     output1<-coutput                                                                     # 將Cmss_pr命名為coutput1    
     show(coutput);cat("\n\n")
     ### below 'infcpr()' can be found at 'pr.R' -YJ 
     C<-infcpr(AmiSSpar[6,2],AmiSSpar[8,2],AmiSSpar[7,2],AmiSSpar[7,2])                   # calculate predicted steady-state trough concentration of Aminoglycoside
     ### sim<-matrix(C[1 ,1])
     coutput<-data.frame(C)
     colnames(coutput)<-list("Ctss_pr (mg/L)")                                            # 並命名此欄為為Ctss_pr(mg/L)
     output3<-coutput                                                                     # 將Cmss_pr命名為coutput3
     show(coutput);cat("\n\n") 
     C<-infcpr(AmiSSpar[6,2],AmiSSpar[8,2],AmiSSpar[7,2],AmiSSpar[8,2])                   # calculate predicted steady-state peak concentration of Aminoglycoside
     ### sim<-matrix(C[1 ,1])                                                                 
     coutput<-data.frame(C)
     colnames(coutput)<-list("Cpss_pr (mg/L)")                                            # 並命名此欄為為Cpss_pr(mg/L)
     output2<-coutput                                                                     # 將Cpss_pr命名為coutput2
     show(coutput);cat("\n\n")     
     cat("\n")   
     Ami.more()               # in 'adjustss.more()'; doing 'C -> D' or 'D -> C'
}