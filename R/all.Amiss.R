all.Amiss<-function(){
     cat("\n")                                                                       # show input and output information make user convenience to use 
     note_for_Amiss_input()
     cat("\n")
     note_for_close_window()
     AmiSSpar<-data.frame(parameter=c("Gender","age (yr)","bw (kg)","Ht (cm)","Scr (mg/dL)","D (mg)","tau (hr)","tin (hr)","ts (hr)","c (mg/L)"),value=c(0))      # edit table of Aminoglycoside input data information     
     AmiSSpar<-edit(AmiSSpar)                                                                                                                                     # show table of Aminoglycoside input data information for user editing
     AmiSSpar<-ycheck(AmiSSpar)                                                                                                                                   # avoid user missing input information
     cat("\n")                                                                                                                                                    # �Ť@��
     Ami.ss(AmiSSpar[10,2],AmiSSpar[7,2],AmiSSpar[9,2],AmiSSpar[8,2],AmiSSpar[6,2],AmiSSpar[3,2],AmiSSpar[4,2],AmiSSpar[5,2],AmiSSpar[1,2],AmiSSpar[2,2])   # calculate individual Aminoglycoside PK parameters and show its prediction
     note_for_convergence_plots()
     convergence_plots_sep()
     note_for_Amiss_output()                                                 # show Aminoglycoside output data information
     show(samplesStats("*"))                                                 # show estimated PK parameters of Aminoglycoside
     cat("\n") 
     C<-infcpr(AmiSSpar[6,2],AmiSSpar[8,2],AmiSSpar[7,2],AmiSSpar[9,2]+AmiSSpar[8,2])                   # calculate predicted steady-state measured concentration of Aminoglycoside (equation of intermediate iv infusion concentration)
     sim<-matrix(C[1 ,1])                                                                 # take the entry of form[1,1] as the input of sim
     coutput<-data.frame(sim)                                                             # �R�W�Ҩ��X�Ӫ�[1,1]��couput
     colnames(coutput)<-list("Cmss_pr (mg/L)")                                            # �éR�W���欰��Cmss_pr(mg/L)
     output1<-coutput                                                                     # �NCmss_pr�R�W��coutput1    
     show(coutput) 
     C<-infcpr(AmiSSpar[6,2],AmiSSpar[8,2],AmiSSpar[7,2],AmiSSpar[7,2])                   # calculate predicted steady-state trough concentration of Aminoglycoside
     sim<-matrix(C[1 ,1])
     coutput<-data.frame(sim)
     colnames(coutput)<-list("Ctss_pr (mg/L)")                                            # �éR�W���欰��Ctss_pr(mg/L)
     output3<-coutput                                                                     # �NCmss_pr�R�W��coutput3
     show(coutput)
     C<-infcpr(AmiSSpar[6,2],AmiSSpar[8,2],AmiSSpar[7,2],AmiSSpar[8,2])                   # calculate predicted steady-state peak concentration of Aminoglycoside
     sim<-matrix(C[1 ,1])                                                                 
     coutput<-data.frame(sim)
     colnames(coutput)<-list("Cpss_pr (mg/L)")                                            # �éR�W���欰��Cpss_pr(mg/L)
     output2<-coutput                                                                     # �NCpss_pr�R�W��coutput2
     show(coutput)    
     cat("\n")   
     Ami.more()                                           # calculate dose adjustment of Aminoflycoside with single subject single concentration
}