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
     cat("\n")                                                                                                                                                    # ªÅ¤@¦æ
     Ami.ss(AmiSSpar[10,2],AmiSSpar[7,2],AmiSSpar[9,2],AmiSSpar[8,2],AmiSSpar[6,2],AmiSSpar[3,2],AmiSSpar[4,2],
            AmiSSpar[5,2],AmiSSpar[1,2],AmiSSpar[2,2])                           # calculate individual Aminoglycoside PK parameters and show its prediction
     note_for_convergence_plots()
     ### convergence_plots_sep()
     note_for_Amiss_output()                                                     # show Aminoglycoside output data information
     ### show(samplesStats("*"))                                                 # show estimated PK parameters of Aminoglycoside
     cat("\n") 
     C1<-infcpr(AmiSSpar[6,2],AmiSSpar[8,2],AmiSSpar[7,2],AmiSSpar[9,2]+AmiSSpar[8,2])     # calculate predicted steady-state measured concentration of Aminoglycoside (equation of intermediate iv infusion concentration)
     ### below 'infcpr()' can be found at 'pr.R' -YJ 
     C2<-infcpr(AmiSSpar[6,2],AmiSSpar[8,2],AmiSSpar[7,2],AmiSSpar[7,2])                   # calculate predicted steady-state trough concentration of Aminoglycoside
     C3<-infcpr(AmiSSpar[6,2],AmiSSpar[8,2],AmiSSpar[7,2],AmiSSpar[8,2])                   # calculate predicted steady-state peak concentration of Aminoglycoside
     coutput<-data.frame(Parameters=c("Cmss_pr (mg/L)","Cpss_pr (mg/L)","Ctss_pr (mg/L)"),
                             Values=c(C1,C3,C2))
     cat("\n");show(coutput);cat("\n\n")     
     cat("\n")   
     Ami.more()               # in 'adjustss.more()'; doing 'C -> D' or 'D -> C'
}