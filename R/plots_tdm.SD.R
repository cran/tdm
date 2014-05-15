plots_tdm.SD<-function(Dose){

cl_F<-cl_F
v_F <-v_F
ka  <-ka
Tau <-Tau

MD<-FALSE
plots_tdm(1,Dose,ka,v_F,cl_F/v_F,18,Tau,MD)

MD<-TRUE
plots_tdm(1,Dose,ka,v_F,cl_F/v_F,18,Tau,MD)
}