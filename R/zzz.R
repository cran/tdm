# change the following line since R v.2.15.3 as .onAttach [2013/3/4 AM 06:27:15]


.onAttach <- function(lib, pkg)  {
# echo output to screen
packageStartupMessage("

************************--> tdm <--********************************
     Data Analysis for Therapeutic Drug Monitoring (TDM)              
                          v 3.0.3                                 
  DISCLAIMER:                        
  tdm is created for your own personal uses and testing purposes.  
  TDM shall be used as a guide or a decision support tool only.    
  Medical decisions should NOT be solely based on the results of   
  this program. Although this program has been tested thoroughly,  
  the accuracy of the information cannot be guaranteed. Once you   
  use tdm, you have automatically agreed with this disclaimer.      

  ** Please type 'run()' to get started or 
     type 'demo(phenytoin)' for the test runs of estimation
     phenytoin PK parameters.

*******************************************************************")
}

