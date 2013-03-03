# change the following line since R v.2.15.3 as .onAttach
# (either works fine) [2013/3/4 AM06:27:15]
# .First.lib <- function(...) {

.onAttach <- function(lib, pkg)  {
# echo output to screen
packageStartupMessage("

************************--> tdm <--********************************
           An R Tool for Therapeutic Drug Monitoring               
                          v 2.2.5                                   
  TDM is created for your own personal uses and testing purposes.  
  TDM shall be used as a guide or a decision support tool only.    
  Medical decisions should NOT be solely based on the results of   
  this program. Although this program has been tested thoroughly,  
  the accuracy of the information cannot be guaranteed. Once you   
  use tdm, you have automatically agreed with this disclaimer.      

  ** Please type 'run()' to get started or 
     type 'PheDemo()' for the test runs of estimating phenytoin 
     PK parameters.

ps. Please install openBUGS from http://www.openbugs.info/ 
    based on your platform (linux or Windows) before running 
    this package.
*******************************************************************")

}

