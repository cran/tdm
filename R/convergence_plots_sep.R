convergence_plots_sep <- function(){
     get(getOption("device"))()                                              # open a new window
     samplesHistory("*",mfrow=c(3,1), ask = FALSE)                           # show plots of history
     get(getOption("device"))()                                              
     samplesDensity("*", mfrow = c(3, 2), ask = FALSE)                       # show plots of density
     get(getOption("device"))()
     samplesAutoC("*",1, mfrow = c(3, 2), ask = FALSE)                       # show pots of autocorrection
    }
    
convergence_plots_war_sep <- function(){
     get(getOption("device"))()                                              # open a new window
     samplesHistory("*",mfrow=c(3,2), ask = FALSE)                           # show plots of history
     get(getOption("device"))()                                              
     samplesDensity("*", mfrow = c(3, 2), ask = FALSE)                       # show plots of density
     get(getOption("device"))()
     samplesAutoC("*",1, mfrow = c(3, 2), ask = FALSE)                       # show pots of autocorrection
    }    
    
convergence_plots_con <- function(){
     options("graphics.record"=TRUE)
     samplesHistory("*",mfrow=c(3,1), ask = FALSE)
     samplesDensity("*", mfrow = c(3, 2), ask = FALSE)
     samplesAutoC("*",1, mfrow = c(3, 2), ask = FALSE)
    }
    
    
