convergence_plots_sep <- function(){
     graphics.off()                                                            # shuts down all open graphics devices
     samplesHistory("*", mfrow = c(3,1), ask = FALSE)                          # show plots of history
     dev.new()                                                                 # open a new graphic device
     samplesDensity("*", mfrow = c(3,2), ask = FALSE)                          # show plots of density
     dev.new()
     samplesAutoC("*",1, mfrow = c(3,2), ask = FALSE)                          # show pots of autocorrection
    }
    
convergence_plots_con <- function(){
     options("graphics.record"=TRUE)
     graphics.off()                                                            # shuts down all open graphics devices     
     samplesHistory("*", mfrow = c(3,1), ask = FALSE)
     dev.new()
     samplesDensity("*", mfrow = c(3,2), ask = FALSE)
     dev.new()
     samplesAutoC("*",1, mfrow = c(3,2), ask = FALSE)
    }
    