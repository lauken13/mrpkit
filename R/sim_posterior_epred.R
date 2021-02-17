
#' sim_posterior_epred
#'
#' @name sim_posterior_epred
#' @export
#' @description Helper function to create predictive samples from a glmer class model
#' Takes in model class and newdata to predict for
#' Return a matrix with rows that represent posterior samples and columns that represent
#' data that is being predicted for
#' Only for binomial family models.
#' @param object Model of type glmer class and binomial family
#' @param newdata Data to predict for
#' @param nsamples Number of samples
#' @examples



sim_posterior_epred  <- function(object, newdata, nsamples = 4000){
  if(!class(object)=="glmerMod"){
    stop("Object must a model type of glmerMod")
  }else{
    if(!family(object)$family=="binomial"){
      stop("Model must be a binomial model type")
    }else{
      #Make predictions using merTools (summary stats not needed as we use this as a convenience to get
      #the predicted samples)
      predict_nd <- merTools::predictInterval(merMod = object, newdata = newdata,
                                                         level = 0.95, n.sims = nsamples,
                                                         stat = "median", type="probability",
                                                         include.resid.var = TRUE,
                                                         returnSims = TRUE,
                                                         .parallel=FALSE)
      #Obtain predictive samples
      predicted_results <- plogis(attributes(predict_nd)$sim.results)

      return(t(predicted_results))
    }
  }
}
