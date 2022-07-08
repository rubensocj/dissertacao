#' Calcula a Raiz do Erro M?dio Quadr?tico - RMSE (ROOT MEAN SQUARE ERROR)
#'
#' @param previsto: array contendo os valores calculados por algum modelo de previs?o
#' @param original: array contendo os valores originais
#' @return valor do RMSE calculado
#'
rmse = function(previsto, original) {
  return(sqrt(mean((previsto - original)^2)))
}

#' Calcula o Erro Percentual Absoluto M?dio - MAPE (MEAN ABSOLUTE PERCENTAGE ERROR)
#'
#' @param previsto: array contendo os valores calculados por algum modelo de previs?o
#' @param original: array contendo os valores originais
#' @return valor do MAPE calculado
#'
mape = function(previsto, original) {
  return(mean(abs((original - previsto)/original)))
}

#' Calcula o Erro Absoluto M?dio - MAPE (MEAN ABSOLUTE ERROR)
#'
#' @param previsto: array contendo os valores calculados por algum modelo de previs?o
#' @param original: array contendo os valores originais
#' @return valor do MAE calculado
#'
mae = function(previsto, original) {
  return(mean(abs((previsto - original))))
}

#' Calcula o R-Squared R^2
#'
#' @param previsto: array contendo os valores calculados por algum modelo de previs?o
#' @param original: array contendo os valores originais
#' @return valor do R^2 calculado
#'
coef.det <- function(pred, obs) {
  mod <- lm(obs~pred)
  anova <- summary(mod)
  R2 <- anova$r.squared
  return(R2)
  # return(1 - sum((obs-pred)^2) / sum((obs-mean(obs))^2))
}

theilsu <- function(pred, obs) {
  n <- length(pred)
  fpe <- pred[2:n]/obs[1:(n-1)] - 1
  ape <- obs[2:n]/obs[1:(n-1)] - 1
  U <- sqrt(sum((fpe - ape)^2)/sum(ape^2))
  return(U)
}

getTheil = function(target,forecast){
  seriesSize = length(target)
  squaredSumTF = 0; squaredSumTT = 0
  i=2
  while(i<=seriesSize){
    squaredSumTF = squaredSumTF + (target[i]-forecast[i])^2
    squaredSumTT = squaredSumTT + (target[i]-target[i-1])^2
    #valor.theil[i]=((target[i]-forecast[i])^2)/(sum((target[i]-target[i+1])^2))
    i=i+1
  }
  Theil = squaredSumTF/squaredSumTT
  return(Theil)
}

# https://github.com/cran/hydroGOF/blob/master/R/NSE.R
NSE.default <- function (sim, obs, na.rm=TRUE, FUN=NULL, epsilon=c(0, "Pushpalatha2012", "other"), epsilon.value=NA, ...){

  if ( is.na(match(class(sim), c("integer", "numeric", "ts", "zoo", "xts"))) |
       is.na(match(class(obs), c("integer", "numeric", "ts", "zoo", "xts")))
  ) stop("Invalid argument type: 'sim' & 'obs' have to be of class: c('integer', 'numeric', 'ts', 'zoo', 'xts')")

  # vi <- valindex(sim, obs)
  #
  # obs <- obs[vi]
  # sim <- sim[vi]
  #
  # if (!is.null(FUN)) {
  #   new <- preproc(sim=sim, obs=obs, FUN=FUN, epsilon=epsilon, epsilon.value=epsilon.value, ...)
  #   sim <- new[["sim"]]
  #   obs <- new[["obs"]]
  # } # IF end

  denominator <- sum( (obs - mean(obs))^2 )

  if (denominator != 0) {

    NS <- 1 - ( sum( (obs - sim)^2 ) / denominator )

  } else {
    NS <- NA
    warning("'sum((obs - mean(obs))^2)=0' => it is not possible to compute 'NSE'")
  } # ELSE end

  return(NS)

} # 'NSE' end

# https://github.com/cran/hydroGOF/blob/master/R/nrmse.R
nrmse.default <- function (sim, obs, na.rm=TRUE, norm="sd", ...) {

  # Checking that the user provied a valid argument for 'norm'
  if (is.na(match(norm, c("sd", "maxmin") ) ) )
    stop("Invalid argument: 'norm' must be in c('sd', 'maxmin')")

  if ( is.na(match(class(sim), c("integer", "numeric", "ts", "zoo", "xts"))) |
       is.na(match(class(obs), c("integer", "numeric", "ts", "zoo", "xts")))
  ) stop("Invalid argument type: 'sim' & 'obs' have to be of class: c('integer', 'numeric', 'ts', 'zoo', 'xts')")

  # vi <- valindex(sim, obs)

  # obs <- obs[vi]
  # sim <- sim[vi]

  if (norm=="sd") {
    cte <- sd(obs, na.rm=na.rm)
  } else if (norm=="maxmin") {
    cte <- ( max(obs, na.rm= na.rm) - min(obs, na.rm =na.rm) )
  } # ELSE end

  rmse <- rmse(sim, obs)

  if (max(obs, na.rm= na.rm) - min(obs, na.rm= na.rm) != 0) {

    nrmse <- rmse / cte

  } else {
    nrmse <- NA
    warning("'obs' is constant, it is not possible to compute 'nrmse'")
  } # ELSE end

  return(nrmse)

} # 'nrmse.default' end

pbias.default <- function (sim, obs, na.rm=TRUE, ...){

  if ( is.na(match(class(sim), c("integer", "numeric", "ts", "zoo"))) |
       is.na(match(class(obs), c("integer", "numeric", "ts", "zoo")))
  ) stop("Invalid argument type: 'sim' & 'obs' have to be of class: c('integer', 'numeric', 'ts', 'zoo')")

  # index of those elements that are present both in 'x' and 'y' (NON- NA values)
  # vi <- valindex(sim, obs)

  # Filtering 'obs' and 'sim', selecting only those pairs of elements
  # that are present both in 'x' and 'y' (NON- NA values)
  # obs <- obs[vi]
  # sim <- sim[vi]

  # lenght of the data sets that will be ocnsidered for the ocmputations
  n <- length(obs)

  denominator <- sum( obs )

  if (denominator != 0) {

    pbias <- 100 * ( sum( sim - obs ) / denominator )

  } else {
    pbias <- NA
    warning("'sum((obs)=0', it is not possible to compute 'pbias'")
  } # ELSE end

  return( round(pbias, 1) )

} # 'pbias.default' end

# https://github.com/cran/hydroGOF/blob/master/R/md.R
md.default <- function (sim, obs, j=1, na.rm=TRUE, ...){

  if ( is.na(match(class(sim), c("integer", "numeric", "ts", "zoo"))) |
       is.na(match(class(obs), c("integer", "numeric", "ts", "zoo")))
  ) stop("Invalid argument type: 'sim' & 'obs' have to be of class: c('integer', 'numeric', 'ts', 'zoo')")

  # Checking that the provided exponent is positive
  if (j < 0 ) stop("Invalid argument: 'j' must be positive")

  # index of those elements that are present both in 'x' and 'y' (NON- NA values)
  # vi <- valindex(sim, obs)

  # Filtering 'obs' and 'sim', selecting only those pairs of elements
  # that are present both in 'x' and 'y' (NON- NA values)
  # obs <- obs[vi]
  # sim <- sim[vi]

  # the next two lines are required for avoiding an strange behaviour
  # of the difference function when sim and obs are time series.
  if ( !is.na(match(class(sim), c("ts", "zoo"))) ) sim <- as.numeric(sim)
  if ( !is.na(match(class(obs), c("ts", "zoo"))) ) obs <- as.numeric(obs)

  # Mean of the observed values
  Om <- mean(obs)

  denominator <- sum( ( abs(sim - Om) + abs(obs - Om)  )^j )

  if (denominator != 0) {

    d1 <- 1 - ( sum( ( abs(obs - sim) )^j ) / denominator )

  } else {
    d1 <- NA
    warning("'sum((abs(sim-Om)+abs(obs-Om))^j)=0', it is not possible to compute 'md'")
  } # ELSE end

  return(d1)

} # 'md.default' end

#' Mean Arctangent Absolute Percentage Error
#' https://github.com/tidyverts/fabletools/blob/master/R/accuracy.R
#'
#' @inheritParams point_accuracy_measures
#'
#' @references
#' Kim, Sungil and Heeyoung Kim (2016) "A new metric of absolute percentage error
#' for intermittent demand forecasts". \emph{International Journal of Forecasting},
#' \bold{32}(3), 669-679.
#'
#' @export
MAAPE <- function(obs, pre, na.rm = TRUE, ...){
  mean(atan(abs((obs - pre) / obs)), na.rm = na.rm)
}

my_AIC <- function(model) {
  return(nrow(model$model)*(log(2*pi)+1+log((sum(model$residuals^2)/nrow(model$model))))+
           ((length(model$coefficients)+1)*2))
}
my_BIC <- function(model) {
  res<-model$residuals
  n<-nrow(model$model)
  w<-rep(1,n)
  ll<-0.5 * (sum(log(w)) - n * (log(2 * pi) + 1 - log(n) + log(sum(w * res^2))))
  k.original<-length(model$coefficients)
  df.ll<-k.original+1
  bic<- -2 * ll + log(n) * df.ll
  return(bic)
}
getPOCID = function(target,forecast){
  Dt = 0
  i=1
  seriesSize = length(target)
  while(i<seriesSize){
    TT = (target[i+1]-target[i])
    FF = (forecast[i+1]-forecast[i])
    #if((target[i+1]-target[i])*(forecast[i+1]-forecast[i])>0){
    if(TT*FF>0 | (TT==0 & FF==0)){
      Dt= Dt + 1
    }
    i=i+1
    #print(i)
  }
  POCID=100*(Dt/(seriesSize-1))
  POCID
}

# Percent of Correct State for Wet (PCSWet), Dry (PCSDry), and both (PCSA)
PCSWet <- function(obs, est) {
  I1 <- I11 <- numeric(length = length(obs))

  I1[obs > 0] <- 1
  I11[obs > 0 & est > 0] <- 1

  return(100*(sum(I11)/sum(I1)))
}
PCSDry <- function(obs, est) {
  I0 <- I00 <- numeric(length = length(obs))

  I0[obs == 0] <- 1
  I00[obs == 0 & est == 0] <- 1

  return(100*(sum(I00)/sum(I0)))
}
PCSA <- function(obs, est) {
  I0 <- I00 <- numeric(length = length(obs))
  I1 <- I11 <- numeric(length = length(obs))

  I0[obs == 0] <- 1
  I00[obs == 0 & est == 0] <- 1

  I1[obs > 0] <- 1
  I11[obs > 0 & est > 0] <- 1

  return(100*(sum(I00+I11)/length(obs)))
}
