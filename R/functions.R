#' Function for importing packages
#'
#' Try to load a package, if it is not installed, perform installation
#' and loads in workspace
#'
#' @param x package names to be imported (string vector)
load_packages <- function(x) {
  for (pkg in x) {
    if(!require(pkg, character.only = TRUE)){
      install.packages(pkg, )
      library(pkg)
    }
  }
}

#' Function for import groundwater level files from RIMAS/CPRM website
#'
#' Imports a text file downloaded directly from RIMAS/CPRM website
#' with gorundwater level information from monitoring and converts
#' into a data.frame object with two column: Data and Nivel
#'
#' @param file path to text file (string)
#'
#' @return groundwater levels and date (data.frame)
import_RIMAS <- function(file) {
  require("dplyr")

  # file <- file.gwl[1]
  # import as text
  x <- base::readLines(file, encoding = "latin1")
  x <- x[-which(x==";;;;;;")]
  x <- x[-which(x=="")]

  # header (n) e data (x)
  n <- "N"#úmero de sequencia do nível d'agua;Data da medição;Hora da medição;Nível da água;Vazão;Bombeando;"
  for (i in 1:length(x)) {
    if (base::substr(x[i],1,1)==n) {
      kn <- i
      break
    }
  }
  n <- x[kn]
  x <- x[(kn+1):length(x)]

  # separe by ";"
  sx <- base::strsplit(x, split = ";")
  nx <- base::unlist(strsplit(n, split = ";"))

  # create data frame
  xm <- data.frame(matrix(unlist(sx), nrow = length(sx), byrow = T))
  names(xm) <- nx
  len <- nrow(xm)

  # date column correct
  xm <- xm %>% dplyr::mutate(Data = as.Date(xm[,2], "%d/%m/%Y"))

  # level column correct
  xm <- xm %>% dplyr::mutate(Nivel = as.numeric(gsub(",",".",xm[,4])))
  xm <- xm %>% dplyr::group_by(Data) %>% dplyr::mutate(Nivel = mean(Nivel))

  # date regularization in a new data frame
  dti <- xm$Data[1]
  dtf <- xm$Data[len]
  df <- data.frame(Data = seq(as.Date(dti), as.Date(dtf), by = 1))
  # df <- suppressMessages(left_join(df, select(xm, Nivel), by = "Data"))
  df <- suppressMessages(df %>% dplyr::left_join(xm))
  df <- df %>% dplyr::select(Data, Nivel)
  # names(df) <- c("Data", "Nivel")

  # day, month and year columns
  df <- df %>% dplyr::mutate(Ano = as.numeric(substr(Data, 1, 4)), .before = Nivel)
  df <- df %>% dplyr::mutate(Mes = as.numeric(substr(Data, 6, 7)), .before = Nivel)
  df <- df %>% dplyr::mutate(Dia = as.numeric(substr(Data, 9, 10)), .before = Nivel)

  return(df)
}

#' Function for import rainfall files from FUNCEME website
#'
#' Imports a text file downloaded directly from FUNCEME main website
#' with rainfall information of pluviometric station and converts into
#' a data.frame with two columns: Data and Chuva
#'
#' @param file path to text file (string)
#'
#' @return a list containing a data.frame with rainfall and date,
#' and latitude and longitude as decimal geographic coordinates
import_FUNCEME <- function(file) {
  require("dplyr"); require("tidyr")

  # import text
  x <- readLines(file, encoding = "latin1")
  x <- x[-which(x=="")]

  # header (n) e data (x)
  n <- as.character("Posto")
  for (i in 1:length(x)) {
    if (substr(x[i],1,5)==n) {
      kn <- i
      break
    }
  }
  n <- x[kn]
  x <- x[(kn+1):length(x)]

  # separate by ","
  sx <- strsplit(x, split = ",")
  nx <- unlist(strsplit(n, split = ","))

  # create data frame
  xm <- data.frame(matrix(unlist(sx), nrow = length(sx), byrow = T))
  names(xm) <- nx
  len <- nrow(xm)

  # get geographic coordinates
  lat <- as.numeric(unique(xm$Latitude))
  lon <- as.numeric(unique(xm$Longitude))

  # converts to longitudinal data format
  pluvio <- tidyr::gather(xm[,-c(1,2,3,4)], dia, precip, Dia1:Dia31)
  names(pluvio) <- c("Ano", "Mes", "Dia", "Precip")
  pluvio <- pluvio %>% mutate(Dia = as.numeric(substr(Dia, 4, 5)))
  pluvio <- pluvio %>% mutate(Mes = as.numeric(Mes))
  pluvio <- pluvio %>% mutate(Ano = as.numeric(Ano))
  pluvio <- pluvio %>% arrange(Ano,Mes)
  pluvio <- pluvio %>% mutate(Data = paste0(sprintf("%02d", as.numeric(Dia)),"/",sprintf("%02d", as.numeric(Mes)),"/",Ano), .before = Ano)
  pluvio <- pluvio %>% mutate(Data = as.Date(Data, "%d/%m/%Y"))
  pluvio <- pluvio[!(pluvio$Precip == "*"),]
  pluvio$Precip[pluvio$Precip == "-"] <- NA
  pluvio <- pluvio %>% mutate(Precip = as.numeric(Precip))

  return(list(df = pluvio, latitude = lat, longitude = lon))
}

import_FUNCEME2 <- function(file) {
  require("dplyr"); require("tidyr")

  # file <- paste0("./man/chuva/mv2021/","298.txt")

  # import text
  x <- readLines(file, encoding = "latin1")

  # separate by ","
  sx <- strsplit(x, split = ";")

  # create data frame
  xm <- data.frame(matrix(unlist(sx), nrow = length(sx), byrow = T))
  names(xm) <- xm[1,]
  xm <- xm[-1,]
  len <- nrow(xm)

  # get geographic coordinates
  lat <- as.numeric(unique(xm$Latitude))
  lon <- as.numeric(unique(xm$Longitude))

  # data regularization in new data.frame
  pluvio <- data.frame(cbind(Ano=xm$Anos,Mes=xm$Meses,Precip=xm$Total))
  pluvio <- pluvio %>% mutate(AnoMes=as.Date(paste0(Ano,"/",Mes,"/1")), .before = Precip)

  dti <- paste0(xm$Anos[1],"/",xm$Meses[1],"/1")
  dtf <- paste0(xm$Anos[len],"/",xm$Meses[len],"/1")
  df <- data.frame(Data = seq(as.Date(dti), as.Date(dtf), by = "month"))
  df <- df %>% mutate(Ano = as.numeric(format(Data, "%Y")), Mes = as.numeric(format(Data, "%m"))); head(df)
  df <- suppressMessages(left_join(df, pluvio, by = c("Data"="AnoMes")))
  df <- df[,-c(1,4,5)]
  names(df) <- c("Ano", "Mes", "Precip")

  return(list(df = df, latitude = lat, longitude = lon))
}

import_FUNCEME3 <- function(file) {
  require("dplyr"); require("tidyr")

  # file <- paste0("./man/chuva/mv2021/","1.txt")

  # import text
  x <- readLines(file, encoding = "latin1")

  # separate by ","
  sx <- strsplit(x, split = ";")

  # create data frame
  xm <- data.frame(matrix(unlist(sx), nrow = length(sx), byrow = T))
  names(xm) <- xm[1,]
  xm <- xm[-1,]
  len <- nrow(xm)

  # get geographic coordinates
  lat <- as.numeric(unique(xm$Latitude))
  lon <- as.numeric(unique(xm$Longitude))

  # data regularization in new data.frame
  pluvio <- data.frame(cbind(Ano=xm$Anos,Mes=xm$Meses,Precip=xm$Total))
  # pluvio <- pluvio %>% mutate(AnoMes=as.Date(paste0(Ano,"-",Mes,"-1")), .before = Precip)

  # converts to longitudinal data format
  pluvio <- tidyr::gather(xm[,-c(1,2,3,4,7)], dia, precip, Dia1:Dia31)
  names(pluvio) <- c("Ano", "Mes", "Dia", "Precip")
  pluvio <- pluvio %>% mutate(Dia = as.numeric(substr(Dia, 4, 5)))
  pluvio <- pluvio %>% mutate(Mes = as.numeric(Mes))
  pluvio <- pluvio %>% mutate(Ano = as.numeric(Ano))
  pluvio <- pluvio %>% arrange(Ano,Mes)
  pluvio <- pluvio %>% mutate(Data = paste0(sprintf("%02d", as.numeric(Dia)),"/",sprintf("%02d", as.numeric(Mes)),"/",Ano), .before = Ano)
  pluvio <- pluvio %>% mutate(Data = as.Date(Data, "%d/%m/%Y"))
  pluvio <- pluvio[!(pluvio$Precip == "888.0"),]
  pluvio$Precip[pluvio$Precip == "999.0"] <- NA
  pluvio <- pluvio %>% mutate(Precip = as.numeric(Precip))

  # dti <- paste0(xm$Anos[1],"/",xm$Meses[1],"/1")
  # dtf <- paste0(xm$Anos[len],"/",xm$Meses[len],"/1")
  # df <- data.frame(Data = seq(as.Date(dti), as.Date(dtf), by = "month"))
  # df <- df %>% mutate(Ano = as.numeric(format(Data, "%Y")), Mes = as.numeric(format(Data, "%m"))); head(df)
  # df <- suppressMessages(left_join(df, pluvio, by = c("Data"="AnoMes")))
  # df <- df[,-c(1,4,5)]
  # names(df) <- c("Ano", "Mes", "Precip")

  return(list(df = pluvio, latitude = lat, longitude = lon))
}

#' Function for time series MinMax normalization
#'
#' @param x: array or ts object to be normalized
#' @param inf: lower boundary of normalization range
#' @param sup: upper boundary of normalization range
#'
#' @return normalized time series
norm_minmax <- function(x, inf, sup) {
  return(((x - min(x, na.rm = TRUE)) / (max(x, na.rm = TRUE) - min(x, na.rm = TRUE))) * (sup - inf) + inf)
}
norm_standard <- function(x) {
  return((x-mean(x))/sd(x))
}

#' Function for time series MinMax de-normalization
#'
#' @param x: array or ts object to be de-normalized
#' @param src: original data before previous normalization (array or ts object)
#' @param inf: lower boundary of normalization range
#' @param sup: upper boundary of normalization range
#'
#' @return de-normalized time series
denorm_minmax = function(x, src, inf, sup) {
  return(min(src, na.rm = TRUE) + (max(src, na.rm = TRUE) - min(src, na.rm = TRUE)) * ((x - inf)/(sup - inf)))
}
denorm_standard <- function(x, src) {
  return(x*sd(src) + mean(src))
}
denorm_diff <- function(x, src, diff) {
  return(x+src[diff:length(src)-diff])
}
denorm_diff2 <- function(x, src, diff) {
  res <- c()
  res[1] <- src[1]
  for(i in 1:(length(src)-1)) {
    res[i+1] <- sum(c(src[diff], x[diff:i]))
  }
  return(res)
}

#' Function for lagged time series matrix
#'
#' @param x: array ou ts object
#' @param lag: number of lag values e.g. y_{t-lag}
#'
#' @return lag matrix (data.frame)
lag_matrix = function(x, lag) {
  ps <- tsp(x)
  yt <- x

  mt <- cbind(yt)

  for (i in 1:lag) {
    mt <- cbind(mt, stats::lag(yt, -i))
  }
  colnames(mt) <- c("yt", paste0("yt_",c(1:lag)))

  df <- data.frame(mt[-seq(length(x)+1, length.out = lag),])
  return(df)
}

# MESOR ----
getMinimalVariancePredictor = function(residuals, forecasts, target, from, to){
  # print(paste("*************", "MINIMAL VARIANCE COMBINATION", "*************"))
  modellingTime = Sys.time()#proc.time()[[3]]
  nSingleModels = ncol(forecasts)
  covMatrix=cov(residuals[from:to, ], use = "pairwise.complete.obs")
  invCovMatrix = tryCatch({
    # expr = inv(covMatrix)},
    expr = base::solve(covMatrix)},
    error = function(e){
      message("Error: inv(covMatrix). Approaching via ginv(covMatrix)");
      return(ginv(covMatrix))
    }
    #,finally={message(paste("Error: ginv(covMatrix)")); return(NA)}
  )

  #print(covMatrix%*%invCovMatrix)
  weigths = numeric(nSingleModels)
  for(i in 1:nSingleModels){
    weigths[i] = sum(invCovMatrix[i,])
  }
  sw=sum(weigths)
  weigths = weigths/sw
  modellingTime = Sys.time() - modellingTime#proc.time()[[3]] - modellingTime#time in seconds
  forecastingTime = Sys.time()#proc.time()[[3]]
  mvForecasts = weigths[1]*forecasts[,1]
  for(i in 2:nSingleModels){
    mvForecasts = mvForecasts + weigths[i]*forecasts[,i]
  }
  forecastingTime = Sys.time() - forecastingTime#proc.time()[[3]] - forecastingTime
  modellingTime = as.numeric(modellingTime)
  forecastingTime = as.numeric(forecastingTime)
  weigths = as.list(weigths)
  names(weigths) = colnames(forecasts)
  optimal = list()
  optimal$modellingTime = modellingTime
  optimal$forecastingTime = forecastingTime
  optimal$forecasts = mvForecasts
  optimal$weigths = weigths
  optimal$cov = covMatrix

  optimal$coefficients = weigths
  optimal$residuals = mvForecasts - target
  optimal$model = forecasts
  return(optimal)
}

get_cMvNewForecasts = function(ModelsObjs, singleForecasts){
  forecastingTime = Sys.time()#proc.time()[[3]]
  singleForecasts = singleForecasts
  cMVobj = ModelsObjs#$opt.c.MvObj
  w = cMVobj$weigths
  nModels = length(w)
  nmModels = names(w)
  forecasts = 0
  for(i in 1:nModels){
    nmModel_i = nmModels[i]
    forecasts = forecasts + w[[nmModel_i]]*singleForecasts[,which(colnames(singleForecasts)==nmModel_i)]
  }
  forecastingTime = Sys.time() - forecastingTime#proc.time()[[3]] - forecastingTime
  ret = list()
  ret$forecastingTime = as.numeric(forecastingTime)
  ret$forecasts = forecasts
  return (ret)
}

getSimpleAverageForecasts = function(forecasts){
  t1 <- Sys.time()
  pred <- apply(forecasts, 1, mean)
  duration <- as.numeric(Sys.time() - t1)

  return(list('forecasts' = pred, 'forecasting.time' = duration))
}
getSimpleMedianForecasts = function(forecasts){#forecasts=single.forecasts
  t1 <- Sys.time()
  pred <- apply(forecasts, 1, quantile, 0.5)
  duration <- as.numeric(Sys.time() - t1)

  return(list('forecasts' = pred, 'forecasting.time' = duration))
}
getMPD_viaInformationCriterion = function(e){
  require("rmutil")
  require("fGarch")
  require("copula")
  require("moments")
  require("MASS")

  ret = list()
  ret$residuals = e
  #e = e[!is.na(e)]
  n = length(e); k = log(n)

  margins = NULL; logLiks = NULL; nPars = NULL; BICs = NULL; parsEstimates = list(); i=0

  #SYMMETRICAL PDFs
  margins = c(margins, "norm")
  i=i+1; parsEstimates[[i]] = list(mean=mean(e, na.rm = TRUE), sd = sd(e, na.rm = TRUE))
  logLiks = c(logLiks, sum(dnorm(x = e, mean = parsEstimates[[i]]$mean, sd = parsEstimates[[i]]$sd, log=TRUE), na.rm = TRUE))

  margins = c(margins, "cauchy")
  location = median(e, na.rm = TRUE)
  q = quantile(e, probs = c(.25, .75), na.rm = TRUE)
  scale = as.numeric(q[2]-q[1])/2
  i=i+1; parsEstimates[[i]] =  list(location=location, scale = scale)
  logLiks = c(logLiks, sum(dcauchy(x = e, location = parsEstimates[[i]]$location, scale = parsEstimates[[i]]$scale, log=TRUE), na.rm = TRUE))

  margins = c(margins, "laplace")
  m = median(e, na.rm = TRUE)
  s = sd(e, na.rm = TRUE)/sqrt(2)
  i=i+1; parsEstimates[[i]] =  list(m = m, s = s)
  logLiks = c(logLiks, sum(dlaplace(y = e, m = parsEstimates[[i]]$m, s = parsEstimates[[i]]$s, log=TRUE), na.rm = TRUE))

  #ASYMMETRICAL PDFs

  margins = c(margins, "snorm")
  fit = snormFit(e[which(!is.na(e))])
  mean = fit$par[1]; sd = fit$par[2]; xi = fit$par[3]
  i=i+1; parsEstimates[[i]] = list(mean=mean, sd = sd, xi = xi)
  logLiks = c(logLiks, sum(dsnorm(x = e, mean = parsEstimates[[i]]$mean, sd = parsEstimates[[i]]$sd, xi = parsEstimates[[i]]$xi, log=TRUE), na.rm = TRUE))

  # TAKING THE BEST BIC FIT
  minBIC = Inf; minBIC_index = NULL; nFits = length(margins)
  for(i in 1:nFits){
    nPars[i] = length(parsEstimates[[i]])
    BICs[i] = -2*logLiks[i] + k*nPars[i]
    if(BICs[i] < minBIC){
      minBIC = BICs[i]; minBIC_index = i
    }
  }
  ret$nPars = nPars[minBIC_index]
  ret$BIC = BICs[minBIC_index]
  ret$margin = margins[minBIC_index]
  ret$parEstimates = parsEstimates[[minBIC_index]]
  ret$loglik = logLiks[minBIC_index]

  # rocj: get BIC values from all models
  names(BICs) <- margins
  ret$trash <- BICs

  #TAKING THE BEST BIC FIT CDF RESIDUALS POINTS
  if(ret$margin=="norm"){
    ret$CDF = function(x){return(pnorm(x, mean=ret$parEstimates[[1]], sd=ret$parEstimates[[2]]))}
    ret$pdf = function(x){return(dnorm(x = x, mean = ret$parEstimates[[1]], sd = ret$parEstimates[[2]]))}
  }
  else if(ret$margin=="cauchy"){
    ret$CDF = function(x){return(pcauchy(x, location=ret$parEstimates[[1]], scale=ret$parEstimates[[2]]))}
    ret$pdf = function(x){return(dcauchy(x = x, location=ret$parEstimates[[1]], scale=ret$parEstimates[[2]]))}
  }
  else if(ret$margin=="laplace"){
    ret$CDF = function(x){return(plaplace(x, m=ret$parEstimates[[1]], s=ret$parEstimates[[2]]))}
    ret$pdf = function(x){return(dlaplace(x, m=ret$parEstimates[[1]], s=ret$parEstimates[[2]]))}
  }
  else if(ret$margin=="snorm"){
    ret$CDF = function(x){return(psnorm(x, mean=mean, sd = sd, xi = xi))}
    ret$pdf = function(x){return(dsnorm(x = x, mean = ret$parEstimates[[1]], sd = ret$parEstimates[[2]], xi = ret$parEstimates[[3]]))}
  }
  return(ret)
}

getCopulaBasedPredictor = function(targets = target.all.norm,
                                   forecasts=single.forecasts,
                                   from=(n+1), to=(n+v)){
  # targets <- y
  # forecasts <- cbind(mod.arima$fitted, mod.ets$fitted)

  # print(paste("*************", seriesName, ": COPULA-BASED COMBINATION", "*************"))
  modellingTime = Sys.time()
  nSingleModels = ncol(forecasts)#nSingleModels=3
  residuals = targets - forecasts
  #pairsFigure(residuals[from:to,], seriesName)#paste(seriesName, "withOutliers", sep="."))
  copulaFamily = list(Empirical = "Empirical", Archimedean= "Archimedean", Elliptical = "Elliptical", Extreme_Value = "Extreme_Value")
  copulaDists = list(); copulaDists.initPar = list(); copulaDists.nPar = list();

  copulaDists[[copulaFamily$Empirical]]=        c("rankCacoullos")#, "cvtCacoullos", "Cacoullos")
  copulaDists.initPar[[copulaFamily$Empirical]]=c(1)
  copulaDists.nPar[[copulaFamily$Empirical]]=   rep(1, length(copulaDists.initPar$Empirical))

  copulaDists[[copulaFamily$Archimedean]]=        c("clayton", "frank", "gumbel", "joe")#, "amh"
  copulaDists.initPar[[copulaFamily$Archimedean]]=c(1,         1,        2,        2)#   .5,
  copulaDists.nPar[[copulaFamily$Archimedean]]=   rep(1, length(copulaDists.initPar$Archimedean))

  copulaDists[[copulaFamily$Elliptical]]=         c("normal")#, "t")
  copulaDists.initPar[[copulaFamily$Elliptical]]= c(.5)#,       .5)
  copulaDists.nPar[[copulaFamily$Elliptical]]=   rep((nSingleModels*(nSingleModels-1)/2), length(copulaDists.initPar$Elliptical))

  copulaDists[[copulaFamily$Extreme_Value]]= NULL#c("tawn", "huslerReiss", "galambos", "tev")
  copulaDists.initPar[[copulaFamily$Extreme_Value]]= c(.5,       .5)
  copulaDists.nPar[[copulaFamily$Extreme_Value]]=   rep(2, length(copulaDists.initPar$Extreme_Value))

  c.copulaDists = c(copulaDists$Empirical, copulaDists$Archimedean, copulaDists$Elliptical, copulaDists$Extreme_Value)
  c.copulaDists.initPar = c(copulaDists.initPar$Empirical, copulaDists.initPar$Archimedean, copulaDists.initPar$Elliptical, copulaDists.initPar$Extreme_Value)
  c.copulaDists.nPar = c(copulaDists.nPar$Empirical, copulaDists.nPar$Archimedean, copulaDists.nPar$Elliptical, copulaDists.nPar$Extreme_Value)
  c.family = c(rep(copulaFamily$Empirical, length(copulaDists$Empirical))
               , rep(copulaFamily$Archimedean, length(copulaDists$Archimedean))
               , rep(copulaFamily$Elliptical, length(copulaDists$Elliptical))
               , rep(copulaFamily$Extreme_Value, length(copulaDists$Extreme_Value)))

  modelLabels = dimnames(forecasts)[[2]]
  margins = NULL; paramMargins = list(); CDFs = NULL; MPDs = list(); Errors = NULL
  outliersIndexes = NULL
  NA_indexes = NULL
  # from <- 1; to <- length(y)
  for(i in 1:nSingleModels){
    # i <- 2
    modelLabel = modelLabels[i]
    e = residuals[from:to,i]
    # NA_indexes_i = which(is.na(e))
    # outliersIndexes_i = getOutliersIndexes(e)
    # e[outliersIndexes_i] = NA
    # outliersIndexes = union(outliersIndexes, outliersIndexes_i)
    # NA_indexes = union(NA_indexes, c(NA_indexes_i, outliersIndexes_i))
    MPD = getMPD_viaInformationCriterion(e)
    #histFigure(data=e, MPD, modelName=modelLabel, seriesName)
    margins = c(margins, MPD$margin)
    paramMargins[[i]] = MPD$parEstimates
    CDFs = cbind(CDFs, MPD$CDF(e))
    Errors = cbind(Errors, MPD$residuals)
    MPDs[[modelLabel]] = MPD
  }
  # if(length(NA_indexes)>0){#eventual outliers are also removed (they were set NA in the for(...))
  #   Errors = Errors[-NA_indexes, ]
  #   CDFs = CDFs[-NA_indexes, ]
  # }
  # if(length(outliersIndexes)>0){
  #   # dimnames(Errors) = list(NULL, modelLabels)
  #   Errors = Errors[-outliersIndexes, ]
  #   CDFs = CDFs[-outliersIndexes, ]
  #   # pairsFigure(as.data.frame(Errors), paste(seriesName, "withoutOutliers", sep="."))
  # }

  getOptimalMvdc = function(Errors, CDFs, margins, paramMargins){
    nSingleModels = dim(Errors)[2]
    n = dim(Errors)[1]
    mvdc_ = NULL
    BIC = Inf
    bestCopulaDist = NULL
    nCopulaDists = length(c.copulaDists)
    for(i in 1:nCopulaDists){#i=6
      loglike = -Inf; nPar = Inf
      copulaDist = c.copulaDists[i]
      nPars = c.copulaDists.nPar[i]
      initPars = rep(c.copulaDists.initPar[i], nPars)
      family = c.family[i]
      copula = NULL;
      if (family==copulaFamily$Empirical) {}
      else {
        # trash = function(){
        # }
        if (family == copulaFamily$Elliptical) {
          copula = ellipCopula(
            copulaDist,
            param = initPars,
            dim = nSingleModels,
            dispstr = "un")#, df = ((n-1)^nSingleModels))
        } else if (family==copulaFamily$Archimedean) {
          copula = archmCopula(
            copulaDist,
            param = initPars,
            dim = nSingleModels)
        } else if (family==copulaFamily$Extreme_Value) {
          copula = evCopula(
            copulaDist,
            param = initPars,
            dim = nSingleModels)
        }
        mvdc_i = mvdc(copula = copula, margins = margins, paramMargins = paramMargins)
        fitIfm = tryCatch(
          fitCopula(mvdc_i@copula, CDFs, method="ml")
          , error = function(e){return(NULL)})#, finally=function(){return(NULL)})
        #properties_fitIfm = summary(fitIfm); #IfmCoefficients = properties_fitIfm$coefficients[1]
        if(!is.null(fitIfm)){
          IfmCoefficients = coef(fitIfm)
          if(family==copulaFamily$Elliptical){
            copula = ellipCopula(copulaDist, param=IfmCoefficients, dim=nSingleModels, dispstr="un")#, df = ((n-1)^nSingleModels))
          } else if(family==copulaFamily$Archimedean){
            copula = archmCopula(copulaDist, param=IfmCoefficients, dim=nSingleModels)
          } else if(family==copulaFamily$Extreme_Value){
            copula = evCopula(copulaDist, param=IfmCoefficients, dim=nSingleModels)
          }
          mvdc_i = mvdc(copula = copula, margins = margins, paramMargins = paramMargins)
          loglike = fitIfm@loglik
          nPar = length(IfmCoefficients)
        }
        #     }
      }
      BIC_i = -2*(loglike) + nPar*log(n)
      if((BIC_i < BIC) & is.finite(BIC_i)){
        BIC = BIC_i
        mvdc_ = mvdc_i
        bestCopulaDist = copulaDist
      }
    }
    ret = list(bestMvdc= mvdc_, copulaDist = bestCopulaDist)
    return(ret)
  }
  mvdc = getOptimalMvdc(Errors, CDFs, margins, paramMargins)
  modellingTime = Sys.time() - modellingTime#time in seconds

  uCombined = numeric(length(targets))
  all.data = cbind(targets, forecasts)
  bounds = c(min(all.data, na.rm = TRUE), max(all.data, na.rm = TRUE))
  getCBForecast = function(mvdc, forecasts_t, bounds){
    bestEstimate = NA
    # if(length(which(is.na(forecasts_t)))>0){
    #   return (bestEstimate)
    # }
    # getOptimumViaEvenlySpacedPoints = function(){
      nPoints = 1000
      x = seq(from=bounds[1], to=bounds[2],length.out=nPoints)
      nModels = length(forecasts_t)
      correctedEst = NULL
      for(i in 1:nModels){
        modelLabel_i = names(forecasts_t)[i]#modelLabels[i]
        errors = x - rep(forecasts_t[[modelLabel_i]], nPoints)
        # errors = x - rep(forecasts_t[[i]], nPoints)
        correctedEst = cbind(correctedEst, errors)
      }
      dimnames(correctedEst) = list(round(x, 4), modelLabels)
      # dimnames(correctedEst) = list(round(x, 4), colnames(forecasts_t))
      #maybe, there is a problem with my call to dMvdc(.)....
      fx = dMvdc(correctedEst, mvdc, log=TRUE)#;plot(x, fx, type="l")
      s = which.max(fx)
      bestEstimate = x[s]
      # return(bestEstimate)
    # }
    # bestEstimate = getOptimumViaEvenlySpacedPoints()
    return(bestEstimate)
  }
  n_v_m = length(targets)
  forecastingTime = Sys.time()
  for(t in 1:(n_v_m)){
    uCombined[t] = getCBForecast(mvdc$bestMvdc, forecasts_t= forecasts[t,], bounds)
  }
  forecastingTime = Sys.time() - forecastingTime

  modellingTime = as.numeric(modellingTime)
  forecastingTime = as.numeric(forecastingTime)

  optimal = list()
  optimal$modellingTime = modellingTime
  optimal$forecastingTime = forecastingTime
  optimal$model = list(MVDC = mvdc$bestMvdc, MPDs = MPDs, combinedModelsNames = modelLabels)
  optimal$forecasts = uCombined
  optimal$modelingBounds = bounds
  optimal$getCBForecast = getCBForecast
  return(optimal)
}

get_cCbNewForecasts = function(ModelsObjs, singleForecasts) {
  # ModelsObjs <- mod.ccp
  # singleForecasts <- cbind(fct.arima,fct.ets)
  opt_cCb = ModelsObjs
  # cANN one-step forecast  (fixed model and updating time series)
  # forecastingTime = proc.time()[[3]]
  newDataMatrix = singleForecasts
  # series.norm = getNormalizedSeries(series = newDataMatrix
  #                                   , min = ModelsObjs$superMinimum
  #                                   , max = ModelsObjs$superMaximum)
  forecastingTime = Sys.time()

  # forecasts.series.norm = opt_cCb$getCBForecast(
  #   mvdc = opt_cCb$model$MVDC, forecasts_t=newDataMatrix
  #   , bounds = opt_cCb$modelingBounds)

  forecasts.series.norm <- c()
  for(i in 1:nrow(newDataMatrix)) {
    forecasts.series.norm[i] <- opt_cCb$getCBForecast(
        mvdc = opt_cCb$model$MVDC, forecasts_t=newDataMatrix[i,]
        , bounds = opt_cCb$modelingBounds)
  }

  # forecasts = getDenormalizedSeries(dataset.norm = forecasts.series.norm
  #                                   , min = ModelsObjs$superMinimum
  #                                   , max = ModelsObjs$superMaximum)
  forecastingTime = Sys.time() - forecastingTime
  # forecastsData = cbind(new_u = forecasts, newDataMatrix)
  # forecastsData = as.data.frame(t(forecastsData))#;View(forecastsData)

  ###
  ret = list()
  ret$forecastingTime = as.numeric(forecastingTime)
  ret$forecasts = forecasts.series.norm
  # ret$forecastsData = forecastsData
  return (ret)
}

#' Function for lag matrix from a time series
#'
#' @param x ts object
#' @param lags vector with index of lags to be computed. Use positive
#' (negative) values for get past (future) lags
#'
#' @return data frame
#' @export
#'
#' @examples
lag_matrix2 <- function(x, lags) {
  cll <- paste0("cbind(x, ", paste0("stats::lag(x, -", lags,")", collapse = ", "), ")")
  df <- eval(parse(text = cll))
  colnames(df) <- c("x", paste0("x.",lags))
  return(df)
}

compose_lags_combn <- function(lags.max) {
  require("arrangements")
  lags.option <- list()
  t1 <- Sys.time()
  for(i in 1:lags.max) {
    # lags.option[[i]] <- t(combn(lags.max, i))
    lags.option[[i]] <- arrangements::combinations(lags.max, i)
  }
  t2 <- Sys.time()
  lags.option <- lapply(lags.option, function(k) split(k, seq(nrow(k))))
  lags.option <- unlist(lags.option, recursive = FALSE)

  # Slower computation time, equals to sum(choose(lags.max, i))
  # lags.tot <- lags.tot + choose(lags.max, i)
  lags.tot <- length(lags.option)
  return(list(lags.option = lags.option, lags.tot = lags.tot,
              elapsed.time = (t2-t1)))
}

compose_lags <- function(y, p, P) {
  m <- max(round(frequency(y)), 1L)
  if (P > 0) {
    lags <- sort(unique(c(1:p, m * (1:P))))
  }
  else {
    lags <- 1:p
  }
  return(lags)
}
