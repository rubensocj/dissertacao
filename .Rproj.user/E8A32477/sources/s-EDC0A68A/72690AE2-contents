#' @author: Rubens O. da Cunha Júnior

rm(list = ls())
cat('\014')
set.seed(1)

source(file = './r/functions.R')
source(file = './r/metrics.R')
source(file = './r/models.R')

load_packages(c('forecast','dplyr','imputeTS','tsfeatures','xtable','MASS',
                'corrplot','elmNNRcpp','keras','tensorflow'))

path.gwl <- './man/nivel/todos/'
path.results <- './man/results/main/'

# Input parameters ----
well <- 4
min.scale <- -1; max.scale <- 1
ml.par <- list('p' = 5, 'P' = 3, 'nodes.max' = 40, 'units.max' = 40,
               'layers.max' = 4, 'lstm.layers.max' = 1, 'epochs' = 400,
               'pmutation' = 0.1, 'pcrossover' = 0.9, 'max.iter' = 50,
               'popSize' = 10)

# Import ----
temp.gwl <- list.files(path = path.gwl, pattern = '*.csv')
file.gwl <- paste0(path.gwl, temp.gwl)
file.gwl <- lapply(file.gwl, import_RIMAS)
names(file.gwl) <- substring(temp.gwl, 1, nchar(temp.gwl[1])-4)

# Time series objects ----
# get average monthly GWL time series
ls.gwl.average <- suppressMessages(lapply(file.gwl, function(y) {
  return(y %>% group_by(Ano, Mes) %>% summarise(NivelMedio = mean(Nivel)))
}))

# list with all GWL time series
ls.gwl.raw <- lapply(ls.gwl.average, function(y) {
  return(ts(as.numeric(y$NivelMedio),
            start = c(y$Ano[1], y$Mes[1]),
            end = c(y$Ano[nrow(y)], y$Mes[nrow(y)]),
            frequency = 12))})

rename.gwl <- paste0("P",sprintf("%02d", 1:21))

# Missing values (MV) imputation
ls.gwl <- lapply(ls.gwl.raw, imputeTS::na_seadec)

# Target time series
ts.gwl <- ls.gwl[[well]]

# Training, validation and test ----
len.tes <- 12; len.val <- 12
len.tra.val <- length(ts.gwl) - len.tes
len.tra <- len.tra.val - len.val
ts.tra.src <- head(ts.gwl, len.tra.val)
ts.tes.src <- tail(ts.gwl, len.tes)

# Normalization ----
ts.tra.minmax <- norm_minmax(ts.tra.src, min.scale, max.scale)

# Training and test set ----
y <- ts.tra.minmax
y.tes <- ts.tes.src

# ARIMA ----
mod.ARIMA <- GWL_ts_model(y = y, type = 'arima',
                          nmodels = ml.par$popSize*ml.par$max.iter)
fct.ARIMA <- GWL_ts_forecast(mod.ARIMA, h = len.tes)

# ETS ----
mod.ETS <- GWL_ts_model(y = y, type = 'ets')
fct.ETS <- GWL_ts_forecast(mod.ETS, h = len.tes)

# SVR ----
mod.SVR <- ga_SVR(
  y = y,
  p = ml.par$p,
  P = ml.par$P,
  training = len.tra,
  xreg = NULL,
  maxiter = ml.par$max.iter,
  popSize = ml.par$popSize,
  pmutation = ml.par$pmutation,
  pcrossover = ml.par$pcrossover,
  monitor = TRUE
)
fct.SVR <- GWL_ts_forecast(mod.SVR$model, h = len.tes)

# ANN ----
mod.ANN <- ga_ANN(
  y = y,
  p = ml.par$p,
  P = ml.par$P,
  hid.max = ml.par$nodes.max,
  layer.max = ml.par$layers.max,
  training = len.tra,
  xreg = NULL,
  maxiter = ml.par$max.iter,
  popSize = ml.par$popSize,
  pmutation = ml.par$pmutation,
  pcrossover = ml.par$pcrossover,
  monitor = TRUE
)
fct.ANN <- GWL_ts_forecast(mod.ANN$model, h = len.tes)

# ELM ----
mod.ELM <- ga_ELM(
  y = y,
  p = ml.par$p,
  P = ml.par$P,
  hid.max = ml.par$units.max,
  training = len.tra,
  xreg = NULL,
  maxiter = ml.par$max.iter,
  popSize = ml.par$popSize,
  pmutation = ml.par$pmutation,
  pcrossover = ml.par$pcrossover,
  monitor = TRUE
  )
fct.ELM <- GWL_ts_forecast(mod.ELM$model, h = len.tes)

# LSTM ----
# Candidate 01
mod.LSTM01 <- GWL_ts_model(
  y = y,
  type = 'lstm',
  lags = mod.ANN$model$lags,
  hidden = mod.ANN$hidden[1],
  epochs = ml.par$epochs,
  learningrate = 0.001,
  from = 1,
  to = length(y)
)
fct.LSTM01 <- GWL_ts_forecast(object = mod.LSTM01, h = len.tes)

# Candidate 02
mod.LSTM02 <- GWL_ts_model(
  y = y,
  type = 'lstm',
  lags = mod.ELM$model$lags,
  hidden = mod.ELM$hidden,
  epochs = ml.par$epochs,
  learningrate = 0.001,
  from = 1,
  to = length(y)
)
fct.LSTM02 <- GWL_ts_forecast(object = mod.LSTM02, h = len.tes)

# Select best LSTM
rmse.LSTM01 <- rmse(as.numeric(denorm_minmax(
  fct.LSTM01$forecasts, ts.tra.src, min.scale, max.scale)),
                    as.numeric(y.tes))
rmse.LSTM02 <- rmse(as.numeric(denorm_minmax(
  fct.LSTM02$forecasts, ts.tra.src, min.scale, max.scale)),
                    as.numeric(y.tes))
if(rmse.LSTM01 < rmse.LSTM02) {
  mod.LSTM <- mod.LSTM01
  fct.LSTM <- fct.LSTM01
} else {
  mod.LSTM <- mod.LSTM02
  fct.LSTM <- fct.LSTM02
}

# Individual models ----
fct.SINGLE <- cbind(
  'ARIMA' = fct.ARIMA$forecasts,
  'ETS' = fct.ETS$forecasts,
  'ANN' = fct.ANN$forecasts,
  'SVR' = fct.SVR$forecasts,
  'ELM' = fct.ELM$forecasts,
  'LSTM' = fct.LSTM$forecasts
)
fit.SINGLE <- cbind(
  'ARIMA' = mod.ARIMA$fitted,
  'ETS' = mod.ETS$fitted,
  'ANN' = mod.ANN$model$fitted,
  'SVR' = mod.SVR$model$fitted,
  'ELM' = mod.ELM$model$fitted,
  'LSTM' = mod.LSTM$fitted
); fit.SINGLE <- na.omit(fit.SINGLE)

# SA combination ----
fit.cSA <- getSimpleAverageForecasts(fit.SINGLE)
fct.cSA <- getSimpleAverageForecasts(fct.SINGLE)

# SM combination ----
fit.cSM <- getSimpleMedianForecasts(fit.SINGLE)
fct.cSM <- getSimpleMedianForecasts(fct.SINGLE)

# ANN combination ----
mod.cANN <- ga_cANN(
  y = tail(y, nrow(fit.SINGLE)),
  training = nrow(fit.SINGLE)-len.val,
  hid.max = ml.par$nodes.max,
  layer.max = ml.par$layers.max,
  xreg = fit.SINGLE,
  maxiter = ml.par$max.iter,
  popSize = ml.par$popSize,
  pmutation = ml.par$pmutation,
  pcrossover = ml.par$pcrossover,
  monitor = TRUE
)
fct.cANN <- stats::predict(mod.cANN$model$model, newdata = fct.SINGLE)

# SVR combination ----
mod.cSVR <- ga_cSVR(
  y = tail(y, nrow(fit.SINGLE)),
  training = nrow(fit.SINGLE)-len.val,
  xreg = fit.SINGLE,
  maxiter = ml.par$max.iter,
  popSize = ml.par$popSize,
  pmutation = ml.par$pmutation,
  pcrossover = ml.par$pcrossover,
  monitor = TRUE
)
fct.cSVR <- stats::predict(mod.cSVR$model$model, newdata = fct.SINGLE)

# MV combination ----
mod.cMV <- getMinimalVariancePredictor(
  residuals = y - fit.SINGLE,
  forecasts = fit.SINGLE,
  target = tail(y, nrow(fit.SINGLE)),
  from = 1,
  to = nrow(fit.SINGLE)
)
fct.cMV <- get_cMvNewForecasts(mod.cMV, fct.SINGLE)

# Copulas-based combination ----
mod.cCP <- getCopulaBasedPredictor(
  targets = tail(y, nrow(fit.SINGLE)),
  forecasts = fit.SINGLE,
  from = 1,
  to = nrow(fit.SINGLE)
)
fct.cCP <- get_cCbNewForecasts(mod.cCP, fct.SINGLE)

# All models ----
# Test set
fct.MODELS <- cbind(
  fct.SINGLE,
  'cSA' = fct.cSA$forecasts,
  'cSM' = fct.cSM$forecasts,
  'cANN' = fct.cANN,
  'cSVR' = fct.cSVR,
  'cMV' = fct.cMV$forecasts,
  'cCP' = fct.cCP$forecasts
); colnames(fct.MODELS)[1:ncol(fct.SINGLE)] <- colnames(fct.SINGLE)

# Training set
fit.MODELS <- cbind(
  fit.SINGLE,
  'cSA' = fit.cSA$forecasts,
  'cSM' = fit.cSM$forecasts,
  'cANN' = as.numeric(mod.cANN$model$fitted),
  'cSVR' = as.numeric(mod.cSVR$model$fitted),
  'cMV' = mod.cMV$forecasts,
  'cCP' = mod.cCP$forecasts
); colnames(fit.MODELS)[1:ncol(fit.SINGLE)] <- colnames(fit.SINGLE)

# Transform back ----
# Min-Max
fit.MODELS.src <- apply(fit.MODELS, 2, denorm_minmax, ts.tra.src, min.scale, max.scale)
fct.MODELS.src <- apply(fct.MODELS, 2, denorm_minmax, ts.tra.src, min.scale, max.scale)

y.tes.src <- y.tes

# Forecasting Accuracy ----
methods <- c('ARIMA','ETS',
             'ANN','SVR','ELM',
             'LSTM','cSA','cSM',
             'cANN','cSVR',
             'cMV','cCP')
res.fct <- data.frame(t(apply(fct.MODELS.src, 2, function(f) {
  cbind(
    round(rmse(f, y.tes.src),4),
    round(mae(f, y.tes.src),4),
    round(100*mape(f, y.tes.src),4),
    round(getTheil(y.tes.src, f),4),
    round(coef.det(f, y.tes.src),4),
    round(getPOCID(y.tes.src, f),2))
}))); colnames(res.fct) <- c('RMSE','MAE','MAPE','U de Theil','R$^2$','POCID')
res.fct <- cbind('Modelo' = methods, res.fct)

# Plot forecasts ----
point <- c(1,2,3,4,5,6,
           8,0,20,6,3,4)
lin <- c(2,3,4,5,6,2,
         2,3,4,5,6,2)
col <- c('#e41a1c','#377eb8','#4daf4a','#984ea3','#ff7f00','#a65628',
         '#1b9e77','#d95f02','#7570b3','#e7298a','#66a61e','#e6ab02')
y.lab <- c('Nível (m)')

par(cex.axis = 0.7, xpd = TRUE, mgp = c(0.05,0.2,0), tck = -0.02,
    mar = c(2,2,1,3)+0.1, mfrow = c(2,1))

# Single
ts.plot(
  cbind(res.MODELS.src[,c(2:7)]),
  ylim = c(max(res.MODELS.src),min(res.MODELS.src)),
  xlab = '',
  ylab = '',
  type = 'o',
  lty = lin[1:6],
  lwd = 0.8,
  gpars = list(pch = point[1:6],
               cex = 0.3,
               col = col[1:6])
)
lines(res.MODELS.src[,1], lwd = 0.8)
abline(v = time(ts.gwl)[len.tra.val], xpd = FALSE,
       lty = 2, lwd = 0.8, col = 'darkgray')
mtext(text = y.lab, side = 2, line = 1, cex = 0.7)
mtext(text = "Tempo", side = 1, line = 1, cex = 0.7)
mtext(text = "a", side = 3, cex = 0.8, font = 2, line = 0, adj = 0.02)
legend(
  'topright',
  legend = c(rename.gwl[[well]],methods[1:6]),
  cex = 0.6,
  bty = 'n',
  inset = c(-0.12,0),
  x.intersp = 0.5,
  seg.len = 1.5,
  col = c('black',col[1:6]),
  pch = c(0, point[1:6]),
  lty = c(1, lin[1:6]),
  pt.cex = c(0, rep(0.3,6)),
  lwd = c(0.8, rep(0.8,6)),
  xjust = 0,
  yjust = 0
)

# Combined
ts.plot(
  cbind(res.MODELS.src[,c(8:13)]),
  ylim = c(max(res.MODELS.src), min(res.MODELS.src)),
  xlab = '',
  ylab = '',
  type = 'o',
  lwd = 0.8,
  gpars = list(pch = point[7:12],
               cex = 0.3,
               col = col[7:12],
               lty = lin[7:12])
)
lines(res.MODELS.src[,1], lwd = 0.8)
abline(v = time(ts.gwl)[len.tra.val], xpd = FALSE,
       lty = 2, lwd = 0.8, col = 'darkgray')
mtext(text = y.lab, side = 2, line = 1, cex = 0.7)
mtext(text = "Tempo", side = 1, line = 1, cex = 0.7)
mtext(text = "b", side = 3, cex = 0.8, font = 2, line = 0, adj = 0.02)
legend(
  'topright',
  legend = c(rename.gwl[[well]],methods[7:12]),
  cex = 0.6,
  bty = 'n',
  inset = c(-0.11,0),
  x.intersp = 0.5,
  seg.len = 1.5,
  col = c('black',col[7:12]),
  pch = c(0, point[7:12]),
  lty = c(1, lin[7:12]),
  pt.cex = c(0, rep(0.3,6)),
  lwd = c(0.8, rep(0.8,6)),
  xjust = 0,
  yjust = 0
)