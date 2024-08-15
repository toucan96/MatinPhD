ifnConc <- function(data, standard){
  std <- drc::drm(formula = absorbance ~ concentration, data = subset(standard, concentration > 0), fct = drc::LL.4())
  prediction <- drc::ED(std, respLev = data$absorbance, type = "absolute", display = FALSE)
  newplot <- plot(std)
  newList <- list(
    standard = std,
    standard.plot = newplot,
    prediction = prediction,
    output = cbind(
      data,
      ifn = prediction[,1]
    )
  )
  return(newList)
}
