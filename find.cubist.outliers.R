######################################
### author: John Boudreaux ###########
### date: 2018-01-12 #################
######################################

require(Cubist)
require(dprep)

find.cubist.outliers <- function(cubist.model, prediction.data){
  ###step 0: basic error catching
  if(class(cubist.model) != "cubist"){
    stop("Must input cubist model")
  }
  if(class(prediction.data) != "data.frame"){
    stop("input data must be data frame")
  }
  if(colnames(prediction.data) != cubist.model$vars$all){
    stop("prediction data names do not match cubist parameters")
  }
  
  ###step 1: reconstruct training data frame
  data.strings <- strsplit(cubist.model$data, "\\n")
  all.data <- lapply(data.strings, function(x){strsplit(x, ",")})
  all.data <- t(as.data.frame(all.data))
  rownames(all.data) <- NULL
  
  ###the first column is the y value. we should drop it, not useful
  all.data <- all.data[,c(2:ncol(all.data))]
  colnames(all.data) <- cubist.model$vars$all
  
  ###we have now reconstructed the training data in all.data df
  ###now we move on to classifying the predictions as type 1, 2, or 3
  ###type 1: data is outside of MIN/MAX of training data-> data is an extrapolation. Lower confidence
  ###type 2: data is an outlier from multivariate perspective. look at something like knn distance
  ###type 3: data is well within scope of training data
  
  out.vect <- rep(3, nrow(prediction.data))
  
  ###detect type 1 outliers
  mins <- apply(all.data, 2, min)
  maxs <- apply(all.data, 2, max)
  
  
  ###TODO: rewrite with apply statements
  
  for(i in 1:nrow(prediction.data)){
    for(j in 1:ncol(prediction.data)){
      ###first, check if column is factor or numeric
      if(class(prediction.data[i,j]) == "factor"){
        ###make sure that the i'th entry is a factor included in training set
        if(!(prediction.data[i,j] %in% unique(all.data[[j]]))){
          out.vect[i] <- "class"
          break
        }
      }
      if(class(prediction.data[i,j]) == "numeric"){
        ###make sure that the i'th entry is within range of training set
        if(prediction.data[i,j] > mins[j] | prediction.data[i,j] < maxs[j]){
          out.vect[i] <- "number"
          break
        }
      }
    }
  }
  
  
  
  
}