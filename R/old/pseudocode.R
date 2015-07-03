# listOfExpr is a list where each element is composed by the expr, params and consts
# extrapolate func are functions which generate single number from fit resutls and its std.err
slice(dataset,predictor,response,levelName,listOfFactors,listOfExpr,listOfExtrapolateFunc)
  active.dataset = dataset
  res.previous <- list()

  if listOfFactors is not NULL

    listOfNames = listOfNames(dataset,listOfFactors[1])
  
    for each name in listOfNames
      subset = subsetOf(dataset,listOfFactors[1],name)
      res.previous <- res.previous + slice(subset,name,listOfFactors[2,],listOfExpr[2,],listOfExtrapolateFunc[2,])
    
    active.dataset <- buildDataset(res.previous)

  res.name <- levelName
  res.dataset <- active.dataset
  res.fit <- fit(active.dataset,predictor,response,listOfExpr[1])
  res.row <- listOfExtrapolateFunc[1](res.fit)<this generates two columns>,levelName
    
  return res  