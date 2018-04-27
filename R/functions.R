#' @name fn_mode
#' @title Calculating mode value of a vector
#' @description The function returns the mode of a vector. The vector can be of any datatype ie. numerical or categorical.
#' @param x a vector of string or number
#' @return The function returns the mode value of the input vector.
#' @examples fn_mode(c(1,2,3,1,4,1,7))
#' @author Arya Poddar <aryapoddar290990@gmail.com>
#' @export
fn_mode <- function(x){
  dt <- data.frame(table(x))
  dt <- dt[is.na(dt$x)==F&dt$x!="",]
  s <- as.character(dt$x)[which.max(dt$Freq)]
  return(s)}

#' @name missing_val
#' @title Missing value imputation
#' @description The function imputes the missing value in the input dataset. For numerical variables, missing values can be replaced by four possible method - 1. "mean" - mean or simple average of the non-missing values ; 2. - "median" - median or the 50th percentile of the non-missing values; 3. "mode"- mode or the value with maximum frequency among the non-mising values; 4. special extreme value of users' choice to be passes as an argument (-99999 is the default value). For categorical value, missing class can be replaced by two possible methods - 1. "mode" - mode or the class with maximum frequency among the non-mising values; 2. special class of users' choice to be passes as an argument ("missing_value" is the default class). The target column will remain unchanged.
#' @param base input dataframe
#' @param target column/field name of the target variable, to be passed as a string
#' @param num_missing (optional) method for replacing missing values for numerical type fields - to be chosen between "mean", "median", "mode" or a value of users' choice (default value is -99999)
#' @param cat_missing (optional) method for replacing missing values for categorical type fields - to be chosen between "mode" or a class of users' choice (default value is "missing_value")
#' @return The function returns an object of class "missing_val" which is a list containing the following components:
#' @return \item{base}{a dataframe after imputing missing values}
#' @return \item{mapping_table}{a dataframe with mapping between original variable and imputed missing value (if any)}
#' @examples data <- iris
#' data$Species <- as.character(data$Species)
#' data$Y <- sample(0:1,size=nrow(data),replace=TRUE)
#' data[sample(1:nrow(data),size=25),"Sepal.Length"] <- NA
#' data[sample(1:nrow(data),size=10),"Species"] <- NA
#'
#' missing_list <- missing_val(base = data,target = "Y")
#' missing_list$base
#' missing_list$mapping_table
#' @author Arya Poddar <aryapoddar290990@gmail.com>
#' @export
missing_val <- function(base,target,num_missing=-99999,cat_missing="missing_value"){
  mapping_table <- data.frame()
  if(sum(is.na(base))>0){
    counter <- 0
    if(num_missing!="mean"&num_missing!="median"&num_missing!="mode"&is.na(as.numeric(num_missing))==T){
      print("Invalid numeric missing value argument")
      counter <- counter+1}

    if(cat_missing!="mode"&typeof(cat_missing)!="character"){
      print("Invalid numeric missing value argument")
      counter <- counter+1}

    t <- which(names(base)==target)
    if(sum(is.na(base[,t]))>0){
      print("Target has missing value")
      counter <- counter+1}

    if(counter==0){
      index <- setdiff(1:ncol(base),t)
      for(i in index){
        mapping_table[nrow(mapping_table)+1,"Variable_name"] <- names(base)[i]
        if(typeof(base[,i])=="character"){
          if(cat_missing=="mode"){
            mode_char <- as.character(fn_mode(base[,i]))
            base[is.na(base[,i])|base[,i]=="",i] <- mode_char
            mapping_table[nrow(mapping_table),"imputed_missing"] <- mode_char
          } else {
            base[is.na(base[,i])|base[,i]=="",i] <- cat_missing
            mapping_table[nrow(mapping_table),"imputed_missing"] <- cat_missing
          }
        } else {
          if(num_missing=="mean"){
            mean_num <- mean(base[,i],na.rm =T)
            base[is.na(base[,i]),i] <- mean_num
            mapping_table[nrow(mapping_table),"imputed_missing"] <- mean_num
          } else{
            if(num_missing=="median"){
              median_num <- median(base[,i],na.rm =T)
              base[is.na(base[,i]),i] <- median_num
              mapping_table[nrow(mapping_table),"imputed_missing"] <- median_num
            } else {
              if(num_missing=="mode"){
                mode_num <- as.numeric(fn_mode(base[,i]))
                base[is.na(base[,i]),i] <- mode_num
                mapping_table[nrow(mapping_table),"imputed_missing"] <- mode_num
              } else {
                base[is.na(base[,i]),i] <- as.numeric(num_missing)
                mapping_table[nrow(mapping_table),"imputed_missing"] <- as.numeric(num_missing)
              }  }  }  }  }
    }
  } else {print("There were no missing value")}
  l <- list(base=base, mapping_table=mapping_table)
  return(l)
}

#' @name fn_target
#' @title Redefines target value
#' @description The function redefines the "binary" target variable to be used for modelling. It takes the variable or field name of the target and the event class. It changes the target field name to "Target", changes the events into 1 and non-events as 0 and places the target column at the end of the dataframe before returning it as output.
#' @param base input dataframe
#' @param target column / field name for the target variable, to be passed as string
#' @param event the event class, to be passed as string
#' @return The function returns a dataframe after changing the target classes to 0 or 1.
#' @examples data <- iris
#' data$Species <- as.character(data$Species)
#' data$Y <- sample(0:1,size=nrow(data),replace=TRUE)
#'
#' data2 <- fn_target(base = data,target = "Y",event = 1)
#' @author Arya Poddar <aryapoddar290990@gmail.com>
#' @export
fn_target <- function(base,target,event){
  t <- which(names(base)==target)
  if(length(t)==0) {print("target not found")} else {
    names(base)[t] <- "target"
    base$target <- as.character(base$target)
    base$Target <- ifelse(base$target==event,1,0)
    base <- base[,-t]
    return(base)}
}

#' @name univariate
#' @title Univariate analysis of variables
#' @description The function gives univariate analysis of the variables as output dataframe. The univariate statistics includes - minimum, maximum, mean, median, number of distinct values, variable type, counts of null value, percentage of null value, maximum population percentage among all classes/values, correlation with target. It also returns the list of names of character and numerical variable types along with variable name with population concentration more than a threshold at a class/value.
#' @param base input dataframe
#' @param target column / field name for the target variable to be passed as string (must be 0/1 type)
#' @param threshold sparsity threshold, to be provided as decimal/fraction
#' @return The function returns an object of class "univariate" which is a list containing the following components:
#' @return \item{univar_table}{univariate summary of variables}
#' @return \item{num_var_name}{array of column names of numerical type variables}
#' @return \item{char_var_name}{array of column names of categorical type variables}
#' @return \item{sparse_var_name}{array of column names where population concentration at a class or value is more then the sparsity threshold}
#' @examples data <- iris
#' data$Species <- as.character(data$Species)
#' data$Y <- sample(0:1,size=nrow(data),replace=TRUE)
#'
#' univariate_list <- univariate(base = data,target = "Y",threshold = 0.95)
#' univariate_list$univar_table
#' univariate_list$num_var_name
#' univariate_list$char_var_name
#' univariate_list$sparse_var_name
#' @author Arya Poddar <aryapoddar290990@gmail.com>
#' @importFrom stats median cor
#' @export
univariate <- function(base,target,threshold){
  var_start_list <- data.frame()
  t <- which(names(base)==target)

  counter <- 0
  if(length(t)==0){
    print("Target not found")
    counter <- counter+1}

  u <- as.character(unique(base[,t]))

  if(length(u)!=2){
    print("Target not Binary")
    counter <- counter+1}

  if(min(u)!=0|max(u)!=1){
    print("Target not 0/1 type")
    counter <- counter+1}

  if(counter==0){
    index <- setdiff(1:ncol(base),t)

    for(i in index)
    { var_start_list[i,"var"] <- names(base)[i]
    if(typeof(base[,i])!="character"&is.factor(base[,i])==F){
      var_start_list[i,"var_min"] <- min(base[,i],na.rm=T)
      var_start_list[i,"var_max"] <- max(base[,i],na.rm=T)
      var_start_list[i,"mean"] <- mean(base[,i],na.rm=T)
      var_start_list[i,"median"] <- median(base[,i],na.rm=T)
      var_start_list[i,"var_vals"] <- length(unique(base[,i]))
      var_start_list[i,"type"] <- "numeric"
      var_start_list[i,"count_missing"] <- sum(is.na(base[,i]))
      var_start_list[i,"perc_missing"] <- var_start_list[i,"count_missing"]/nrow(base)
      var_start_list[i,"max_pop_conc"] <- max(table(base[,i]))/nrow(base)
      var_start_list$corr[i] <- cor(base[,paste0(var_start_list$var[i])],base[,t],use="pairwise.complete.obs")
    } else {
      var_start_list[i,"var_vals"] <- length(unique(base[,i]))
      var_start_list[i,"type"] <- "character"
      var_start_list[i,"count_missing"] <- sum(is.na(base[,i]))
      var_start_list[i,"perc_missing"] <- var_start_list[i,"count_missing"]/nrow(base)
      var_start_list[i,"max_pop_conc"] <- max(table(base[,i]))/nrow(base)}
    }
    var_start_list <- var_start_list[is.na(var_start_list$var)==F,]

    l <- list(univar_table=var_start_list,
              num_var_name=var_start_list[var_start_list$type!="character","var"],
              char_var_name=var_start_list[var_start_list$type=="character","var"],
              sparse_var_name=var_start_list[var_start_list$max_pop_conc>=threshold|var_start_list$perc_missing>=threshold |
                                               var_start_list$var_vals==1,"var"])
    return(l)}
}

#' @name others_class
#' @title Clubbing of classes of categorical variable with low population percentage into one class
#' @description The function groups the classes of a categorical variable which have population percentage less than a threshold as "Low_pop_perc". The user can choose whether to club the missing class or keep it as separate class. The default setting is that missing classes are not treated separately.
#' @param base input dataframe
#' @param target column / field name for the target variable to be passed as string (must be 0/1 type)
#' @param column_name column name or array of column names of the dataframe on which the operation is to be done
#' @param threshold threshold population percentage below which the class is to be classified as others, to be provided as decimal/fraction
#' @param char_missing (optional) imputed missing value for categorical variable if its to be kept separate (default value is NA)
#' @return \item{base}{a dataframe after converting all low percentage classes into "Low_pop_perc" class}
#' @return \item{mapping_table}{a dataframe with mapping between original classes which are now "Low_pop_perc" class (if any)}
#' @examples data <- iris[c(1:110),]
#' data$Y <- sample(0:1,size=nrow(data),replace=TRUE)
#' data$Species <- as.character(data$Species)
#' data_otherclass <- others_class(base = data,target = "Y",column_name = "Species",threshold = 0.15)
#' @author Arya Poddar <aryapoddar290990@gmail.com>
#' @export
others_class <- function(base,target,column_name,threshold,char_missing=NA){
  base1 <- base[,names(base) %in% c(target,column_name)]

  counter <- 0
  if(ncol(base1)==0){
    print("Invalid Column names")
    counter <- counter+1}

  if(ncol(base1)<length(column_name)){
    print("Some Column names are invalid")
    counter <- counter+1}

  if(threshold<=0|threshold>=1){
    print("Threshold out of bounds")
    counter <- counter+1}

  mapping_tab <- data.frame()

  if(counter==0){
    for(i in 1:length(column_name)){
      t <- which(names(base)==column_name[i])
      dtable <- data.frame(table(base[,t]))
      dtable$perc <- dtable$Freq/sum(dtable$Freq)
      dtable$Variable_name <- column_name[i]
      dtable$Var1 <- as.character(dtable$Var1)
      dtable$Category <- dtable$Var1
      var <- as.character(subset(dtable$Var1,dtable$perc<=threshold))
      if(is.na(char_missing)==F) var <- setdiff(var,char_missing)
      if(length(var)>1){
        dtable[which(dtable[,"Var1"] %in% var),"Var1"] <- "Low_pop_perc"
        base[which(base[,t] %in% var),t] <- "Low_pop_perc"}
      dtable <- dtable[,c(4,5,1)]
      names(dtable) <- c("Variable_name","Category","New_category")
      mapping_tab <- rbind(mapping_tab,dtable)
    }

    l <- list(base=base, mapping_table=mapping_tab)
    return(l)
  }
}

#' @name sampling
#' @title Random sampling of data into train and test
#' @description The function does random sampling of the data and split it into train and test datasets. Training base percentage and seed value(optional) is taken as arguments. If seed value is not specified, random seed will be generated on different iterations.
#' @param base input dataframe
#' @param train_perc (optional) percentage of total base to be kept as training sample, to be provided as decimal/fraction (default percentage is 0.7)
#' @param seed (optional) seed value (if not given random seed is generated)
#' @param replace (optional) whether replacement will e with or without replacement (default is FALSE ie. without replacement)
#' @return An object of class "sampling" is a list containing the following components:
#' @return \item{train_sample}{training sample as a dataframe}
#' @return \item{test_sample}{test sample as a dataframe}
#' @return \item{seed}{seed used}
#' @examples data <- iris
#' sampling_list <- sampling(base = data,train_perc = 0.7,seed = 1234)
#' sampling_list$train
#' sampling_list$test
#' sampling_list$seed
#' @author Arya Poddar <aryapoddar290990@gmail.com>
#' @export
sampling <- function(base,train_perc=0.7,seed=NA,replace=F){
  if(train_perc<=0|train_perc>=1){print("Invalid train percentage")} else {
    if(is.na(seed)==F) set.seed(seed)
    index <- sample(1:nrow(base),train_perc*nrow(base),replace=replace)
    l <- list(train=base[index,], test=base[-index,], seed=seed)
    return(l)}
}

#' @name club_cat_class
#' @title Clubbing class of a categorical variable with low population percentage with another class of similar event rate
#' @description The function groups classes of categorical variable, which have population percentage less than a threshold, with another class of similar event rate. If a class of exactly same event rate is not available, it is clubbed with the one having a higher event rate closest to it.
#' @param base input dataframe
#' @param target column / field name for the target variable to be passed as string (must be 0/1 type)
#' @param variable column name of categorical variable on which the operation is to be done, to be passed as string
#' @param threshold threshold population percentage below which the class will be considered to be be clubbed with another class, to be provided as decimal/fraction
#' @param event (optional) the event class, to be passed as 0 or 1 (default is 1)
#' @return The function returns a dataframe after clubbing low percentage classes with another class of similar or closest but higher event rate.
#' @examples data <- iris[1:110,]
#' data$Species <- as.character(data$Species)
#' data$Y <- sample(0:1,size=nrow(data),replace=TRUE)
#' data_clubclass <- club_cat_class(base = data,target = "Y",variable = "Species",threshold = 0.2)
#' @author Arya Poddar <aryapoddar290990@gmail.com>
#' @author Kanishk Dogar <kanishkd4@gmail.com>
#' @importFrom stats as.formula
#' @export
club_cat_class <- function(base,target,variable,threshold,event=1){
  base <- fn_target(base,target,event)
  t <- which(names(base)==variable)
  tab <- data.frame(table(base[,c(t,ncol(base))]))
  formula <- paste(variable,"Target",sep=' ~ ')
  tab <- dcast(data = tab,formula = as.formula(formula),value.var = "Freq")
  names(tab) <- c("Category","Non_event","Event")
  tab$Category <- as.character(tab$Category)
  tab$Total <- tab$Non_event+tab$Event
  tab$Pop_perc <- tab$Total/sum(tab$Total)

  while(min(tab$Pop_perc)<threshold){
    tab$Event_rate <- tab$Event/sum(tab$Total)
    tab <- tab[order(tab$Event_rate),]
    index <- which.min(tab$Pop_perc)
    index2 <- ifelse(index==nrow(tab),index-1,index+1)

    tab[index,"Category"] <- paste(tab[index,"Category"],tab[index2,"Category"],sep=",")
    tab[index,"Non_event"] <- tab[index,"Non_event"]+tab[index2,"Non_event"]
    tab[index,"Event"] <- tab[index,"Event"]+tab[index2,"Event"]
    tab[index,"Total"] <- tab[index,"Total"]+tab[index2,"Total"]

    tab <- tab[-index2,1:4]
    tab$Pop_perc <- tab$Total/sum(tab$Total)
    tab$Event_rate <- tab$Event/sum(tab$Total)
  }

  bands <- data.frame("Category"=tab[,"Category"],stringsAsFactors = F)
  output <- data.frame()

  for(i in 1:nrow(bands)){
    dt <- data.frame(strsplit(bands[i,"Category"],split = ","))
    dt$new_class <- bands[i,"Category"]
    names(dt) <- c("old_class","new_class")
    output <- rbind(output,dt)
  }

  output$Variable_name <- variable
  output <- output[,c(3,1,2)]
  return(output)
}

#' @name cat_new_class
#' @title Clubbing class of categorical variables with low population percentage with another class of similar event rate
#' @description The function groups classes of categorical variables, which have population percentage less than a threshold, with another class of similar event rate. If a class of exactly same event rate is not available, it is clubbed with the one having a higher event rate closest to it.
#' @param base input dataframe
#' @param target column / field name for the target variable to be passed as string (must be 0/1 type)
#' @param cat_var_name column name or array of column names of categorical variable on which the operation is to be done, to be passed as string
#' @param threshold threshold population percentage below which the class will be considered to be be clubbed with another class, to be provided as decimal/fraction
#' @param event (optional) the event class, to be passed as 0 or 1 (default is 1)
#' @return The function returns an object of class "cat_new_class" which is a list containing the following components:
#' @return \item{base_new}{a dataframe after clubbing low percentage classes with another class of similar or closest but higher event rate}
#' @return \item{cat_class_new}{a dataframe with mapping between original classes and new clubbed classes (if any)}
#' @examples data <- iris[1:110,]
#' data$Species <- as.character(data$Species)
#' data$Y <- sample(0:1,size=nrow(data),replace=TRUE)
#' data_newclass <- cat_new_class(base = data,target = "Y",cat_var_name = "Species",threshold = 0.1)
#' @author Arya Poddar <aryapoddar290990@gmail.com>
#' @author Kanishk Dogar <Kanishkd4@gmail.com>
#' @export
cat_new_class <- function(base,target,cat_var_name,threshold,event=1){
  cat_output <- data.frame()
  for(j in 1:length(cat_var_name)){
    cat_tab <- club_cat_class(base,target,cat_var_name[j],threshold,event=1)
    base[,cat_var_name[j]] <- cat_tab$new_class[match(base[,cat_var_name[j]], cat_tab$old_class)]
    cat_output <- rbind(cat_output,cat_tab)
  }
  l <- list(base_new=base,cat_class_new=cat_output)
  return(l)
}

#' @name dtree_split_val
#' @title Getting the split value for terminal nodes from decision tree
#' @description The function takes a ctree type model, with only one numerical variable, as argument input and gives a dataframe with the minimum and maximum value of each node. The intervals are open ended at lower limit and closed at upper limit.
#' @param desc_model ctree class model with one variable
#' @param variable numerical variable name which on which decision tree was run, to be passed as string
#' @return The function returns a dataframe giving the lower and upper bound of split values of each node.
#' @author Arya Poddar <aryapoddar290990@gmail.com>
#' @examples data <- iris
#' data$Y <- ifelse(data$Species=="setosa",1,0)
# desc_tree <- ctree(Y~Sepal.Length,data)
# dtree_tab <- dtree_split_val(desc_model = desc_tree,variable = "Species")
#' @import partykit
#' @import stringr
#' @importFrom utils getFromNamespace
#' @export
dtree_split_val <- function(desc_model,variable){
  list_rules <- getFromNamespace(".list.rules.party", "partykit")
  bound_tab <- test <- data.frame("node"=list_rules(desc_model))
  bound_tab$"Variable_name" <- variable
  bound_tab$node <- as.numeric(row.names(bound_tab))
  test$node <- gsub(paste0(variable,"|&| "),"",test$node)

  for(i in 1:nrow(test)){
    pos <- data.frame("index_1"=sort(union(unlist(gregexpr("<=",test$node[i])),
                                           unlist(gregexpr(">",test$node[i])))))
    pos$index_2 <- union(pos$index_1-1,str_length(test$node[i]))[-1]
    for(j in 1:nrow(pos)){
      pos$info[j] <- substr(test$node[i],pos$index_1[j],pos$index_2[j])
      pos$grt[j] <- unlist(str_split(pos$info[j],">"))[2]
      pos$less[j] <- unlist(str_split(pos$info[j],"<="))[2]
    }
    bound_tab[i,"Lower_open_bound"] <- suppressWarnings(max(as.numeric(pos$grt),na.rm=T))
    bound_tab[i,"Upper_closed_bound"] <- suppressWarnings(min(as.numeric(pos$less),na.rm=T))
  }
  bound_tab$Category <- paste0("(",bound_tab$Lower_open_bound,",",bound_tab$Upper_closed_bound,"]")
  return(bound_tab)
}

#' @name dtree_trend_iv
#' @title Recursive Decision Tree partitioning with monotonic event rate along with IV table for individual numerical variable
#' @description The function takes base data, target and the numerical variable which is to be binned. It returns the optimal cuts based on recursive partitioning decision tree such that the trend of event rate holds good ie. it is strictly monotonically increasing or decreasing. If missing values are imputed by any extreme value, the same can be passed as an argument, and it will be shown as a different category. The output is a dataframe with the WOE and IV value.
#' @param base input dataframe
#' @param target column / field name for the target variable to be passed as string (must be 0/1 type)
#' @param variable numerical variable name which is to be binned into categorical buckets, to be passed as string
#' @param num_missing (optional) imputed missing value for numerical variable or an array of values which are to be kept as different bucket in binning step (default value is -99999)
#' @param mincriterion (optional) the value of the test statistic or (1 - p-value) that must be exceeded in order to implement a split (default value is 0.1)
#' @param event (optional) the event class, to be passed as 0 or 1 (default is 1)
#' @return The function returns a dataframe with count and iv.
#' @examples data <- iris
#' data$Y <- ifelse(data$Species=="setosa",1,0)
#' dtree_trend_tab <- dtree_trend_iv(base = data,target = "Y",variable = "Sepal.Length",event = 1)
#' @author Arya Poddar <aryapoddar290990@gmail.com>
#' @author Aiana Goyal <aianagoel002@gmail.com>
#' @import partykit
#' @import stringr
#' @import reshape2
#' @importFrom stats as.formula
#' @export
dtree_trend_iv <- function(base,target,variable,num_missing=-99999,mincriterion=0.1,event=1){
  base <- fn_target(base,target,event)
  minb <- nrow(base)
  t <- which(names(base)==variable)
  if(typeof(num_missing)!="character"){
    base2 <- base[base[,t] %in% num_missing,]
    base <- base[!(base[,t] %in% num_missing),]}

  if(nrow(base)>0){
    formula <- paste("Target",paste0("`",variable,"`"),sep=' ~ ')
    dtree_model <- ctree(as.formula(formula),data=base,mincriterion=mincriterion,maxdepth=3,minbucket=0.05*minb)

    split_tab <- dtree_split_val(dtree_model,variable)
    dtree_tab <- data.frame(dtree_model$fitted)

    flag <- 1
    while(flag>0){
      tab <- data.frame(table(dtree_tab[,c(1,3)]))
      tab$X.fitted. <- as.numeric(as.character(tab[,1]))
      tab$X.response. <- as.numeric(as.character(tab[,2]))
      if(nrow(tab)==1){
        tab[2,"X.fitted."] <- tab[1,"X.fitted."]
        tab[2,"X.response."] <- ifelse(tab[1,"X.response."]==1,0,1)
        tab[2,"Freq"] <- 0
      }
      tab <- dcast(data = tab,formula = X.fitted. ~ X.response.,value.var = "Freq")
      names(tab) <- c("node","Non_event","Event")
      tab$Total <- tab$Non_event+tab$Event
      tab$Event_rate <- tab$Event/tab$Total
      n <- nrow(tab)
      tab <- cbind(tab,"lag"=c(0,tab$Event_rate[-n]))
      tab$diff <- tab$Event_rate-tab$lag
      tab$is_decr <- 2*as.numeric(tab$Event_rate-tab$lag<0)-1
      tab$is_decr[1] <- 0

      if(abs(sum(tab$is_decr))!=(n-1)){
        ifelse(sum(tab$is_decr)>=0,index <- which(tab$is_decr==-1),index <- which(tab$is_decr==1))
        index2 <- union(index,index+1)
        to_merge <- which.min(abs(tab[index2,"diff"]))
        mer1 <- index2[to_merge]-1
        mer2 <- index2[to_merge]
        split_tab$Lower_open_bound[mer1] <- suppressWarnings(min(split_tab$Lower_open_bound[mer1:mer2]))
        split_tab$Upper_closed_bound[mer1] <- suppressWarnings(max(split_tab$Upper_closed_bound[mer1:mer2]))
        split_tab <- split_tab[-mer2,]
        dtree_tab$X.fitted. <- ifelse(dtree_tab$X.fitted.==tab[mer2,"node"],tab[mer1,"node"],dtree_tab$X.fitted.)
      } else(flag <- 0)
    }

    output <- merge(split_tab,tab[,c(1:5)],all.x=T)
    output <- output[,-1]
    output$Category <- paste0("(",output$Lower_open_bound,",",output$Upper_closed_bound,"]")
  } else {output<-data.frame()}

  if(nrow(base2)>0){
    un <- sort(unique(base2[,t]))
    for(i in 1:length(un)){
      base3 <- base2[base2[,t]==un[i],]
      output[nrow(output)+1,"Variable_name"] <- variable
      output[nrow(output),"Lower_open_bound"] <- un[i]
      output[nrow(output),"Upper_closed_bound"] <- un[i]
      output[nrow(output),"Category"] <- paste0("sp_val_",un[i])
      output[nrow(output),"Event"] <- sum(base3[,"Target"])
      output[nrow(output),"Total"] <- nrow(base3)
      output[nrow(output),"Event_rate"] <- output[nrow(output),"Event"]/output[nrow(output),"Total"]
      output[nrow(output),"Non_event"] <- output[nrow(output),"Total"] - output[nrow(output),"Event"]
    }
  }

  output$Non_event_perc <- output$Non_event/sum(output$Non_event)
  output$Event_perc <- output$Event/sum(output$Event)
  output$Non_event_perc_adj <- ifelse(output$Non_event==0,0.5,output$Non_event)/sum(output$Non_event)
  output$Event_perc_adj <- ifelse(output$Event==0,0.5,output$Event)/sum(output$Event)
  output$Pop_perc <- output$Total/sum(output$Total)

  output$woe <- log(output$Non_event_perc_adj/output$Event_perc_adj)
  output$iv <- output$woe*(output$Non_event_perc_adj-output$Event_perc_adj)
  output <- output[,-c(11:12)]

  output[nrow(output)+1,5:13] <- colSums(output[,5:13])
  output[nrow(output),1] <- variable
  output[nrow(output),4] <- "Total"
  output[nrow(output),"Event_rate"] <- output[nrow(output),"Event"]/output[nrow(output),"Total"]
  return(output)
}

#' @name categorical_iv
#' @title IV table for individual categorical variable
#' @description The function takes base data, target and the categorical variable for which IV is to be calculated. It returns a dataframe with the WOE and IV value of the variable.
#' @param base input dataframe
#' @param target column / field name for the target variable to be passed as string (must be 0/1 type)
#' @param variable categorical variable name for which IV is to be calculated, to be passed as string
#' @param event (optional) the event class, to be passed as 0 or 1 (default is 1)
#' @return The function returns a dataframe.
#' @examples data <- iris
#' data$Species <- as.character(data$Species)
#' data$Y <- sample(0:1,size=nrow(data),replace=TRUE)
#' cat_iv <- categorical_iv(base = data,target = "Y",variable = "Species",event = 1)
#' @author Arya Poddar <aryapoddar290990@gmail.com>
#' @author Aiana Goyal <aianagoel002@gmail.com>
#' @import reshape2
#' @importFrom stats as.formula
#' @export
categorical_iv <- function(base,target,variable,event=1){
  base <- fn_target(base,target,event)
  t <- which(names(base)==variable)
  tab <- data.frame(table(base[,c(t,ncol(base))]))
  formula <- paste(variable,"Target",sep=' ~ ')
  tab <- dcast(data = tab,formula = as.formula(formula),value.var = "Freq")
  names(tab) <- c("Category","Non_event","Event")
  tab$Category <- as.character(tab$Category)
  tab$Total <- tab$Non_event+tab$Event
  tab$Event_rate <- tab$Event/tab$Total
  tab$Non_event_perc <- tab$Non_event/sum(tab$Non_event)
  tab$Event_perc <- tab$Event/sum(tab$Event)
  tab$Non_event_perc_adj <- ifelse(tab$Non_event==0,0.5,tab$Non_event)/sum(tab$Non_event)
  tab$Event_perc_adj <- ifelse(tab$Event==0,0.5,tab$Event)/sum(tab$Event)
  tab$Pop_perc <- tab$Total/sum(tab$Total)

  tab$woe <- log(tab$Non_event_perc_adj/tab$Event_perc_adj)
  tab$iv <- tab$woe*(tab$Non_event_perc_adj-tab$Event_perc_adj)
  tab$Variable_name <- variable
  tab <- tab[,c(13,1:7,10:12)]
  tab[nrow(tab)+1,3:11] <- colSums(tab[,3:11])
  tab[nrow(tab),1] <- variable
  tab[nrow(tab),2] <- "Total"
  return(tab)
}

#' @name iv_table
#' @title WOE and IV table for list of numerical and categorical variables
#' @description The function takes column indices of categorical and numerical variables and returns a list with four dataframes - WOE table of numerical variables, categorical variables, consolidated table of both numerical & categorical variables and a IV table.
#' @param base input dataframe
#' @param target column / field name for the target variable to be passed as string (must be 0/1 type)
#' @param num_var_name column name or array of column names of numerical variable for which IV is to be calculated, to be passed as string
#' @param num_missing (optional) imputed missing value for numerical variable or an array of values which are to be kept as different bucket in binning step (default value is -99999)
#' @param cat_var_name column name or array of column names of categorical variable for which IV is to be calculated, to be passed as string
#' @param mincriterion (optional) the value of the test statistic or (1 - p-value) that must be exceeded in order to implement a split (default value is 0.1)
#' @param event (optional) the event class, to be passed as 0 or 1 (default is 1)
#' @return An object of class "iv_table" is a list containing the following components:
#' @return \item{num_woe_table}{numerical woe table with IV as a dataframe}
#' @return \item{cat_woe_table}{categorical woe table with IV as a dataframe}
#' @return \item{woe_table}{numerical and categorical woe table with IV as a dataframe}
#' @return \item{iv_table}{Variable with IV value as a dataframe}
#' @examples data <- iris
#' data$Species <- as.character(data$Species)
#' data$Y <- sample(0:1,size=nrow(data),replace=TRUE)
#' x <- c("Sepal.Length","Sepal.Width","Petal.Length","Petal.Width")
#' iv_table_list <- iv_table(base = data,target = "Y",num_var_name = x,cat_var_name = "Species")
#' iv_table_list$num_woe_table
#' iv_table_list$cat_woe_table
#' iv_table_list$woe_table
#' iv_table_list$iv_table
#' @author Arya Poddar <aryapoddar290990@gmail.com>
#' @author Aiana Goyal <aianagoel002@gmail.com>
#' @author Kanishk Dogar <kanishkd4@gmail.com>
#' @export
iv_table <- function(base,target,num_var_name=F,num_missing=-99999,cat_var_name=F,mincriterion=0.1,event=1){
  num_output <- cat_output <- data.frame()

  if(length(num_var_name)>1|num_var_name[1]!=F){
    for(i in 1:length(num_var_name)){
      num_tab <- dtree_trend_iv(base,target,num_var_name[i],num_missing,mincriterion,event)
      num_output <- rbind(num_output,num_tab)}
  }

  if(cat_var_name!=F){
    for(j in 1:length(cat_var_name)){
      cat_tab <- categorical_iv(base,target,cat_var_name[j],event)
      cat_output <- rbind(cat_output,cat_tab)}
  }

  full_output <- rbind(num_output[,-c(2,3)],cat_output)
  iv_output <- full_output[full_output$Category=="Total",c(1,11)]
  iv_output <- iv_output[order(-iv_output$iv),]

  l <- list(num_woe_table=num_output, cat_woe_table=cat_output, woe_table=full_output, iv_table=iv_output)
  return(l)
}

#' @name iv_filter
#' @title Variable reduction based on Information Value filter
#' @description The function returns a list of variables that can be dropped because of low discriminatory power, based on Information Value. If IV for a variable is less than a user defined threshold, the variable will be recommended to be dropped by this function.
#' @param base input dataframe
#' @param iv_table dataframe of class iv_table with two columns - Variable_name, iv
#' @param threshold threshold IV value below which the variable will be recommended to be dropped
#' @return An object of class "iv_filter" is a list containing the following components:
#' @return \item{retain_var_tab}{variables remaining post IV filter as a dataframe}
#' @return \item{retain_var_name}{array of column names of variables to be retained}
#' @return \item{dropped_var_tab}{variables that can be dropped based on IV filter as a dataframe}
#' @return \item{threshold}{threshold IV value used as input parameter}
#' @examples data <- iris
#' data$Y <- sample(0:1,size=nrow(data),replace=TRUE)
#' x <- c("Sepal.Length","Sepal.Width","Petal.Length","Petal.Width")
#' iv_table_list <- iv_table(base = data,target = "Y",num_var_name = x,cat_var_name = "Species")
#' ivf_list <- iv_filter(base = data,iv_table = iv_table_list$iv_table,threshold = 0.02)
#' ivf_list$retain_var_tab
#' ivf_list$retain_var_name
#' ivf_list$dropped_var_tab
#' ivf_list$threshold
#' @author Arya Poddar <aryapoddar290990@gmail.com>
#' @export
iv_filter<-function(base,iv_table,threshold){
  iv_ret <- iv_table[iv_table$iv>=threshold,]
  iv_drop <- iv_table[iv_table$iv<threshold,]

  l <- list(retain_var_tab=iv_ret, retain_var_name=iv_ret$Variable_name,dropped_var_tab=iv_drop, threshold=threshold)
  return(l)
}

#' @name num_to_cat
#' @title Binning numerical variables based on cuts from IV table
#' @description The function takes the num_woe_table output from a class "iv_table". Based on the split points from the num_woe_table, the numerical variables are binned into categories.
#' @param base input dataframe
#' @param num_woe_table num_woe_table class from iv table output
#' @param num_missing (optional) imputed missing value for numerical variable (default value is -99999)
#' @return The function returns a dataframe after converting the numerical variables into categorical classes.
#' @examples data <- iris
#' data$Y <- sample(0:1,size=nrow(data),replace=TRUE)
#' x <- c("Sepal.Length","Sepal.Width","Petal.Length","Petal.Width")
#' iv_table_list <- iv_table(base = data,target = "Y",num_var_name = x,cat_var_name = "Species")
#' num_cat <- num_to_cat(base = data,num_woe_table = iv_table_list$num_woe_table)
#' @author Arya Poddar <aryapoddar290990@gmail.com>
#' @export
num_to_cat <- function(base,num_woe_table,num_missing=-99999){
  num_dt <- num_woe_table[num_woe_table$Category!="Total",]
  if(typeof(num_missing)!="character"){
    num_dt$Lower_open_bound <- gsub("missing",num_missing,num_dt$Lower_open_bound)}
  u <- unique(num_woe_table$Variable_name)

  for(i in 1:length(u)){
    cut_pt <- c(subset(num_dt$Lower_open_bound,num_dt$Variable_name==u[i]),"Inf")
    t <- which(names(base)==u[i])
    if(length(t)>0) base[,t] <- cut(base[,t],breaks=unique(cut_pt))
  }
  return(base)
}

#' @name cv_test
#' @title Cramer's V value between two categorical variables
#' @description The function gives the pairwise Cramer's V value between two input categorical variables.
#' @param base input dataframe
#' @param var_1 categorical variable name, to be passed as string
#' @param var_2 categorical variable name, to be passed as string
#' @return The function returns a dataframe with pairwise CV value.
#' @examples data <- iris
#' data$Species <- as.character(data$Species)
#' data$Sepal.Length <- as.character(floor(data$Sepal.Length))
#' data$Y <- sample(0:1,size=nrow(data),replace=TRUE)
#' cv_result <- cv_test(base = data,var_1 = "Species",var_2 = "Sepal.Length")
#' @author Arya Poddar <aryapoddar290990@gmail.com>
#' @importFrom stats chisq.test
#' @export
cv_test <- function(base,var_1,var_2){
  t1 <- which(names(base)==var_1)
  t2 <- which(names(base)==var_2)
  base <- base[,c(t1,t2)]
  names(base) <- c("V1","V2")
  cv = suppressWarnings(sqrt(chisq.test(base$V1,base$V2,correct=F)$statistic/
              (length(base$V1)*(min(length(unique(base$V1)),length(unique(base$V2)))-1))))
  output <- data.frame()
  output[1,"var_1"] <- var_1
  output[1,"var_2"] <- var_2
  output[1,"cv_value"] <- cv
  return(output)
}

#' @name cv_table
#' @title Pairwise Cramer's V among a list of categorical variables
#' @description The function gives a dataframe with pairwise Cramer's V value between all possible combination of categorical variables from the list of variables provided.
#' @param base input dataframe
#' @param column_name column name or array of column names for which Cramer's V is to be calculated
#' @return An object of class "cv_table" is a list containing the following components:
#' @return \item{cv_val_tab}{pairwise Cramer's V value as a dataframe}
#' @return \item{single_class_var_index}{array of column index of variables with only one class}
#' @examples data <- iris
#' data$Species <- as.character(data$Species)
#' data$Sepal.Length <- as.character(floor(data$Sepal.Length))
#' cv_tab_list <- cv_table(data, c("Species", "Sepal.Length"))
#' cv_tab_list$cv_val_tab
#' cv_tab_list$single_class_var_index
#' @author Arya Poddar <aryapoddar290990@gmail.com>
#' @export
cv_table <- function(base,column_name){
  if(length(column_name)<2) {print("only one column selected")} else {
    base <- base[,names(base) %in% column_name]
    u <- c()
    for(i in 1:ncol(base))  if(length(unique(base[,i]))==1) {u <- c(u,i)}

    if(length(u)>0) {base2 <- base[,-u]} else {base2 <- base}
    n <- ncol(base2)
    output <- data.frame()
    for(i in 1:(n-1))
      for(j in (i+1):n){
        tab <- cv_test(base2,names(base2)[i],names(base2)[j])
        output <- rbind(output,tab)
      }
  }
  l <- list(cv_val_tab=output, single_class_var_index=u)
  return(l)
}

#' @name cv_filter
#' @title Variable reduction based on Cramer's V filter
#' @description The function returns a list of variables that can be dropped because of high correlation with another variable, based on Cramer's V and IV. If V1 and V2 have a Cramer's V value more than a user defined threshold, the variable with lower IV will be recommended to be dropped by this function. The variable which got dropped wont be considered for dropping any more variables.
#' @param cv_table dataframe of class cv_table with three columns - var_1, var_2, cv_value
#' @param iv_table dataframe of class iv_table with two columns - Variable_name, iv
#' @param threshold Cramers' V value above which one of the variable will be recommended to be dropped
#' @return An object of class "cv_filter" is a list containing the following components:
#' @return \item{retain_var_list}{list of variables remaining post CV filter}
#' @return \item{dropped_var_list}{list of variables that can be dropped based on CV filter}
#' @return \item{dropped_var_tab}{CV correlation value for dropped variables as a dataframe}
#' @return \item{threshold}{threshold CV value used as input parameter}
#' @examples data <- iris
#' set.seed(11)
#' data$Y <- sample(0:1,size=nrow(data),replace=TRUE)
#' cv_tab_list <- cv_table(data, c("Species", "Sepal.Length"))
#' cv_tab <- cv_tab_list$cv_val_tab
#' x <- c("Sepal.Length","Sepal.Width","Petal.Length","Petal.Width")
#' iv_table_list <- iv_table(base = data,target = "Y",num_var_name = x,cat_var_name = "Species")
#' iv_tab <- iv_table_list$iv_table
#' cv_filter_list <- cv_filter(cv_table = cv_tab,iv_table = iv_tab,threshold = 0.5)
#' cv_filter_list$retain_var_list
#' cv_filter_list$dropped_var_list
#' cv_filter_list$dropped_var_tab
#' cv_filter_list$threshold
#' @author Arya Poddar <aryapoddar290990@gmail.com>
#' @export
cv_filter <- function(cv_table,iv_table,threshold){
  tab <- merge(cv_table,iv_table,by.x="var_1",by.y="Variable_name")
  tab <- merge(tab,iv_table,by.x="var_2",by.y="Variable_name")
  tab <- tab[,c(1:3,5,4)]
  names(tab) <- c("var_1","var_2","cv_value","iv_1","iv_2")
  tab <- tab[tab$cv_value>threshold,]
  tab <-tab[order(-tab$cv_value),]

  var_drop <- data.frame()
  j <- 1
  while(nrow(tab)>0){
    if(tab$iv_1[1]>=tab$iv_2[1]){
      var_drop[j,"var_1"] <- tab$var_2[1]
      var_drop[j,"var_2"] <- tab$var_1[1]
      var_drop[j,"cv_value"] <- tab$cv_value[1]
      var_drop[j,"iv_1"] <- tab$iv_2[1]
      var_drop[j,"iv_2"] <- tab$iv_1[1]
    } else {var_drop <- rbind(var_drop,tab[1,])}

    tab <- tab[tab$var_2!=var_drop[j,"var_1"]&tab$var_1!=var_drop[j,"var_1"],]
    j <- j+1
  }

  names(var_drop) <- c("dropped_var","var_2","cv_value","dropped_var_iv","iv_2")
  var_retain <- setdiff(unique(c(cv_table$var_1,cv_table$var_2)),var_drop$dropped_var)
  l <- list(retain_var_list=var_retain,dropped_var_list=var_drop$dropped_var,dropped_var_tab=var_drop,threshold=threshold)
  return(l)
}

#' @name vif_filter
#' @title Removing multicollinearity from a model using vif test
#' @description The function takes a dataset with the starting variables and target only. The vif is calculated and if the maximum vif value is more than the threshold, the variable is dropped from the model and the vif's are recomputed. These steps of computing vif and dropping variable keep iterating till the maximum vif value is less than or equal to the threshold.
#' @param base input dataframe with set of final variables only along with target
#' @param target column / field name for the target variable to be passed as string (must be 0/1 type)
#' @param threshold threshold value for vif (default value is 2)
#' @return An object of class "vif_filter" is a list containing the following components:
#' @return \item{vif_table}{vif table post vif filtering}
#' @return \item{model}{the model used for vif calculation}
#' @return \item{retain_var_list}{variables remaining in the model post vif filter as an array}
#' @return \item{dropped_var_list}{variables dropped from the model in vif filter step}
#' @return \item{threshold}{threshold }
#' @examples data <- iris
#' set.seed(11)
#' data$Y <- sample(0:1,size=nrow(data),replace=TRUE)
#' vif_data_list <- vif_filter(base = data,target = "Y")
#' vif_data_list$vif_table
#' vif_data_list$model
#' vif_data_list$retain_var_list
#' vif_data_list$dropped_var_list
#' vif_data_list$threshold
#' @author Arya Poddar <aryapoddar290990@gmail.com>
#' @import car
#' @importFrom stats glm as.formula
#' @export
vif_filter <- function(base,target,threshold=2){
  var <- c()
  counter <- 1
  while(counter>0){
    base2 <- base[,!(names(base) %in% var)]
    formula <- paste(target,".",sep=' ~ ')
    log_model <- glm(as.formula(formula),data = base2,family = "binomial")
    vif_dt <- data.frame(vif(log_model))
    if(suppressWarnings(max(vif_dt$GVIF))<=threshold) {counter <- 0} else {
      v <- row.names(vif_dt)[which.max(vif_dt$GVIF)]
      v <- gsub("`","",v)
      var <- c(var,v)}
  }

  l <- list(vif_table=vif_dt, model=log_model,
            retain_var_list=setdiff(names(base),c(var,"Target")), dropped_var_list=var, threshold=threshold)
  return(l)
}

#' @name scalling
#' @title Converting coefficients of logistic regression into scores for scorecard building
#' @description The function takes a logistic model as input and scales the coefficients into scores to be used for scorecard generation. The
#' @param base base input dataframe
#' @param target column / field name for the target variable to be passed as string (must be 0/1 type)
#' @param model input logistic model from which the coefficients are to be picked
#' @param point (optional) points after which the log odds will get multiplied by "factor" (default value is 15)
#' @param factor (optional) factor by which the log odds must get multiplied after a step of "points" (default value is 2)
#' @param setscore (optional) input for setting offset (default value is 660)
#' @return The function returns a dataframe with the coefficients and scalled scores for each class of all explanatory variables of the model.
#' @examples data <- iris
#' set.seed(11)
#' data$Y <- sample(0:1,size=nrow(data),replace=TRUE)
#' x <- c("Sepal.Length","Sepal.Width","Petal.Length","Petal.Width")
#' iv_table_list <- iv_table(base = data,target = "Y",num_var_name = x,cat_var_name = "Species")
#' num_cat <- num_to_cat(base = data,num_woe_table = iv_table_list$num_woe_table)
#' log_model <- glm(Y ~ ., data = num_cat, family = "binomial")
#' scaling_tab <- scalling(base = num_cat,target = "Y",model = log_model)
#' @author Arya Poddar <aryapoddar290990@gmail.com>
#' @export
scalling <- function(base,target,model,point=15,factor=2,setscore=660){
  sc1 <- data.frame()
  t <- which(names(base)==target)
  base2 <- base[,-t]

  for(i in 1:ncol(base2)){
    sc <- data.frame("category"=unique(base2[,i]))
    sc$var <- names(base2)[i]
    sc$comb <- paste0(sc$var,sc$category)
    sc1 <- rbind(sc1,sc)}

  sc2 <- data.frame(model$coefficients)
  intercept <- sc2$model.coefficients[1]
  sc2$var <- row.names(sc2)
  sc2 <- sc2[-1,]

  for(i in 1:nrow(sc1)){
    sub_coeff <- subset(sc2$model.coefficients,sc2$var==sc1$comb[i])
    ifelse(length(sub_coeff)>0,sc1$coefficient[i] <- sub_coeff,sc1$coefficient[i] <- 0)}

  m <- 0
  sc3 <- data.frame()
  for(i in 1:ncol(base2)){
    sub <- subset(sc1,sc1$var==names(base2)[i])
    mn <- min(sub$coefficient)
    sub$dij_hat <- sub$coefficient-mn
    m <- m+mn
    sc3 <- rbind(sc3,sub)}

  offset <- setscore-((point/log(factor))*log(point))
  No_of_var <- ncol(base2)
  sumdij <- intercept+m
  fctor <- (sumdij*(point/log(factor)))/No_of_var
  scr <- offset/No_of_var+fctor

  sc3$score <- round((point/log(factor))*sc3$dij_hat+scr)
  sc3 <- sc3[,c(2,1,4:6)]
  names(sc3) <- c("Variable","Category","Coefficient","D(i,j)_hat","Score")

  return(sc3)
}

#' @name scoring
#' @title Scoring a dataset with class based on a scalling logic to arrive at final score
#' @description The function takes the data, with each variable as class. The dataframe of class scalling is used to convert the class into scores and finally arrive at the row level final scores by adding up the score values.
#' @param base input dataframe with classes same as scalling logic
#' @param target column / field name for the target variable to be passed as string (must be 0/1 type)
#' @param scalling dataframe of class scalling with atleast two columns - Variable, Category, Coefficient, D(i,j)_hat, Score
#' @return The function returns a dataframe with classes converted to scores and the final score for each record in the input dataframe.
#' @examples data <- iris
#' set.seed(11)
#' data$Y <- sample(0:1,size=nrow(data),replace=TRUE)
#' x <- c("Sepal.Length","Sepal.Width","Petal.Length","Petal.Width")
#' iv_table_list <- iv_table(base = data,target = "Y",num_var_name = x,cat_var_name = "Species")
#' num_cat <- num_to_cat(base = data,num_woe_table = iv_table_list$num_woe_table)
#' log_model <- glm(Y ~ ., data = num_cat, family = "binomial")
#' scaling_tab <- scalling(base = num_cat,target = "Y",model = log_model)
#' score_tab <- scoring(base = num_cat,target = "Y",scalling = scaling_tab)
#' @author Arya Poddar <aryapoddar290990@gmail.com>
#' @export
scoring <- function(base,target,scalling){
  counter <- 0
  if(ncol(base)>length(unique(scalling$Variable))+1) {
    print("additional columns selected")
    counter <- counter+1
  }

  if(ncol(base)<=length(unique(scalling$Variable))) {
    print("some columns are missing")
    counter <- counter+1
  }

  if(ncol(base)>length(unique(scalling$Variable))+1) {
    print("additional columns selected")
    counter <- counter+1
  }

  t <- which(names(base)==target)
  if(length(t)==0) {
    print("target not found")
    counter <- counter+1}

  index <- setdiff(1:ncol(base),t)

  if(counter==0){
    for(i in index){
      score <- subset(scalling,scalling$Variable==names(base)[i])
      base[,i] <- score$Score[match(base[,i],score$Category)]
    }

    base$total_score <- rowSums(base[,-t])
    return(base)}
}

#' @name gini_table
#' @title Performance measure table with Gini coefficient, KS-statistics and Gini lift curve
#' @description The function takes a dataframe along with a model or the name of a column with predicted value. If a model (only lm or glm works is guaranted to work perfectly) is provided as argument, the response on the data is predicted. Otherwise, if the data already contains a predicted column, it can be referred as an argument. The predicted column, thus obtained, is classified into bands to get the Gini coefficient, Kolmogorov-Smirnov statistics and Gini lift curve. The number of bands required can be passed as argument, with default value as 10 ie. decile binning is done. Otherwise, the cutpoints for converting the predicted value into bands can also be specified.
#' @param base input dataframe
#' @param target column / field name for the target variable to be passed as string (must be 0/1 type)
#' @param col_pred (optional) column name which contains the predicted value, not required if "model"=TRUE (default value is FALSE)
#' @param model (optional) object of type lm or glm model, required only if "col_pred"=FALSE (default value is FALSE)
#' @param brk (optional) array of break points of predicted value (default value is FALSE)
#' @param quantile_pt (optional) number of quantiles to divide the predicted value range (default value is 10)
#' @return An object of class "gini_table" is a list containing the following components:
#' @return \item{prediction}{base with the predicted value as a dataframe}
#' @return \item{gini_tab}{gini table as a dataframe}
#' @return \item{gini_value}{gini coefficient value}
#' @return \item{gini_plot}{gini curve plot}
#' @return \item{ks_value}{Kolmogorov-Smirnov statistic}
#' @return \item{breaks}{break points}
#' @examples data <- iris
#' data$Species <- as.character(data$Species)
#' set.seed(11)
#' data$Y <- sample(0:1,size=nrow(data),replace=TRUE)
#' set.seed(11)
#' data$Y_pred <- sample(300:900,size=nrow(data),replace=TRUE)
#' gini_tab_list <- gini_table(base = data,target = "Y",col_pred = "Y_pred",quantile_pt = 10)
#' gini_tab_list$prediction
#' gini_tab_list$gini_tab
#' gini_tab_list$gini_value
#' gini_tab_list$gini_plot
#' gini_tab_list$ks_value
#' gini_tab_list$breaks
#' @author Arya Poddar <aryapoddar290990@gmail.com>
#' @author Aiana Goyal <aianagoel002@gmail.com>
#' @importFrom stats predict quantile
#' @importFrom ggplot2 ggplot aes_ geom_line aes geom_segment
#' @export
gini_table <- function(base,target,col_pred=F,model=F,brk=F,quantile_pt=10){
  if(model!=F){
    base$predicted <- predict(model,newdata = base,type = "response")} else {
      p <- which(names(base)==col_pred)
      base$predicted <- base[,p]}

  t <- which(names(base)==target)
  names(base)[t] <- "Target"
  base <- base[,c("Target","predicted")]

  ifelse(prod(brk==F)==1,brk <- unique(quantile(base$predicted,probs = seq(0,1,by=(1/quantile_pt)))),brk <- brk)
  base$band <- cut(base$predicted,breaks=brk, include.lowest = T)
  tab <- data.frame(table(base[,c("Target","band")]))
  tab <- dcast(data = tab,band ~ Target,value.var = "Freq")
  names(tab) <- c("Band","Non_event","Event")
  tab$Lower_open_bound <- gsub("\\(","",unlist(str_split(tab$Band,","))[seq(1,nrow(tab)*2,by=2)])
  tab$Upper_closed_bound <- gsub("]","",unlist(str_split(tab$Band,","))[seq(2,nrow(tab)*2,by=2)])
  tab <- tab[order(as.numeric(tab$Upper_closed_bound)),]
  tab$Total <- tab$Non_event+tab$Event
  tab <- tab[,c(4,5,1,6,2,3)]
  tab$Event_rate <- tab$Event/tab$Total
  tab$row <- 1:nrow(tab)
  suppressWarnings(tab[nrow(tab)+1,] <- 0)
  tab <- tab[order(tab$row),-8]
  tab$Cuml_non_event <- cumsum(tab$Non_event)
  tab$Cuml_event <- cumsum(tab$Event)
  tab$Pop_perc <- tab$Total/sum(tab$Total)
  tab$Cuml_non_event_perc <- tab$Cuml_non_event/sum(tab$Non_event)
  tab$Cuml_event_perc <- tab$Cuml_event/sum(tab$Event)
  tab$Diff <- tab$Cuml_event_perc - tab$Cuml_non_event_perc

  for(i in 2:nrow(tab))
    tab[i,"GINI_formula"] <- (tab[i,"Cuml_event_perc"]+tab[i-1,"Cuml_event_perc"])*
    (tab[i,"Cuml_non_event_perc"]-tab[i-1,"Cuml_non_event_perc"])/2

  gini <- (sum(tab$GINI_formula,na.rm = T)*2)-1
  ks <- max(tab$Diff)

  m <- ggplot(data=tab,aes_(x=~Cuml_non_event_perc,y=~Cuml_event_perc))+geom_line(aes(color=10))+geom_segment(aes(x=0,y=0,xend=1,yend=1))
  # dev.off()

  l <- list(prediction=base, gini_tab=tab, gini_value=gini, gini_plot=m, ks_value=ks, breaks=brk)
  return(l)
}

#' @name fn_conf_mat
#' @title Creates confusion matrix and its related measures
#' @description The function takes the base dataframe with observed/actual and predicted columns. The actual/predicted class preferably should be binary and if not, it will be considered as event vs rest. It computes the performance measures like accuracy, precision, recall, sensitivity, specificity and f1 score.
#' @param base input dataframe
#' @param observed_col column / field name of the observed event
#' @param predicted_col column / field name of the predicted event
#' @param event the event class, to be passed as string
#' @return An object of class "fn_conf_mat" is a list containing the following components:
#' @return \item{confusion_mat}{confusion matrix as a table}
#' @return \item{accuracy}{accuracy measure}
#' @return \item{precision}{precision measure}
#' @return \item{recall}{recall measure}
#' @return \item{sensitivity}{sensitivity measure}
#' @return \item{specificity}{specificity measure}
#' @return \item{f1_score}{F1 score}
#' @examples data <- iris
#' data$Species <- as.character(data$Species)
#' set.seed(11)
#' data$Y <- sample(0:1,size=nrow(data),replace=TRUE)
#' data$Y_pred <- sample(0:1,size=nrow(data),replace=TRUE)
#' fn_conf_mat_list <- fn_conf_mat(base = data,observed_col = "Y",predicted_col = "Y_pred",event = 1)
#' fn_conf_mat_list$confusion_mat
#' fn_conf_mat_list$accuracy
#' fn_conf_mat_list$precision
#' fn_conf_mat_list$recall
#' fn_conf_mat_list$sensitivity
#' fn_conf_mat_list$specificity
#' fn_conf_mat_list$f1_score
#' @author Arya Poddar <aryapoddar290990@gmail.com>
#' @export
fn_conf_mat <- function(base,observed_col,predicted_col,event){
  t1 <- which(names(base)==observed_col)
  t2 <- which(names(base)==predicted_col)
  base_perf <- base[,c(t1,t2)]
  names(base_perf) <- c("Observed","Predicted")
  base_perf$Observed <- ifelse(base_perf$Observed==event,"Event","Non_event")
  base_perf$Predicted <- ifelse(base_perf$Predicted==event,"Event","Non_event")
  dt <- data.frame(table(base_perf))

  tab <- dcast(data = dt,formula = Predicted ~ Observed,value.var = "Freq")
  accuracy <- (tab$Event[1]+tab$Non_event[2])/(tab$Event[1]+tab$Non_event[1]+tab$Event[2]+tab$Non_event[2])
  precision <- tab$Event[1]/(tab$Event[1]+tab$Non_event[1])
  recall <- tab$Event[1]/(tab$Event[1]+tab$Event[2])
  sensitivity <- tab$Event[1]/(tab$Event[1]+tab$Event[2])
  specificity <- tab$Non_event[2]/(tab$Non_event[1]+tab$Non_event[2])
  f1 <- 2*precision*recall/(precision+recall)

  l <- list(confusion_mat=table(base_perf), accuracy=accuracy, precision=precision, recall=recall,
            sensitivity=sensitivity, specificity=specificity, f1_score=f1)
  return(l)}

#' @name fn_error
#' @title Computes error measures between observed and predicted values
#' @description The function takes the input dataframe with observed and predicted columns and computes mean absolute error, mean squared error and root mean squared error terms.
#' @param base input dataframe
#' @param observed_col column / field name of the observed event
#' @param predicted_col column / field name of the predicted event
#' @return An object of class "fn_error" is a list containing the following components:
#' @return \item{mean_abs_error}{mean absolute error between observed and predicted value}
#' @return \item{mean_sq_error}{mean squared error between observed and predicted value}
#' @return \item{root_mean_sq_error}{root mean squared error between observed and predicted value}
#' @examples data <- iris
#' data$Species <- as.character(data$Species)
#' set.seed(11)
#' data$Y <- sample(0:1,size=nrow(data),replace=TRUE)
#' data$Y_pred <- sample(0:1,size=nrow(data),replace=TRUE)
#' fn_error_list <- fn_error(base = data,observed_col = "Y",predicted_col = "Y_pred")
#' fn_error_list$mean_abs_error
#' fn_error_list$mean_sq_error
#' fn_error_list$root_mean_sq_error
#' @author Arya Poddar <aryapoddar290990@gmail.com>
#' @export
fn_error <- function(base,observed_col,predicted_col){
  t1 <- which(names(base)==observed_col)
  t2 <- which(names(base)==predicted_col)
  base_perf <- base[,c(t1,t2)]
  names(base_perf) <- c("Observed","Predicted")
  base_perf$abs_err <- abs(base_perf$Observed - as.numeric(as.character(base_perf$Predicted)))
  base_perf$sq_err <- base_perf$abs_err^2
  mae <- sum(base_perf$abs_err)/nrow(base_perf)
  mse <- sum(base_perf$sq_err)/nrow(base_perf)
  rmse <- sqrt(mse)

  l <- list(mean_abs_error=mae, mean_sq_error=mse, root_mean_sq_error=rmse)
  return(l)}

#' @name fn_cross_index
#' @title Creates random index for k-fold cross validation
#' @description The function base and returns a list of length k, to be used for k-fold cross validation sampling. Each element of the returned list is an array of random index for sampling for k-fold cross validation.
#' @param base input dataframe
#' @param k number of cross validation
#' @return The function a list of length k, each holding an array of index/row number for sampling the base.
#' @examples data <- iris
#' data$Species <- as.character(data$Species)
#' set.seed(11)
#' data$Y <- sample(0:1,size=nrow(data),replace=TRUE)
#' data$Y_pred <- sample(0:1,size=nrow(data),replace=TRUE)
#' data_k_list <- fn_cross_index(base = data,k = 5)
#' data_k_list$index1
#' data_k_list$index2
#' data_k_list$index3
#' data_k_list$index4
#' data_k_list$index5
#' @author Arya Poddar <aryapoddar290990@gmail.com>
#' @importFrom stats setNames
#' @export
fn_cross_index <- function(base,k){
  n <- nrow(base)
  bucket <- ceiling(n/k)
  index <- sample(1:n)

  l <- list()
  for(i in 1:k){
    assign(paste0("index",i),index[((i-1)*bucket+1):min(i*bucket,n)])
    l <- c(l,setNames(list(get(paste0("index",i))),paste0("index",i)))}
  return(l)
}

#' @name support_vector_parameters
#' @title Hyperparameter optimisation or parameter tuning for Suppert Vector Machine by grid search
#' @description The function runs a grid search with k-fold cross validation to arrive at best parameter decided by some performance measure. The parameters that can be tuned using this function for support vector machine algorithm are - kernel (linear / polynomial / radial / sigmoid), degree of polynomial, gamma and cost. The objective function to be minimised is the error (mean absolute error / mean squared error / root mean squared error). For the grid search, the possible values of each tuning parameter needs to be passed as an array into the function.
#' @param base input dataframe
#' @param target column / field name for the target variable to be passed as string (must be 0/1 type)
#' @param scale (optional) logical vector indicating the variables to be scaled (default value is TRUE)
#' @param kernel an array of kernels to be iterated on; kernel used in training and predicting, to be cheosen among "linear", "polynomial", "radial" and "sigmoid"
#' @param degree (optional) an array of degree of polynomial to be iterated on; parameter needed for kernel of type "polynomial" (default value is 2)
#' @param gamma an array of gamma values to be iterated on; parameter needed for all kernels except linear
#' @param cost an array of cost to be iterated on; cost of constraints violation
#' @param error (optional) error measure as objective function to be minimised, to be chosen among "mae", "mse" and "rmse" (default value is "rmse")
#' @param cv (optional) k vakue for k-fold cross validation to be performed (default value is 1 ie. without cross validation)
#' @return An object of class "support_vector_parameters" is a list containing the following components:
#' @return \item{error_tab_detailed}{error summary for each cross validation sample of the parameter combinations iterated during grid search as a dataframe}
#' @return \item{error_tab_summary}{error summary for each combination of parameters as a dataframe}
#' @return \item{best_kernel}{kernel parameter of the optimal solution}
#' @return \item{best_degree}{degree parameter of the optimal solution}
#' @return \item{best_gamma}{gamma parameter of the optimal solution}
#' @return \item{best_cost}{cost parameter of the optimal solution}
#' @return \item{runtime}{runtime of the entire process}
#' @examples data <- iris
#' set.seed(11)
#' data$Y <- sample(0:1,size=nrow(data),replace=TRUE)
#' svm_params_list <- support_vector_parameters(base = data,target = "Y",gamma = 0.1,
#'                    cost = 0.1,kernel = "radial")
#' svm_params_list$error_tab_detailed
#' svm_params_list$error_tab_summary
#' svm_params_list$best_kernel
#' svm_params_list$best_degree
#' svm_params_list$best_gamma
#' svm_params_list$best_cost
#' svm_params_list$runtime
#' @author Arya Poddar <aryapoddar290990@gmail.com>
#' @import e1071
#' @import sqldf
#' @export
support_vector_parameters <- function(base,target,scale=T,kernel,degree=2,gamma,cost,error="rmse",cv=1){
  t <- which(names(base)==target)
  names(base)[t] <- "Target"
  error_tab <- data.frame("kernel"=NA,"degree"=NA,"gamma"=NA,"cost"=NA,
                          "mean_abs_error"=NA,"mean_sq_error"=NA,"root_mean_sq_error"=NA)
  rown <- 1
  cross_index_list <- fn_cross_index(base = base,k = cv)

  for(v in 1:cv){print(v)
    if(cv>1){data_train <- base[-unlist(cross_index_list[v]),]
    data_test <- base[unlist(cross_index_list[v]),]} else {data_train <- data_test <- base}
    for(i in 1:length(kernel))
      for(j in 1:length(degree))
        for(k in 1:length(gamma))
          for(l in 1:length(cost)){
            Sys.time() -> start
            model <- svm(as.factor(Target)~.,data=data_train,scale=scale,
                         kernel=kernel[i],degree=degree[j],gamma=gamma[k]/ncol(data_train),cost=cost[l])

            data_test$pred <- predict(model,newdata = data_test,type = "response")
            fn_error_list <- fn_error(base = data_test,observed_col = "Target",predicted_col = "pred")
            error_tab[rown,1:7] <- c(kernel[i],degree[j],gamma[k],cost[l],unlist(fn_error_list))
            error_tab[rown,"cv"] <- v
            error_tab[rown,"runtime"] <- Sys.time() - start
            rown <- rown+1}
  }

  error_tab2 <- sqldf('select distinct kernel,degree,gamma,cost,avg(mean_abs_error) as mean_abs_error,
                      avg(mean_sq_error) as mean_sq_error,avg(root_mean_sq_error) as root_mean_sq_error,
                      sum(runtime) as runtime_sec from error_tab group by kernel,degree,gamma,cost')

  if(error=="mae") best_par <- error_tab2[which.min(error_tab2$mean_abs_error),]
  if(error=="mse") best_par <- error_tab2[which.min(error_tab2$mean_s_error),]
  if(error=="rmse") best_par <- error_tab2[which.min(error_tab2$root_mean_sq_error),]

  l <- list(error_tab_detailed=error_tab, error_tab_summary=error_tab2,
            best_kernel=best_par$kernel, best_degree=best_par$degree,
            best_gamma=best_par$gamma, best_cost=best_par$cost, runtime=sum(error_tab2$runtime_sec))
  return(l)
}

#' @name random_forest_parameters
#' @title Hyperparameter optimisation or parameter tuning for Random Forest by grid search
#' @description The function runs a grid search with k-fold cross validation to arrive at best parameter decided by some performance measure. The parameters that can be tuned using this function for random forest algorithm are - ntree, mtry, maxnodes and nodesize. The objective function to be minimised is the error (mean absolute error / mean squared error / root mean squared error). For the grid search, the possible values of each tuning parameter needs to be passed as an array into the function.
#' @param base input dataframe
#' @param target column / field name for the target variable to be passed as string (must be 0/1 type)
#' @param model_type to be chosen among "regression" or "classification"
#' @param ntree number of trees to be fitted
#' @param mtry number of variable to be sampled as split criteria at each node
#' @param maxnodes (optional) Maximum number of terminal nodes (default is NULL ie. no restriction on depth of the trees)
#' @param nodesize minimum size of terminal nodes
#' @param error (optional) error measure as objective function to be minimised, to be chosen among "mae", "mse" and "rmse" (default value is "rmse")
#' @param cv (optional) k vakue for k-fold cross validation to be performed (default value is 1 ie. without cross validation)
#' @return An object of class "random_forest_parameters" is a list containing the following components:
#' @return \item{error_tab_detailed}{error summary for each cross validation sample of the parameter combinations iterated during grid search as a dataframe}
#' @return \item{error_tab_summary}{error summary for each combination of parameters as a dataframe}
#' @return \item{best_ntree}{ntree parameter of the optimal solution}
#' @return \item{best_mtry}{mtry parameter of the optimal solution}
#' @return \item{maxnodes}{maxnodes parameter of the optimal solution}
#' @return \item{best_nodesize}{nodesize parameter of the optimal solution}
#' @return \item{runtime}{runtime of the entire process}
#' @examples data <- iris
#' set.seed(11)
#' data$Y <- sample(0:1,size=nrow(data),replace=TRUE)
#' rf_params_list <- random_forest_parameters(base = data,target = "Y",
#'                   model_type = "classification",ntree = 2,mtry = 1,nodesize = 3)
#' rf_params_list$error_tab_detailed
#' rf_params_list$error_tab_summary
#' rf_params_list$best_ntree
#' rf_params_list$best_mtry
#' rf_params_list$maxnodes
#' rf_params_list$best_nodesize
#' rf_params_list$runtime
#' @author Arya Poddar <aryapoddar290990@gmail.com>
#' @author Aiana Goyal <aianagoel002@gmail.com>
#' @import randomForest
#' @import sqldf
#' @export
random_forest_parameters <- function(base,target,model_type,ntree,mtry,maxnodes=NULL,nodesize,error="rmse",cv=1){
  t <- which(names(base)==target)
  names(base)[t] <- "Target"

  counter <- 0
  if(model_type %in% c("regression","classification")==F) {
    print ("invalid model_type, must be 'regression' or 'classification'")
    counter <- counter+1}

  if(is.factor(base$Target)==T&model_type=="regression") {
    print ("target should not be factor if regression tree is to be run")
    counter <- counter+1}

  if(counter==0){error_tab <- data.frame("ntree"=NA,"mtry"=NA,"maxnodes"=NA,"nodesize"=NA,
                                         "mean_abs_error"=NA,"mean_sq_error"=NA,"root_mean_sq_error"=NA)
  rown <- 1

  cross_index_list <- fn_cross_index(base = base,k = cv)

  for(i in setdiff(1:ncol(base),t))
    if(typeof(base[,i])=='character') base[,i] <- as.factor(base[,i])

  for(v in 1:cv){
    if(cv>1){data_train <- base[-unlist(cross_index_list[v]),]
    data_test <- base[unlist(cross_index_list[v]),]} else {data_train <- data_test <- base}
    for(i in 1:length(ntree))
      for(j in 1:length(mtry))
        for(k in 1:max(length(maxnodes),1))
          for(l in 1:length(nodesize)){
            if(model_type=="classification") {
              Sys.time() -> start
              model <- randomForest(as.factor(Target)~.,data=data_train,ntree=ntree[i],mtry=mtry[j],
                                    maxnodes=maxnodes[k],nodesize=nodesize[l])} else {
                                      Sys.time() -> start
                                      model <- randomForest(Target~.,data=data_train,ntree=ntree[i],mtry=mtry[j],
                                                            maxnodes=maxnodes[k],nodesize=nodesize[l])}

            data_test$pred <- predict(model,newdata = data_test,type = "response")
            fn_error_list <- fn_error(base = data_test,observed_col = "Target",predicted_col = "pred")
            error_tab[rown,1:7] <- c(ntree[i],mtry[j],ifelse(length(maxnodes)>0,maxnodes[k],"max_depth"),nodesize[l],unlist(fn_error_list))
            error_tab[rown,"cv"] <- v
            error_tab[rown,"runtime"] <- Sys.time() - start
            rown <- rown+1}
  }

  error_tab2 <- sqldf('select distinct ntree,mtry,maxnodes,nodesize,avg(mean_abs_error) as mean_abs_error,
                      avg(mean_sq_error) as mean_sq_error,avg(root_mean_sq_error) as root_mean_sq_error,
                      sum(runtime) as runtime_sec from error_tab group by ntree,mtry,maxnodes,nodesize')

  if(error=="mae") best_par <- error_tab2[which.min(error_tab2$mean_abs_error),]
  if(error=="mse") best_par <- error_tab2[which.min(error_tab2$mean_s_error),]
  if(error=="rmse") best_par <- error_tab2[which.min(error_tab2$root_mean_sq_error),]

  l <- list(error_tab_detailed=error_tab, error_tab_summary=error_tab2,
            best_ntree=best_par$ntree, best_mtry=best_par$mtry,best_maxnodes=best_par$maxnodes,
            best_nodesize=best_par$nodesize, runtime=sum(error_tab2$runtime_sec))
  return(l)}
}

#' @name gradient_boosting_parameters
#' @title Hyperparameter optimisation or parameter tuning for Gradient Boosting Regression Modelling by grid search
#' @description The function runs a grid search with k-fold cross validation to arrive at best parameter decided by some performance measure. The parameters that can be tuned using this function for gradient boosting regression modelling algorithm are - ntree, depth, shrinkage, min_obs and bag_fraction. The objective function to be minimised is the error (mean absolute error / mean squared error / root mean squared error). For the grid search, the possible values of each tuning parameter needs to be passed as an array into the function.
#' @param base input dataframe
#' @param target column / field name for the target variable to be passed as string (must be 0/1 type)
#' @param ntree number of trees to be fitted
#' @param depth maximum depth of variable interactions
#' @param shrinkage learning rate
#' @param min_obs minimum size of terminal nodes
#' @param bag_fraction fraction of the training set observations randomly selected for next tree
#' @param error (optional) error measure as objective function to be minimised, to be chosen among "mae", "mse" and "rmse" (default value is "rmse")
#' @param cv (optional) k vakue for k-fold cross validation to be performed (default value is 1 ie. without cross validation)
#' @return An object of class "gradient_boosting_parameters" is a list containing the following components:
#' @return \item{error_tab_detailed}{error summary for each cross validation sample of the parameter combinations iterated during grid search as a dataframe}
#' @return \item{error_tab_summary}{error summary for each combination of parameters as a dataframe}
#' @return \item{best_ntree}{ntree parameter of the optimal solution}
#' @return \item{best_depth}{depth parameter of the optimal solution}
#' @return \item{best_shrinkage}{shrinkage parameter of the optimal solution}
#' @return \item{best_min_obs}{cost min_obs of the optimal solution}
#' @return \item{best_bag_fraction}{bag_fraction parameter of the optimal solution}
#' @return \item{runtime}{runtime of the entire process}
#' @examples data <- iris
#' set.seed(11)
#' data$Y <- sample(0:1,size=nrow(data),replace=TRUE)
#' gbm_params_list <- gradient_boosting_parameters(base = data,target = "Y",ntree = 2,depth = 2,
#'                    shrinkage = 0.1,min_obs = 0.1,bag_fraction = 0.7)
#' gbm_params_list$error_tab_detailed
#' gbm_params_list$error_tab_summary
#' gbm_params_list$best_ntree
#' gbm_params_list$best_depth
#' gbm_params_list$best_shrinkage
#' gbm_params_list$best_min_obs
#' gbm_params_list$best_bag_fraction
#' gbm_params_list$runtime
#' @author Arya Poddar <aryapoddar290990@gmail.com>
#' @import gbm
#' @import sqldf
#' @export
gradient_boosting_parameters <- function(base,target,ntree,depth,shrinkage,min_obs,bag_fraction,error="rmse",cv=1){
  t <- which(names(base)==target)
  names(base)[t] <- "Target"
  error_tab <- data.frame("ntree"=NA,"depth"=NA,"shrinkage"=NA,"min_obs"=NA,"bag_fraction"=NA,
                          "mean_abs_error"=NA,"mean_sq_error"=NA,"root_mean_sq_error"=NA)
  rown <- 1
  cross_index_list <- fn_cross_index(base = base,k = cv)

  for(v in 1:cv){
    if(cv>1){data_train <- base[-unlist(cross_index_list[v]),]
    data_test <- base[unlist(cross_index_list[v]),]} else {data_train <- data_test <- base}
    for(i in 1:length(ntree))
      for(j in 1:length(depth))
        for(k in 1:length(shrinkage))
          for(l in 1:length(min_obs))
            for(m in 1:length(bag_fraction)){
              Sys.time() -> start
              model <- gbm(Target~.,data=data_train,distribution='bernoulli',
                           n.trees=ntree[i],interaction.depth=depth[j],shrinkage=shrinkage[k],
                           n.minobsinnode=min_obs[l],bag.fraction=bag_fraction[m])

              data_test$pred <- predict(model,newdata = data_test,type = "response",n.trees = ntree[i])
              fn_error_list <- fn_error(base = data_test,observed_col = "Target",predicted_col = "pred")
              error_tab[rown,1:8] <- c(ntree[i],depth[j],shrinkage[k],min_obs[l],bag_fraction[m],unlist(fn_error_list))
              error_tab[rown,"cv"] <- v
              error_tab[rown,"runtime"] <- Sys.time() - start
              rown <- rown+1}
  }

  error_tab2 <- sqldf('select distinct ntree,depth,shrinkage,min_obs,bag_fraction,avg(mean_abs_error) as mean_abs_error,
                      avg(mean_sq_error) as mean_sq_error,avg(root_mean_sq_error) as root_mean_sq_error,
                      sum(runtime) as runtime_sec from error_tab group by ntree,depth,shrinkage,min_obs,bag_fraction')

  if(error=="mae") best_par <- error_tab2[which.min(error_tab2$mean_abs_error),]
  if(error=="mse") best_par <- error_tab2[which.min(error_tab2$mean_s_error),]
  if(error=="rmse") best_par <- error_tab2[which.min(error_tab2$root_mean_sq_error),]

  l <- list(error_tab_detailed=error_tab, error_tab_summary=error_tab2,
            best_ntree=best_par$ntree, best_depth=best_par$depth, best_shrinkage=best_par$shrinkage,
            best_min_obs=best_par$min_obs, best_bag_fraction=best_par$bag_fraction, runtime=sum(error_tab2$runtime_sec))
  return(l)
}
