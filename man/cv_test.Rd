% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/functions.R
\name{cv_test}
\alias{cv_test}
\title{Cramer's V value between two categorical variables}
\usage{
cv_test(base, var_1, var_2)
}
\arguments{
\item{base}{input dataframe}

\item{var_1}{categorical variable name, to be passed as string}

\item{var_2}{categorical variable name, to be passed as string}
}
\value{
The function returns a dataframe with pairwise CV value.
}
\description{
The function gives the pairwise Cramer's V value between two input categorical variables.
}
\examples{
data <- iris
data$Species <- as.character(data$Species)
data$Sepal.Length <- as.character(floor(data$Sepal.Length))
data$Y <- sample(0:1,size=nrow(data),replace=TRUE)
cv_result <- cv_test(base = data,var_1 = "Species",var_2 = "Sepal.Length")
}
\author{
Arya Poddar <aryapoddar290990@gmail.com>
}
