#' @title Titanic data
#'
#' @description A \code{dataframe} that is for testing and running of examples. 
#'
#' @docType data
#' 
#' @usage data(myData)
#' 
#' @keywords datasets
#' 
#' @source https://cran.r-project.org/web/packages/titanic/index.html
#'
#' @format A \code{dataframe} with 891 rows and 8 colummns/variables which are:
#' 
#' \itemize{
#' \item Survived: A binary variable telling whether someone survived or not:
#' \itemize{
#' \item 0 = No
#' \item 1 = Yes
#' }
#' \item PClass: A categorical variable telling passenger class:
#'   \itemize{
#'  \item 1st = Upper
#'  \item 2nd = Middle
#'  \item 3rd = Lower
#'  }
#' \item Sex: Gender of passenger:
#' \itemize{ 
#' \item male
#' \item female
#' }
#' \item Age: Age of individual
#' \item SibSp: Number of Siblings/Spouses Aboard:
#' \itemize{
#' \item Sibling = brother, sister, stepbrother, stepsister
#' \item Spouse = husband, wife (mistresses and fiancés were ignored)
#' }
#' \item Parch: Number of Parents/Children Aboard:
#' \itemize{
#' \item Parent = mother, father
#' \item Child = daughter, son, stepdaughter, stepson
#' \item Some children travelled only with a nanny, therefore parch=0 for them
#' }
#' \item Fare: Price that individual had to pay for the ticket
#' \item Embarked: Port of Embarkation:
#' \itemize{
#' \item C = Cherbourg
#' \item Q = Queenstown
#' \item S = Southampton
#' }
#' }

"myData" 
