#' Perform a GRTS sampling on a database table via ODBC
#'
#' @param channel an ODBC channel to the database
#' @param tablename the name of the table in the database
#' @param grts.vars the name of the variabeles which define the GRTS sampling space. Typically an X and Y variable. However the only limitations are that all variables are numeric and the number of variables must be at least one.
#' @param cellsize the size of the smallest GRTS cell for each dimension.
#' @param samplesize the required sample size
#' @param availableLevels the number of available GRTS levels in the database. These must be named L01, L02, ... Currently available levels is limited to 99
#' @param reset logical value. If FALSE the current GRTS randomisation is completed. If TRUE a new GRTS randomisation will start.
#' @importFrom assertthat assert_that is.string noNA is.count is.flag
#' @importFrom DBI dbListTables dbGetQuery dbSendQuery dbClearResult
#' @importFrom dplyr %>%
#' @export
GRTS.ODBC <- function(channel, tablename, grts.vars, cellsize, samplesize,
                      availableLevels = 20, reset = FALSE){
  assert_that(inherits(channel, "DBIConnection"), is.string(tablename),
              noNA(tablename), inherits(grts.vars, "character"),
              tablename %in% dbListTables(channel), is.count(availableLevels),
              availableLevels < 100, is.flag(reset), noNA(reset))

  # calculate the central point

  sprintf("(Min(%1$s) + Max(%1$s)) / 2 AS mid_%1$s", grts.vars) %>%
    paste(collapse = ", ") %>%
    sprintf(fmt = "SELECT %s FROM %s", tablename) %>%
    dbGetQuery(conn = channel) -> Mid
  if (class(Mid) != "data.frame") {
    stop("Make sure that grts.vars points to existing columns in ",  tablename)
  }
  colnames(Mid) <- grts.vars

  #reset the GRTS sample
  if (reset) {
    seq_len(availableLevels) %>%
      sprintf(fmt = "L%02i = NULL") %>%
      paste(collapse = ", ") %>%
      sprintf(fmt = "UPDATE %2$s SET %1$s", tablename) %>%
      dbSendQuery(conn = channel) %>%
      dbClearResult()
  }

  #estimate the range (number of GRTS cells per dimension)
  sprintf("Max(%1$s) - Min(%1$s) AS range_%1$s", grts.vars) %>%
    paste(collapse = ", ") %>%
    sprintf(fmt = "SELECT %s FROM %s", tablename) %>%
    dbGetQuery(conn = channel) -> Range
  maxLevel <- ceiling(log2(max(Range / cellsize)))
  Range <- cellsize * 2 ^ maxLevel

  #the origin
  Low <- Mid - Range / 2

  seq_len(maxLevel) %>%
    rev() %>%
    sprintf(fmt = "L%02i") %>%
    paste(collapse = ", ") -> SQLsample
  seq_len(maxLevel) %>%
    rev() %>%
    sprintf(fmt = "ifnull(L%1$02i, 0)") %>%
    paste(collapse = ", ") -> SQLsample2
  sprintf(
    "SELECT Count(ID) AS Points, %s FROM %s GROUP BY %s ORDER BY %s LIMIT %i",
    SQLsample, tablename, SQLsample, SQLsample2, samplesize) -> SQL
  Sample <- dbGetQuery(conn = channel, SQL)
  while (any(is.na(Sample))) {
    Sample <- Sample[is.na(Sample[, 2]), ]
    junk <- apply(Sample[, -1], 1, FUN = GRTS.ODBC.Update, maxLevel = maxLevel,
                  grts.vars = grts.vars, tablename = tablename,
                  channel = channel, Low = Low, Range = Range)
    Sample <- dbGetQuery(conn = channel, SQL)
  }
  return(Sample)
}

GRTS.ODBC.Update <- function(thisSample, maxLevel, grts.vars, tablename, Low,
                             Range, channel){
  Level <- 1 + maxLevel - max(which(is.na(thisSample)))
  Sign <- sample(c(">=", "<"), length(grts.vars), replace = TRUE)
  Index <- sample(length(grts.vars)) - 1

  ifelse(
    is.na(thisSample),
    sprintf("%s IS NULL", names(thisSample)),
    sprintf("%s = %s", names(thisSample), thisSample)
  ) %>%
    paste(collapse = " AND ") -> whereStatement
  sprintf("AVG(%1$s) AS M%1$s", grts.vars) %>%
    paste(collapse = ", ") %>%
    sprintf(fmt = "SELECT %s FROM %s WHERE %s", tablename, whereStatement) %>%
    dbGetQuery(conn = channel) -> Mean
  Position <- ((Mean - Low) / Range) %/% 2 ^ (1 - Level)
  Mid <- Low + (Position + 0.5) * Range / 2 ^ (Level - 1)

  sprintf("%i * (%s %s %f)", 2 ^ Index, grts.vars, Sign, Mid) %>%
    paste(collapse = " - ") %>%
    sprintf(fmt = "UPDATE %2$s SET L%3$02i = %1$s WHERE %4$s",
            tablename, Level, whereStatement) %>%
    dbSendQuery(conn = channel) %>%
    dbClearResult()
}
