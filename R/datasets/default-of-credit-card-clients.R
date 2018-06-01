load_dccc_df <- function() {
  df <- data.frame(read.csv('data/default-of-credit-card-clients.csv', head=TRUE, sep=','))

  colnames(df) <- c(
    "ID",
    "LIMIT_BAL",
    "SEX",
    "EDUCATION",
    "MARRIAGE",
    "AGE",
    "PAY_0",
    "PAY_2",
    "PAY_3",
    "PAY_4",
    "PAY_5",
    "PAY_6",
    "BILL_AMT1",
    "BILL_AMT2",
    "BILL_AMT3",
    "BILL_AMT4",
    "BILL_AMT5",
    "BILL_AMT6",
    "PAY_AMT1",
    "PAY_AMT2",
    "PAY_AMT3",
    "PAY_AMT4",
    "PAY_AMT5",
    "PAY_AMT6",
    "DEFAULT_PAY"
  )

  df$SEX <- factor(df$SEX, labels=c("male", "female"))
  df$EDUCATION <- factor(df$EDUCATION, labels=c("graduate school", "university", "high school", "other1", "other2", "other3", "other4"))
  df$MARRIAGE <- factor(df$MARRIAGE, labels=c("married", "single", "other1", "other2"))
  df$DEFAULT_PAY <- factor(df$DEFAULT_PAY)
  df[,6:11] <- lapply(df[,6:11], as.factor)

  return(df)
}