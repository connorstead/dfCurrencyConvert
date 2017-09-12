#' Column Currency Conversion
#'
#' This function converts currencies in a data frame. It requires currency codes to be in a seperate column than the currency values.
#' @param df The data frame containing the values to be converted.
#' @param col_code The name of the column in the data frame containing the currency codes. Ensure you wrap the name of the column in "".
#' @param col_values The name of the column in the data frame containing the currency codes. Ensure you wrap the name of the column in "".
#' @param key Your openexchangerates.org API Key. Ensure you wrap the API key "".
#' @keywords lucr, currency, convert
#' @export
#' @examples
#' col_currency_convert()
col_currency_convert <-
  function(df, col_code, col_value, to = "USD", key){
    require(lucr)
    convertcolname = paste("converted", to, sep="_")
    for(i in 1:nrow(df)) 
    {
      '$'(df, convertcolname)[i] <- currency_convert(df[,col_value][i], from = paste0(df[,col_code][i]), to, key)
    }
    names(df)[names(df) == 'convertcolname'] <- convertcolname
    print(paste("Currency Converted @ ",date()))
    return(df)
  }