message("Hi Sexy Modellers! How do you feel today?")

if(interactive()) 
  try(fortunes::fortune(), silent = TRUE)

.Last = function() {
  # this function will always be run at the end of the session and will try to
  # install the package fortunes, if it is not already installed
  cond = suppressWarnings(!require(fortunes, quietly = TRUE))
  if(cond) 
    try(install.packages("fortunes"), silent = TRUE)
  message("Goodbye at ", date(), "\n")
}