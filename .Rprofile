message("Hi Sexy Modellers! How do you feel today?")
message("Please remember to update all the scripts by pulling from the repository.")

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