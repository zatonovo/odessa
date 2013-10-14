# :vim set filetype=R

odessa.reset <- function() options(odessa.options=list())
if (!exists('odessa.options'))
  odessa.options <- OptionsManager('odessa.options')



