# load functions in the "functions" folder

for(ddd in dir(here::here('functions')))
  source(here::here(paste0("functions/",ddd)))
