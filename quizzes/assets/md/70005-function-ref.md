# Appendix: Filter reference


```{r,echo=F,warning=FALSE,message=FALSE}
if(T){
  library(rvest)
  library(xml2)
  library(tidyverse)
library(htmltools)
  content <- read_html("https://stevepowell99.github.io/CausalMapFunctions/articles/examples.html?version=1234") %>%  html_nodes(css="#main")%>% 
   html_children

  # content <- read_html("https://guide.causalmap.app/x5.html") %>%
  # html_nodes(css="#content")  %>%
  #  html_children

  toRemove <- content %>% rvest::html_nodes(css = "h1")
  xml_remove(toRemove)

  content  %>%
  write_lines("tmp.html")

  #content <- xml2::read_html(x)

# 
#   
#      as.character  %>%
# 
#   paste0(collapse="</br>") %>%
#   pluck(1) %>%
#   cat

} 

if(F)htmltools::includeHTML("tmp.html")

if(F)htmltools::includeHTML("C:/Users/steve/Dropbox/Projects/CausalMapFunctions/vignettes/fragment.html")

# still doesn't work because these are html widgets
```

````{=html}
```{r, echo=FALSE, results='asis'}
if(F)xfun::file_string('tmp.html')
```
````

The list can be found here: [github](https://stevepowell99.github.io/CausalMapFunctions/articles/examples.html).



