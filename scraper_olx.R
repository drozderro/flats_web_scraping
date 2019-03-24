library(rvest)
library(tidyverse)
library(stringr)


content_text <- function(webpage, css_sel){
  webpage %>% 
    html_nodes(css_sel) %>% 
    html_text()
}

# parsing and formatting of table containg details about offer
olx_tab <- function(webpage, css_sel = '.details'){
  tab <- webpage %>% 
    html_nodes('.details') %>%
    html_table(header= F, fill = T) %>% 
    .[[1]] %>% 
    as_tibble() %>% 
    select(X1, X2) %>% 
    filter(!(str_detect(X1, '\\n\t') | str_length(X1) == 0)) %>% 
    spread(X1, X2)
  
  tab$`Czynsz (dodatkowo)` <- tab$`Czynsz (dodatkowo)` %>% 
    str_extract('[:digit:]+,[:digit:]+|[:digit:]+') %>% 
    parse_double(locale = locale(decimal_mark = ','))
  
  tab$Powierzchnia <- tab$Powierzchnia %>% 
    str_extract('[:digit:]+,[:digit:]+|[:digit:]+') %>% 
    parse_double(locale = locale(decimal_mark = ','))
  
  result <- tibble('Czynsz' = if(is_empty(tab$`Czynsz (dodatkowo)`)){NA}
                              else tab$`Czynsz (dodatkowo)`,
                   'Liczba pokoi' = if(is_empty(tab$`Liczba pokoi`)){NA}
                              else tab$`Liczba pokoi`,
                   'Oferta od' = if(is_empty(tab$`Oferta od`)){NA}
                              else tab$`Oferta od`,
                   'Powierzchnia' = if(is_empty(tab$Powierzchnia)){NA}
                              else tab$Powierzchnia,
                   'Poziom' = if(is_empty(tab$Poziom)){NA}
                              else tab$Poziom,
                   'Zabudowa' = if(is_empty(tab$`Rodzaj zabudowy`)){NA}
                              else tab$`Rodzaj zabudowy`,
                   'Meble' = if(is_empty(tab$Umeblowane)){NA}
                              else tab$Umeblowane)
  
  return(result)
}

# function scraps information from flat renting offers on OLX.PL 
scrap_offer <- function(url){
  webpage <- url %>% read_html()
  
  id <- webpage %>% 
    content_text('.offer-titlebox__details > em:nth-child(2) > small:nth-child(1)') %>% 
    parse_number() 
  if(is_empty(id)){id <- NA}
  
  creation_date <- webpage %>% 
    content_text('.offer-titlebox__details > em:nth-child(2)') %>% 
    str_extract('[:digit:]{1,2}\\s[:alpha:]+\\s[:digit:]{4}') %>% 
    parse_date('%d %B %Y', locale = locale('pl'))
  if(is_empty(creation_date)){creation_date <- NA}
  
  name <- webpage %>%  
    content_text('.offer-titlebox > h1:nth-child(1)') %>% 
    str_replace_all('[:space:]', ' ') %>% 
    str_trim()
  if(is_empty(name)){name <- NA}
  
  price <- webpage %>% 
    content_text('.price-label > strong:nth-child(1)') %>% 
    str_replace('[:space:]', '') %>% 
    str_extract('[:digit:]+') %>% 
    as.numeric()
  if(is_empty(price)){price <- NA}
    
  district <- webpage %>% 
    content_text('a.show-map-link:nth-child(1) > strong:nth-child(1)') %>% 
    str_split(',') %>% 
    { .[[1]][3] } %>% 
    str_trim()
  if(is_empty(district)){district <- NA}
  
  discription <- webpage %>% 
    content_text('#textContent') %>% 
    str_replace_all('[:space:]', ' ') %>% 
    str_trim()
  if(is_empty(discription)){discription <- NA}
  
  user <- webpage %>% 
     content_text('.offer-user__details > h4:nth-child(1)') %>% 
     str_replace_all('[:space:]', ' ') %>% 
     str_trim()
  if(is_empty(user)){user <- NA}
  
  views <- webpage %>% 
    content_text('div.pdingtop10:nth-child(3) > strong:nth-child(1)') %>% 
    as.integer()
  if(is_empty(views)){views <- NA}
  
  result <- tibble(id ,
                   creation_date ,
                   name ,
                   price ,
                   district ,
                   discription ,
                   user ,
                   views)
  
  tmp <- olx_tab(webpage)
  result <- result %>% cbind(tmp)
  
  return(result)
}

# function allows to determin how many pages with offers are ther
num_of_offer_sites <- function(webpage){
  webpage %>% 
    # css for bar with numbers at the bottom of page
    html_nodes('.pager') %>%
    html_text() %>% 
    # extraxting numbers from that bar
    str_extract_all('[:space:][:digit:]+[:space:]') %>% 
    unlist() %>% 
    # converting to ints
    as.integer() %>% 
    # selecting max
    max()
}


url_start <- 'https://www.olx.pl/nieruchomosci/mieszkania/wynajem/wroclaw'
webpage_start <- url_start %>% 
  read_html()
max_site_number <- num_of_offer_sites(webpage_start)
offers <- tibble()
for( site_number in 1:max_site_number){
  print(site_number)
  url <- str_c('https://www.olx.pl/nieruchomosci/mieszkania/wynajem/wroclaw/?page=', as.character(site_number))
  webpage <- url %>% 
    read_html()
  
  links <- webpage %>% 
    html_nodes('tbody:nth-child(1) > tr:nth-child(1) > td:nth-child(2) > div:nth-child(1) > h3:nth-child(1) > a:nth-child(1)') %>%
    html_attr('href')
  
  links <- links[links %>% str_detect('https://www.olx.pl/oferta/')]

  for(link in links){
    tmp <- scrap_offer(link)
    offers <- offers %>% rbind(tmp)
  }
}
### POLIGON

## dodaæ info o tym, czy oferta jest wyró¿niona

webpage <- links[23] %>% read_html()
  
  webpage %>% 
  content_text('.offer-titlebox__details > em:nth-child(2) > small:nth-child(1)') %>%
  parse_number()

  webpage %>% 
  content_text('.offer-titlebox__details > em:nth-child(2)') %>% 
  str_extract('[:digit:]{1,2}\\s[:alpha:]+\\s[:digit:]{4}') %>% 
  parse_date('%d %B %Y', locale = locale('pl'))

webpage %>%  
  content_text('.offer-titlebox > h1:nth-child(1)') %>% 
  str_replace_all('[:space:]', ' ') %>% 
  str_trim()

webpage %>% 
  content_text('.price-label > strong:nth-child(1)') %>% 
  str_replace('[:space:]', '') %>% 
  str_extract('[:digit:]+') %>% 
  as.numeric()

 webpage %>% 
  content_text('a.show-map-link:nth-child(1) > strong:nth-child(1)') %>% 
  str_split(',') %>% 
  { .[[1]][3] } %>% 
  str_trim()

webpage %>% 
  content_text('#textContent') %>% 
  str_replace_all('[:space:]', ' ') %>% 
  str_trim()

# something wrong with 'user' - needs closer look  
# v_user <- webpage %>% 
#   content_text('.offer-user__details > h4:nth-child(1)') %>% 
#   str_replace_all('[:space:]', ' ') %>% 
#   str_trim()
# 
webpage %>% 
  content_text('div.pdingtop10:nth-child(3) > strong:nth-child(1)') %>% 
  as.integer()

result <- tibble(id ,
                 creation_date ,
                 name ,
                 price ,
                 district ,
                 discription ,
                 user ,
                 views)

olx_tab(webpage)


tab <- webpage %>% 
  html_nodes('.details') %>%
  html_table(header= F, fill = T) %>% 
  .[[1]] %>% 
  as_tibble() %>% 
  select(X1, X2) %>% 
  filter(!(str_detect(X1, '\\n\t') | str_length(X1) == 0)) %>% 
  spread(X1, X2)

tab$`Czynsz (dodatkowo)` <- tab$`Czynsz (dodatkowo)` %>% 
  str_extract('[:digit:]+,[:digit:]+|[:digit:]+') %>% 
  parse_double(locale = locale(decimal_mark = ','))

tab$Powierzchnia <- tab$Powierzchnia %>% 
  str_extract('[:digit:]+,[:digit:]+|[:digit:]+') %>% 
  parse_double(locale = locale(decimal_mark = ','))

result <- tibble('Czynsz' = if(is_empty(tab$`Czynsz (dodatkowo)`)){NA} else tab$`Czynsz (dodatkowo)`,
                 'Liczba pokoi' = if(is_empty(tab$`Liczba pokoi`)){NA},
                 'Oferta od' = if(is_empty(tab$`Oferta od`)){NA},
                 'Powierzchnia' = if(is_empty(tab$Powierzchnia)){NA},
                 'Poziom' = if(is_empty(tab$Poziom)){NA},
                 'Zabudowa' = if(is_empty(tab$`Rodzaj zabudowy`)){NA},
                 'Meble' = if(is_empty(tab$Umeblowanie)){NA})
