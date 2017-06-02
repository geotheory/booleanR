require(stringr)
require(stringi)

bool_detect = function(x, b, print.call = FALSE){
  b = b %>% str_trim() %>% stri_replace_all(fixed = '?', '.') %>% stri_replace_all(fixed = '*', '.*')

  # single search term
  if(stri_detect(b, regex = '^[^\\s\\(\\)\\&\\|]+$')){
    if(print.call) print(b)
    return(eval(parse(text = paste0("str_detect(x, '", b, "')"))))
  }

  # convert boolean logical operators to '&' and '|'
  b = b %>% str_replace_all(' AND ', ' & ') %>% str_replace_all(' OR ', ' | ')

  seps = c('_', '#','£','%', '~')
  sep = seps[!str_detect(b, seps)][1]

  # quoted text takes priority, so we will convert spaces to a temp delimiter to facilitate interpretation
  if(str_detect(b, '\'|"')){
    bx = str_split(b, '')[[1]] # vector of individual chars
    i = 1
    while(i < length(bx)){
      if(bx[i] %in% c('"', '\'')){
        q = bx[i]   # quote char
        bx = bx[-i] # remove
        while(bx[i] != q){
          if(bx[i] == ' ') bx[i] = sep
          i = i + 1
          if(i > length(bx)) stop('Unclosed quote error\nAre you sure you have closed your quoted text string?')
        }
        bx = bx[-i]
      } else i = i + 1
    }
    b = paste(bx, collapse='')
  }

  # iterate backwards through string to identify search terms, and log position/replacement details
  b0 = b
  subs = list()
  i = nchar(b)
  while(i > 0){
    #cat(i, '\n')
    item = stri_extract_last(b, regex = '\\b[^\\s\\(\\)\\&\\|]+') # whole word not inc logicals
    if(!is.na(item) & item != ''){
      posn = stri_locate_last(b0, fixed = item)[1,] # position of last search term
      orig_item = str_replace_all(item, sep, ' ')
      subs[length(subs)+1] = list(data.frame(start = posn[1], end = posn[2], new_str = paste0("str_detect(x, '", orig_item, "')"), stringsAsFactors = F))
      b = substr(b, 1, posn[1] - 2) # truncate boolean
      i = posn[1] - 1
    } else i = i - 1
  }

  subs = subs %>% bind_rows()
  b = b0

  # convert terms into own str_detect calls
  for(i in 1:nrow(subs)){
    b_head = substr(b, 1, subs$start[i]-1)
    b_tail = substr(b, subs$end[i]+1, nchar(b))
    b = paste0(b_head, subs$new_str[i], b_tail)
  }
  if(print.call) print(b)
  b = stri_replace_all(b, regex = '-[ ]*[(]', '!(') # invert logic for negatives
  eval(parse(text = b)) # evaluate constructed string, maintaining parenthesis logical structure
}
