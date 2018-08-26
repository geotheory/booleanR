require(stringr)
require(stringi)
require(dplyr)


bool_detect = function(x, qry, ignore_case = TRUE, print_call = FALSE){
  ops = c(AND = '&', `&` = '&', OR = '|', `|` = '|', `(` = '(', `)` = ')')  # boolean operators
  # add spaces around parentheses;
  qry = qry %>% str_trim %>% str_replace_all(fixed('('), '( ') %>% str_replace_all(fixed(')'), ' )') %>%
    str_replace_all("'", '"') %>% # enforce double quotes
    str_replace_all('(?<="[^ ]{0,100}) (?=[^ ]+")', '\u00A0') # sub spaces in quoted text for a unicode space
  # split elements by ascii space character
  comps = str_split(qry, '[ ]+')[[1]] %>% str_replace_all('\u00A0', ' ') %>% str_remove_all('"')
  comp_ops = comps %in% names(ops)  # identify operators from query terms
  # construct string for a composite logical call to evaluate
  to_negate = str_detect(comps[!comp_ops], '^-') # args to negate (NOT)
  qry_neg = comps[!comp_ops] %>% str_replace('^-', '')
  # build a str_detect command for each argument, negated as required
  comps[!comp_ops] = paste0(c('','!')[to_negate+1], 'str_detect(x, regex("',
                            qry_neg, '", ignore_case=', ignore_case,'))')
  comps[comp_ops] = ops[comps[comp_ops]]  # convert AND/OR to R logical operators
  qry_final = paste(comps, collapse = ' ')  # reduce to single string
  if(print_call) message(qry_final)
  eval(parse(text = qry_final))
}


bool_detect2 = function(x, qry, ignore_case = TRUE, in_word = TRUE, full_word = FALSE, print_call = FALSE){
  b = qry %>% str_trim() %>% stri_replace_all(fixed = '?', '.') %>% stri_replace_all(fixed = '*', ifelse(in_word, '[\\\\w]*', '.*'))

  # single search term
  if(!stri_detect(b, regex = '[\\s\\(\\)\\&\\|]')){ # any space or logical char?
    b = paste0("str_detect(x, regex('", b, "', ignore_case=", ignore_case, "))")
    if(print_call) print(b)
    return(eval(parse(text = b)))
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
    item = stri_extract_last(b, regex = '\\b[^\\s\\(\\)\\&\\|]+') # whole word not inc logicals
    if(!is.na(item) & item != ''){
      posn = stri_locate_last(b, fixed = item)[1,] # position of last search term
      orig_item = str_replace_all(item, sep, ' ')
      subs[length(subs)+1] = list(data.frame(start = posn[1], end = posn[2], new_str = paste0("str_detect(x, regex('", orig_item, "', ignore_case=", ignore_case, "))"), stringsAsFactors = FALSE))
      b = substr(b, 1, posn[1] - 2) # truncate boolean
      i = posn[1] - 1
    } else i = i - 1
  }

  subs = subs %>% bind_rows() # to data.frame

  # convert terms into own str_detect calls
  for(i in 1:nrow(subs)){
    b_head = substr(b0, 1, subs$start[i]-1)
    b_tail = substr(b0, subs$end[i]+1, nchar(b0))
    b0 = paste0(b_head, subs$new_str[i], b_tail)
  }

  b1 = stri_replace_all(b0, regex = '-[ ]*[(]', '!(') # invert logic for negatives
  if(print_call) print(b1)
  eval(parse(text = b1)) # evaluate constructed string, maintaining parenthesis logical structure
}


bool_filter_db = function(db_tbl, col, qry, print_call=FALSE){
  ops = c(AND = '&', `&` = '&', OR = '|', `|` = '|', `(` = '(', `)` = ')')  # boolean operators
  # add spaces around parentheses;
  qry = qry %>% str_trim %>% str_replace_all(fixed('('), '( ') %>% str_replace_all(fixed(')'), ' )') %>%
    str_replace_all("'", '"') %>% # enforce double quotes
    str_replace_all('(?<="[^ ]{0,100}) (?=[^ ]+")', '\u00A0') # sub spaces in quoted text for a unicode space
  # split elements by ascii space character
  comps = str_split(qry, '[ ]+')[[1]] %>% str_replace_all('\u00A0', ' ') %>% str_remove_all('"')
  comp_ops = comps %in% names(ops)  # identify operators from query terms
  # construct string for a composite logical call to evaluate
  to_negate = str_detect(comps[!comp_ops], '^-') # args to negate (NOT)
  qry_neg = comps[!comp_ops] %>% str_replace('^-', '')
  # build a str_detect command for each argument, negated as required
  comps[!comp_ops] = paste0(c('','!')[to_negate+1], 'str_detect(', col, ', \'', qry_neg, '\')')
  comps[comp_ops] = ops[comps[comp_ops]]
  qry_final = paste0('filter(', deparse(substitute((db_tbl))), ',', paste(comps, collapse = ' '), ')')
  if(print_call) message(qry_final)
  eval(parse(text = qry_final))
}
