# booleanR

Complex boolean string match function for R

### Dependencies

- stringr
- stringi
- dplyr

### Arguments

    bool_detect = function(x, b, ignore_case = TRUE, in_word = TRUE, full_word = FALSE, print.call = FALSE){...}

- `x` - a character string or vector of texts to search
- `b` - a character string boolean search term
- `ignore_case` - ignore case when matching
- `in_word` - Wildcard matching only within whole word (FALSE matches across whole document)
- `full_word` - [not implemented] Forces wildcard matching against whole words (FALSE allows matching within words)
- `print.call`  - Print the final command call to console

### Examples & testing

```
x = "The quick fox jumps over the lazy dog"

tst = c(bool_detect(x, 'dog'), TRUE,
bool_detect(x, 'bird'), FALSE,
bool_detect(x, 'dog OR bird'), TRUE,
bool_detect(x, 'dog AND bird'), FALSE,
bool_detect(x, 'dog   AND   bird'), FALSE,
bool_detect(x, ' dog   AND   bird  '), FALSE,
bool_detect(x, 'dog & bird'), FALSE,
bool_detect(x, 'dog | bird'), TRUE,
bool_detect(x, 'dog & fox'), TRUE,
bool_detect(x, 'dog AND fox'), TRUE,
bool_detect(x, 'dog & (fox | cat)'), TRUE,
bool_detect(x, "'lazy dog'"), TRUE,
bool_detect(x, '"lazy fox"'), FALSE,
bool_detect(x, "fox AND 'lazy dog'"), TRUE,
bool_detect(x, "('pretty poly' OR bird) OR (quick AND (squirrel OR fox))"), TRUE,
bool_detect(x, "f?x"), TRUE,
bool_detect(x, 'fox*lazy', in_word = F), TRUE,
bool_detect(x, 'fox*lazy', in_word = T), FALSE,
bool_detect(x, '"fox * lazy"', in_word = F), TRUE,
bool_detect(x, '"fox * lazy"', in_word = T), FALSE,
bool_detect(x, '"fox " AND " lazy"'), TRUE,
bool_detect(x, "cat OR -(fox)"), FALSE,
bool_detect(x, "-(bird)"),  TRUE,
bool_detect(c(x,x,x), 'dog'), TRUE)

# compare all evenly positioned logical values with their odd predecessor
all(tst[ 2*1:(length(tst)/2) ] == tst[ 2*1:(length(tst)/2) - 1 ])
[1] TRUE
```
