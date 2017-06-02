# booleanR

Complex boolean string match function for R

### Dependencies

- stringr
- stringi

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
bool_detect(x, "fo*zy"), TRUE,
bool_detect(x, "cat OR -(fox)"), FALSE,
bool_detect(x, "-(bird)"),  TRUE,
bool_detect(c(x,x,x), 'dog'), TRUE)

# compare all evenly positioned logical values with their odd predecessor
all(tst[ 2*1:(length(tst)/2) ] == tst[ 2*1:(length(tst)/2) - 1 ])
[1] TRUE
```