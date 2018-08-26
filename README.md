# booleanR

Complex boolean string match functions for R:

`bool_detect` is more elegant code and slightly quicker but possibly buggy.
`bool_detect2` is older and tested, and supports word-wise regex matching.

    bool_detect = function(x, qry, ignore_case = TRUE, print_call = FALSE){...}

    bool_detect2 = function(x, qry, ignore_case = TRUE, in_word = TRUE,
        full_word = FALSE, print_call = FALSE){...}

- `x` - a character string or vector of texts to search
- `qry` - a character string boolean search term
- `ignore_case` - ignore case when matching
- `in_word` - Wildcard matching only within whole word (FALSE matches across whole document)
- `full_word` - [not implemented] Forces wildcard matching against whole words (FALSE allows matching within words)
- `print_call`  - Print the final command call to console


`bool_filter_db` is a filter function compatible with `dplyr`'s remote connectivity (database) functionality. It currently doesn't support in-piping (`db_tbl %>% bool_filter_db(...)` will fail)

    bool_filter_db = function(db_tbl, col, qry, print_call=FALSE){...}

- `db_tbl` - a remote data table object - e.g. created with `tbl(con, "mytable")`
- `col` - name of volumn of `db_tbl` to query (string)
- `qry` - a character string boolean search term


### Examples

```
x = "The quick fox jumps over the lazy dog"

bool_detect(x, 'dog')
bool_detect(x, 'bird')
bool_detect(x, 'dog OR bird')
bool_detect(x, 'dog AND bird')
bool_detect(x, 'dog & (fox | cat)')
bool_detect(x, '"lazy fox"')
bool_detect(x, "fox AND 'lazy dog'")
bool_detect(x, "('pretty poly' OR bird) OR (quick AND (squirrel OR fox))")
bool_detect(x, "f?x")
bool_detect2(x, 'fox*lazy', in_word = F)
bool_detect2(x, 'fox*lazy', in_word = T)
bool_detect2(x, '"fox * lazy"', in_word = F)
bool_detect2(x, '"fox * lazy"', in_word = T)
bool_detect(x, '"fox " AND " lazy"')
bool_detect(x, "fox AND -'fox cubs'", print_call=T)
bool_detect(c(x,x,x), 'dog')
```

#### remote database filtering

```
require(RPostgreSQL)

drv <- dbDriver("PostgreSQL")
con <- dbConnect(drv, dbname = "my_database", host = "localhost",
                 port = 5432, user = "myusername")

copy_to(con, iris, "iris", temporary=F, indexes = list(names(iris)))

iris_pg <- tbl(con, "iris")

set_vir <- bool_filter_db(iris_pg, 'Species', 'setosa OR virginica')

# inspect generated SQL
show_query(set_vir)

# action query and retrieve data
set_vir %>% collect()

```

### Dependencies

- stringr
- stringi
- dplyr
