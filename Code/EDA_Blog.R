# Read sql dump from mysql

library(RMySQL)
library(DBI)

con <- dbConnect(RMySQL::MySQL(), host="localhost",dbname="ccblog",user='sk',password = "test")

dbListTables(con)
dbListFields(con, "wp_users")
dbReadTable(con, "wp_users") -> blog_users
dbListFields(con, "wp_2_posts")
dbReadTable(con, "wp_2_posts") -> blog_posts
str(blog_posts)

# Error because of wrong charset/encoding?
nctr <- sapply(blog_posts[1:4,],nchar)
blog_posts[5,]

# Disconnect from the database
dbDisconnect(con)

#####################
### As starting point
#####################

# ##################
# ### mtcars example
# ##################
# library(DBI)
# # Connect to my-db as defined in ~/.my.cnf (one has to create the cnf-file manually)
# con <- dbConnect(RMySQL::MySQL(), group = "my-db")
# 
# dbListTables(con)
# dbWriteTable(con, "mtcars", mtcars)
# dbListTables(con)
# 
# dbListFields(con, "mtcars")
# dbReadTable(con, "mtcars")
# 
# # You can fetch all results:
# res <- dbSendQuery(con, "SELECT * FROM mtcars WHERE cyl = 4")
# dbFetch(res)
# dbClearResult(res)
# 
# # Or a chunk at a time
# res <- dbSendQuery(con, "SELECT * FROM mtcars WHERE cyl = 4")
# while(!dbHasCompleted(res)){
#   chunk <- dbFetch(res, n = 5)
#   print(nrow(chunk))
# }
# # Clear the result
# dbClearResult(res)
# 
# # Disconnect from the database
# dbDisconnect(con)