library(rdrop2)

token <- drop_auth()
saveRDS(token, "droptoken.rds")