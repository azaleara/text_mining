library(rtweet)


api_key <- "XXXXXX"
api_secret_key <- "XXXXX"
access_token <- "XXXXXX" #esta hay que crearla cada vez
access_token_secret <- "XXXXX" #esta hay que crearla cada vez
# en app nantli te vas a "Key and Tokens" y despues a "Authentication Tokens" luego "generate" 

token <- create_token(
  app = "XXXX",
  consumer_key = api_key,
  consumer_secret = api_secret_key,
  access_token = access_token,
  access_secret = access_token_secret
)

