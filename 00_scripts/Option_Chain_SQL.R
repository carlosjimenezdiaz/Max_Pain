Option_Chain_SQL <-
function(yml_file){
  # Config File
  config <- config::get(file = yml_file)
  
  # Setting the connection with MySQL
  conn <- RMySQL::dbConnect(MySQL(), 
                            dbname   = config$database, 
                            host     = config$host, 
                            port     = config$port, 
                            user     = config$username, 
                            password = config$password)
  
  # Downloading the data from the SQL Server
  db_option_chain <- RMySQL::dbReadTable(conn, "db_option_chain")
  
  # Disconnecting from SQL Server
  RMySQL::dbDisconnect(conn)
  
  # Return
  return(db_option_chain)
}
