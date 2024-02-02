
get_nhanes_data <- function(
    code = c("DEMO", "WHQ", "PAXRAW"), 
    wave = c("C", "D")
    ) {
  
  # Match args
  code <- match.arg(code)
  wave <- match.arg(wave)
  
  # Define URL path
  if (code == "DEMO" && wave == "C") {url_path <- "2003-2004/DEMO_C"}
  if (code == "DEMO" && wave == "D") {url_path <- "2005-2006/DEMO_D"}
  if (code == "WHQ" && wave == "C") {url_path <- "2003-2004/WHQ_C"}
  if (code == "WHQ" && wave == "D") {url_path <- "2005-2006/WHQ_D"}
  if (code == "PAXRAW" && wave == "C") {url_path <- "2003-2004//PAXRAW_C"}
  if (code == "PAXRAW" && wave == "D") {url_path <- "2005-2006//PAXRAW_D"}
  
  # Download DEMO/WHQ data if required
  if (code == "DEMO" || code == "WHQ") {
    data <-
      haven::read_xpt(
        paste0("https://wwwn.cdc.gov/Nchs/Nhanes/", url_path, ".xpt"),
        col_select = NULL,
        skip = 0,
        n_max = Inf,
        .name_repair = "unique"
      )
  }
  
  # Download PAXRAW data if required
  if (code == "PAXRAW") {
    options(timeout = max(2000, getOption("timeout")))
    if (wave == "C") {letter <- "c"} else {letter <- "d"}
    temp <- tempfile()
    download.file(paste0("https://wwwn.cdc.gov/Nchs/Nhanes/", url_path, ".zip"), temp)
    data <-
      haven::read_xpt(
        unz(temp, paste0("paxraw_", letter, ".xpt")),
        col_select = NULL,
        skip = 0,
        n_max = Inf,
        .name_repair = "unique"
      )
    unlink(temp)
  }
  
  # Save data
  path <- file.path("data", paste0(code, "_", wave, ".rds"))
  saveRDS(data, path)
  
  # Return path
  return(path)
  
}
