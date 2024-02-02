
activ_process_all_nhanes <- function(
    datasets,
    config,
    demo, 
    whq,
    content = c("option_1", "option_2", "option_3"),
    out_dir = "./data",
    out_folder = "PAXRAW_RESULTS",
    export_results = FALSE
    ) {

# Define clusters
cl <- mirai::make_cluster(parallel::detectCores())

# Process files
results <- 
  parallel::parLapply(
    cl,
    datasets$data, 
    activ_process_nhanes, 
    config = config, 
    demo = demo, 
    whq = whq
  )

# Close clusters
mirai::stop_cluster(cl)

# Combine results
all_metrics <- results |> dplyr::bind_rows()

# Return results
return(all_metrics)

}
