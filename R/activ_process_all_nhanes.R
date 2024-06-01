
activ_process_all_nhanes <- function(
    datasets,
    config,
    demo, 
    whq
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
