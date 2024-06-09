
activ_process_all_nhanes <- function(
    datasets,
    config,
    demo, 
    bmx
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
    bmx = bmx
  )

# Close clusters
mirai::stop_cluster(cl)

# Combine results
all_metrics <- results |> dplyr::bind_rows()

# Return results
return(all_metrics)

}
