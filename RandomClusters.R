# Random clusters as a baseline: Assigns every cell to a random cluster.

run_random <- function(sce, n_cluster, seed, time, clustering_info) {
  start_time <- Sys.time()
  set.seed(seed)
  
  cell_ids <- colnames(sce)                   # Names of all of the cells.
  n_cells  <- length(cell_ids)                # Number of cells.
  k        <- max(1L, as.integer(n_cluster))  # Number of clusters.
  
  # Random cluster labels (the metrics module needs both a Louvain and a Leiden column).
  louvain_clustering <- sample(seq_len(k), n_cells, replace = TRUE)
  leiden_clustering  <- sample(seq_len(k), n_cells, replace = TRUE)
  
  # Dummy PCA (metrics module requires the file).
  pca <- matrix(rnorm(n_cells * 2), ncol = 2, dimnames = list(cell_ids, c("PC1", "PC2")))
  
  # Information.
  time$louvain <- Sys.time() - start_time
  time$leiden <- time$louvain
  clustering_info$seed <- seed
  
  # Return results.
  return(list(
    pca             = pca,
    hvgs            = character(0),
    cell_ids        = cell_ids,
    time            = time,
    clustering_info = clustering_info,
    leiden          = leiden_clustering,
    louvain         = louvain_clustering
  ))
}