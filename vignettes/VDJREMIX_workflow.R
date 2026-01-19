## ----setup, message=FALSE, warning=FALSE--------------------------------------
# Load the library and set a seed for reproducibility
library(vdjremix)
set.seed(1)

## -----------------------------------------------------------------------------
data(example_airr_features)
dim(example_airr_features)
example_airr_features <- preprocess_features(
  example_airr_features,
  missingness_threshold = 0.4,
  min_unique_values = 8
)


## -----------------------------------------------------------------------------
dim(example_airr_features)

## -----------------------------------------------------------------------------
miss_plots <- plot_missingness(example_airr_features)
miss_plots$feature_missingness
miss_plots$missingness_heatmap

## -----------------------------------------------------------------------------
bench <- benchmark_imputation(
  feature_matrix = example_airr_features,
  na_frequencies = c(0.1, 0.2, 0.3),
  seed_strategy = "feature"
)

## -----------------------------------------------------------------------------
# Visualize benchmark results
imp_plots <- plot_imputation_benchmark(bench)
imp_plots$rmse_by_method

## -----------------------------------------------------------------------------
# Select the best method automatically
selection <- select_imputation_method(bench)
selection$best_method

## -----------------------------------------------------------------------------
filtered <- filter_features_by_imputation_error(
  feature_matrix = example_airr_features,
  benchmark_results = bench,
  method = selection$best_method,
  rmse_threshold = 0.5
)

mat_imputed <- impute_features(
  feature_matrix = filtered$filtered_matrix,
  method = selection$best_method
)

## -----------------------------------------------------------------------------
depth <- get_subsample_depth(mat_imputed)

cor_mat <- robust_correlation(
  feature_matrix = mat_imputed,
  subsample_depth = depth,
  n_repeats = 500,
  n_cores = 1
)

## -----------------------------------------------------------------------------
# Plot the correlation matrix
cor_plots <- plot_correlation_matrix(cor_mat)
cor_plots$correlation_heatmap

