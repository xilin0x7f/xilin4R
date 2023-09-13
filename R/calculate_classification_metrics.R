library(ROCR)

clf.metrics <- function(y_true, y_pred) {
  # Calculate metrics
  y_true <- factor(y_true, levels = c(0, 1))
  y_pred <- factor(y_pred, levels = c(0, 1))
  accuracy <- sum(y_pred == y_true) / length(y_true)
  cm <- table(y_true, y_pred)
  recall <- cm[2, 2] / (cm[2, 1] + cm[2, 2])
  precision <- cm[2, 2] / (cm[1, 2] + cm[2, 2])
  sensitivity <- recall
  specificity <- cm[1, 1] / (cm[1, 1] + cm[1, 2])
  f1 <- 2 * precision * recall / (precision + recall)

  # Return metrics
  metrics <- list(
    Accuracy = accuracy,
    `Confusion Matrix` = cm,
    Recall = recall,
    Precision = precision,
    Sensitivity = sensitivity,
    Specificity = specificity,
    `F1-Score` = f1
  )

  return(metrics)
}

clf.metrics.thresholds <- function(y_true, y_proba) {
  # Create prediction object
  pred <- prediction(y_proba, y_true)

  # Calculate ROC curve
  perf <- performance(pred, "tpr", "fpr")

  # Calculate recall and precision
  recall <- performance(pred, "sens")@y.values[[1]]
  precision <- performance(pred, "prec")@y.values[[1]]

  # Calculate AUC
  auc <- performance(pred, "auc")@y.values[[1]]

  # Get FPR, TPR, thresholds
  fpr <- unlist(perf@x.values[[1]])
  tpr <- unlist(perf@y.values[[1]])
  thresholds <- perf@alpha.values[[1]]

  # Return metrics as a list
  metrics <- list(FPR = fpr,
                  TPR = tpr,
                  Thresholds = thresholds,
                  Recall = recall,
                  Precision = precision,
                  AUC = auc)

  return(metrics)
}
