regress_out_covs <- function(x, covs) {
  # 添加截距项到covs
  covs <- cbind(1, covs)

  # 计算beta
  beta <- solve(t(covs) %*% covs) %*% t(covs) %*% x

  # 计算残差
  residuals <- x - covs %*% beta

  # 取beta的截距并加到残差上
  intercept <- matrix(beta[1, ], nrow(x), ncol(x), byrow = TRUE)
  # result <- intercept + residuals
  result <- list(intercept = intercept,
                 residuals = residuals)

  return(result)
}
