library(readxl)
library(glmnet)

base <- read_xlsx('dataset1.xlsx')


n_col <- ncol(base)
flora_base <- base[,45:n_col]

# 初筛lasso函数
lasso_flora_fun_s1 <- function(dataset, col_1, col_2) {

  require(ggplot2)
  require(caret)
  require(glmnet)
  require(corrplot)
  require(Metrics)
  require(cowplot)
  
  # 数据标准化
  normal_dataset <- scale(dataset)
  normal_flora_dataset <- normal_dataset[,44:ncol(dataset)]
  
  # lasso回归
  xlasso <- normal_flora_dataset[, col_1:col_2]
  ylasso <- normal_dataset[, 2]  # 此处y为总胆固醇
  
  set.seed(1245)
  lasso_testout <- cv.glmnet(xlasso, ylasso, alpha = 1, nfolds = 10)
  
  # 绘制交叉验证结果图
  if (requireNamespace("ggplot2", quietly = TRUE)) {
    plot(lasso_testout)
    plot(lasso_testout$glmnet.fit, "lambda", label = TRUE)
    # 合并出图功能待开发
    #plot_grid(
      #p1, p2, labels = "auto",
      #align = "v")
  }
  
  
  lasso_testout_min <- lasso_testout$lambda.min
  lasso_testout_best <- glmnet(xlasso, ylasso, alpha = 1, lambda = lasso_testout_min)
  
  # 返回系数的名称和值
  #coef_names <- rownames(coef(lasso_testout_best))
  coef_values <- coef(lasso_testout_best)
  return(coef_values)
}


flag_lasso <- 1
repeat {
  print(lasso_flora_fun_s1(base,flag_lasso,flag_lasso+10))
  flag_lasso <- flag_lasso+11
  if (flag_lasso >= ncol(flora_base))  {
    flag_lasso <- 1
    break
  }
}

# 菌群初筛结果
floradata <- read_excel('floradata.xlsx')  # id行已删除


# 协变量+单个菌群，复筛菌群
lm_ea_flora_fun_s2 <- function (dataset, target, mode)  {
  require(ggplot2)
  require(caret)
  require(glmnet)
  require(corrplot)
  require(Metrics)
  require(cowplot)
  n_col <- ncol(dataset)
  normal_dataset <- scale(dataset)
  normal_flora <- normal_dataset[,44:n_col]

  xlm_others <- as.matrix(normal_dataset[,5:9])
  xlm_flora <- as.matrix(normal_flora[,target])
  ylm <- as.matrix(normal_dataset[,1])
  lm_testout <- lm(ylm ~ xlm_others + xlm_flora, data = dataset)

  if(target + 42 >= n_col)   {
    return ('out of range')
  }
  if(mode != 'p_val') {
    return (summary(lm_testout))

  }
  else  {
    return (lm_testout)
  }
}


# 获得p值
lmp <- function (modelobject) {
    if (class(modelobject) != "lm") stop("Not an object of class 'lm' ")
    f <- summary(modelobject)$fstatistic
    p <- pf(f[1],f[2],f[3],lower.tail=F)
    attributes(p) <- NULL
    return(p)
}


# 复筛菌群，规定 p-value <= 0.1
flora_target <- vector()
flag_lm <- 1
repeat {
  p_val <- lmp(lm_ea_flora_fun_s2(floradata,flag_lm,'p_val'))
  if (p_val <= 0.1) {
    cat(flag_lm,'\n',p_val,'\n')
    flora_target <- append(flora_target,flag_lm)
  }
  flag_lm <- flag_lm + 1
  if (flag_lm > 28) {
    flag_lm <- 1
    print(flora_target)
    break
  }
}


rawdata <- read_excel('rawdata.xlsx')


lasso_fun_final <- function(dataset, target) {

  require(ggplot2)
  require(caret)
  require(glmnet)
  require(corrplot)
  require(Metrics)
  require(cowplot)

  # 数据标准化
  normal_dataset <- scale(dataset)
  normal_flora_dataset <- normal_dataset[,44:ncol(dataset)]

  # lasso回归
  xlasso <- normal_dataset[, 5: 43]
  ylasso <- normal_flora_dataset[, target]  # 此处y为总胆固醇

  set.seed(1245)
  lasso_testout <- cv.glmnet(xlasso, ylasso, alpha = 1, nfolds = 10)

  # 绘制交叉验证结果图
  if (requireNamespace("ggplot2", quietly = TRUE)) {
    plot(lasso_testout)
    plot(lasso_testout$glmnet.fit, "lambda", label = TRUE)
    # 合并出图功能待开发
    #plot_grid(
      #p1, p2, labels = "auto",
      #align = "v")
  }


  lasso_testout_min <- lasso_testout$lambda.min
  lasso_testout_best <- glmnet(xlasso, ylasso, alpha = 1, lambda = lasso_testout_min)

  # 返回系数的名称和值
  #coef_names <- rownames(coef(lasso_testout_best))
  coef_values <- coef(lasso_testout_best)
  return(coef_values)
}



