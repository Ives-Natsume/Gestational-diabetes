floradata_sub <- subset(floradata, floradata[,59] > 0 | floradata[,61] > 0)
# lasso回归
xlm_59 <- floradata_sub[,59]
xlm_61 <- floradata_sub[,61]
ylm <- floradata_sub[,1]  # 此处y为总胆固醇
xlm_59 <- unlist(xlm_59)
xlm_61 <- unlist(xlm_61)
ylm <- unlist(ylm)

lm_out_59 <- lm(ylm ~ xlm_59, data = floradata_sub)
lm_out_61 <- lm(ylm ~ xlm_61, data = floradata_sub)


new_xlm_59 <- seq(min(floradata_sub[,59]), max(floradata_sub[,59]), 0.01)
pred_wt_59 <- data.frame(predict(lm_out_59, newdata = data.frame(xlm_59 = new_xlm_59),
                                 interval = "confidence"))
print(head(pred_wt_59))

p_59 <- ggplot() +
  geom_point(data = floradata_sub, mapping = aes(x = xlm_59, y = ylm)) +
  theme_bw() +
  geom_line(data = pred_wt_59, mapping = aes(x = new_xlm_59, y = fit),
            color = "red", size = 1, alpha = 0.5) +
  geom_ribbon(data = pred_wt_59, mapping = aes(x = new_xlm_59,
                                               ymin = lwr, ymax = upr),
              fill = "grey", alpha = 0.5) +
  labs(x = "巨大角杆菌属", y = "总胆固醇")








new_xlm_61 <- seq(min(floradata_sub[,61]), max(floradata_sub[,61]), 0.01)
pred_wt_61 <- data.frame(predict(lm_out_61, newdata = data.frame(xlm_61 = new_xlm_61),
                                 interval = "confidence"))
print(head(pred_wt_61))

p_61 <- ggplot() +
  geom_point(data = floradata_sub, mapping = aes(x = xlm_61, y = ylm)) +
  theme_bw() +
  geom_line(data = pred_wt_61, mapping = aes(x = new_xlm_61, y = fit),
            color = "red", size = 1, alpha = 0.5) +
  geom_ribbon(data = pred_wt_61, mapping = aes(x = new_xlm_61,
                                               ymin = lwr, ymax = upr),
              fill = "grey", alpha = 0.5) +
  labs(x = "弗里辛球菌", y = "总胆固醇")



plot_grid(
  p_59,
  p_61,
  ncol = 1,
  labels = "auto")





library(ggplot2)
library(readxl)
theme_set(theme_bw())  


rawdata <- read_excel('dataset1.xlsx')
xlm_txt <- as.matrix(floradata[,44:ncol(floradata)])
ylm_txt <- unlist(floradata[,1])
lm_out_txt <- lm(ylm_txt ~ xlm_txt, data = floradata)
floradata_txt <- floradata

# Data Prep

lm_out_txt_sort <- as.data.frame(summary(lm_out_txt)$coefficients)
name_flora <- rownames(lm_out_txt_sort)
#floradata$mpg_z <- round((floradata$mpg - mean(floradata$mpg))/sd(floradata$mpg), 2)  # compute normalized mpg
lm_out_txt_sort$type <- ifelse(lm_out_txt_sort[,1] < 0, "below", "above")  # above / below avg flag
lm_out_txt_sort <- lm_out_txt_sort[order(lm_out_txt_sort$Estimate ), ]  # sort
name_flora <- factor(name_flora, levels = name_flora)  # convert to factor to retain sorted order in plot.

# Diverging Barcharts
ggplot(lm_out_txt_sort, aes(y=Estimate, x=name_flora)) + 
   labs(x = "菌群编号", y = "影响程度") +
   geom_bar(stat='identity', aes(fill=type), width=.5)  +
   scale_fill_manual(name="相关性", 
                     labels = c("正相关", "负相关"), 
                     values = c("above"="#00ba38", "below"="#f8766d")) + 
   labs(title="不同菌群对血脂影响的大致分布") + 
   theme(axis.text.x = element_text(angle = 50, vjust = 1, hjust = 1))


ggplot(lm_out_59, aes(sample = xlm_59)) +
  
  geom_qq() +
  ylim(c(-1,5)) +
  geom_qq_line()



normal_floradata <- as.data.frame(scale(floradata))

xlm_food <- as.matrix(normal_floradata[,9:42])
ylm_59 <- unlist(normal_floradata[,59])
ylm_61 <- unlist(normal_floradata[,61])
lm_out_food_59 <- lm(ylm_59 ~ xlm_food, data = normal_floradata)
lm_out_food_61 <- lm(ylm_61 ~ xlm_food, data = normal_floradata)

# Data Prep

lm_out_food_59_sort <- as.data.frame(summary(lm_out_food_59)$coefficients)
name_food <- rownames(lm_out_food_59_sort)
#floradata$mpg_z <- round((floradata$mpg - mean(floradata$mpg))/sd(floradata$mpg), 2)  # compute normalized mpg
lm_out_food_59_sort$type <- ifelse(lm_out_food_59_sort[,1] < 0, "below", "above")  # above / below avg flag
lm_out_food_59_sort <- lm_out_food_59_sort[order(lm_out_food_59_sort$Estimate ), ]  # sort
name_food <- factor(name_food, levels = name_food)  # convert to factor to retain sorted order in plot.

# Diverging Barcharts
food59 <- 
  ggplot(lm_out_food_59_sort, aes(y=Estimate, x=name_food)) + 
  labs(x = "食物种类", y = "影响程度(巨大角杆菌属)") +
  geom_bar(stat='identity', aes(fill=type), width=.5)  +
  scale_fill_manual(name="相关性", 
                    labels = c("正相关", "负相关"), 
                    values = c("above"="#00ba38", "below"="#f8766d")) + 
  theme(axis.text.x = element_text(angle = 50, vjust = 1, hjust = 1))



lm_out_food_61_sort <- as.data.frame(summary(lm_out_food_61)$coefficients)
name_food <- rownames(lm_out_food_61_sort)
#floradata$mpg_z <- round((floradata$mpg - mean(floradata$mpg))/sd(floradata$mpg), 2)  # compute normalized mpg
lm_out_food_61_sort$type <- ifelse(lm_out_food_61_sort[,1] < 0, "below", "above")  # above / below avg flag
lm_out_food_61_sort <- lm_out_food_61_sort[order(lm_out_food_61_sort$Estimate ), ]  # sort
name_food <- factor(name_food, levels = name_food)
food61 <- 
  ggplot(lm_out_food_61_sort, aes(y=Estimate, x=name_food)) + 
  labs(x = "食物种类", y = "影响程度(弗里辛球菌)") + 
  geom_bar(stat='identity', aes(fill=type), width=.5)  + 
  scale_fill_manual(name="相关性", 
                    labels = c("正相关", "负相关"), 
                    values = c("above"="#00ba38", "below"="#f8766d")) + 
  theme(axis.text.x = element_text(angle = 50, vjust = 1, hjust = 1))

plot_grid(
  food59,
  food61,
  ncol = 1,
  labels = "auto")

   