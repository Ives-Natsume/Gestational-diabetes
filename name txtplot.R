floradata_name <- read_xlsx('floradata_name.xlsx')
xlm_txt <- as.matrix(floradata_name[,44:ncol(floradata_name)])
ylm_txt <- unlist(floradata_name[,1])
lm_out_txt <- lm(ylm_txt ~ xlm_txt, data = floradata_name)
floradata_txt <- floradata_name

# Data Prep

lm_out_txt_sort <- as.data.frame(summary(lm_out_txt)$coefficients)
name_flora <- rownames(lm_out_txt_sort)
#floradata_name$mpg_z <- round((floradata_name$mpg - mean(floradata_name$mpg))/sd(floradata_name$mpg), 2)  # compute normalized mpg
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