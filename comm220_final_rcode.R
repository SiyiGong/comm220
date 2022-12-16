library(ggplot2)
data = read.csv("/Users/xingyun/Desktop/Fall/Comm220/final/Comm200_Rating.csv")
data = data.frame(data)

#data cleaning
data = data[,-1] #remove the timestamp column
colnames(data) <- c("t1_l1","t1_l2", "t1_l3","t1_l4","t2_l1","t2_l2", "t2_l3","t2_l4","t3_l1","t3_l2", "t3_l3","t3_l4") #rename columns
t1 <- c(mean(data$t1_l1), mean(data$t1_l2), mean(data$t1_l3),  mean(data$t1_l4))
t2 <- c(mean(data$t2_l1), mean(data$t2_l2), mean(data$t2_l3),  mean(data$t2_l4))
t3 <- c(mean(data$t3_l1), mean(data$t3_l2), mean(data$t3_l3),  mean(data$t3_l4))
t_names = c("level 1","level 2", "level 3","level 4")
theme_names = c("theme1","theme2", "theme3")
t1_df = data.frame(t_names, t1)
t2_df = data.frame(t_names, t2)
t3_df = data.frame(t_names, t3)
cleaned_data <- data.frame(t1, t2, t3)
cleaned_data_theme <- t(cleaned_data)
cleaned_data_theme = cleaned_data_theme[-4,]
cleaned_data_theme = as.data.frame(cleaned_data_theme)
cleaned_data_theme$ave = rowMeans(cleaned_data_theme)
cleaned_data$ave <- rowMeans(cleaned_data)
level_df = data.frame(t_names, cleaned_data$ave)
theme_df = data.frame(theme_names, cleaned_data_theme$ave)


#plot
plot_t1 <- ggplot(data= t1_df, aes(x=t_names,y=t1)) + 
  geom_bar(stat = "identity", width=0.6) +
  #geom_text(aes(label=t1), vjust=-0.3, size = 3.5) +
  ylim(0,10) +
  theme_minimal() +
  xlab("Level of relevancy logic") +
  ylab("Average scores by human raters") +
  ggtitle ("Average score for theme 1")

plot_t2 <- ggplot(data= t2_df, aes(x=t_names,y=t2)) + 
  geom_bar(stat = "identity", width=0.6) +
  #geom_text(aes(label=t1), vjust=-0.3, size = 3.5) +
  ylim(0,10) +
  theme_minimal() +
  xlab("Level of relevancy logic") +
  ylab("Average scores by human raters") +
  ggtitle ("Average score for theme 2")

plot_t3 <- ggplot(data= t3_df, aes(x=t_names,y=t3)) + 
  geom_bar(stat = "identity", width=0.6) +
  #geom_text(aes(label=t1), vjust=-0.3, size = 3.5) +
  ylim(0,10) +
  theme_minimal() +
  xlab("Level of relevancy logic") +
  ylab("Average scores by human raters") +
  ggtitle ("Average score for theme 3")

plot_level <- ggplot(data= level_df, aes(x=t_names,y=cleaned_data.ave)) + 
  geom_bar(stat = "identity", width=0.6) +
  #geom_text(aes(label=t1), vjust=-0.3, size = 3.5) +
  ylim(0,10) +
  theme_minimal() +
  xlab("Level of relevancy logic") +
  ylab("Average scores by human raters across themes") +
  ggtitle ("Average score for levels across themes")

plot_theme <- ggplot(data=cleaned_data_theme, aes(x=theme_names,y=ave)) + 
  geom_bar(stat = "identity", width=0.6) +
  #geom_text(aes(label=t1), vjust=-0.3, size = 3.5) +
  ylim(0,10) +
  theme_minimal() +
  xlab("Theme") +
  ylab("Average scores by human raters across levels within a theme") +
  ggtitle ("Average score for themes across levels")


#statistics tests
t_data = as.data.frame(t(data)) #transpose the data
_data$ave = rowMeans(t_data)
t_data$theme = c(1,1,1,1,2,2,2,2,3,3,3,3)
t_data$level = c(1,2,3,4,1,2,3,4,1,2,3,4)
#two-way ANOVA
two.way <- aov (ave ~ theme + level, data = t_data) #Main effect
summary(two.way)
interaction <- aov (ave ~ theme * level, data = t_data) #interaction effect
summary(interaction)

