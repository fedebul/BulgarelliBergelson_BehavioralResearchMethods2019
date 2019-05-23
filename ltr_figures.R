source("ltr_datacleaning.R")
library(scales)
library(ggpubr)


#comprehensive matrix plot
comp.matrix.plot <- ggplot(data = human_as_gold, mapping = aes(x = Human, y=lena_tags)) + 
  geom_tile(aes(fill= rescale(prop_cat)), colour = "white") +
  geom_text(aes(label = paste(round(prop_cat*100,2),"%"), vjust = -1)) + 
  geom_text(aes(label = paste("n =",n), vjust = 2)) + 
  scale_fill_gradient(low = "white", high = "purple", name = "Proportion") +
  ylab("LENA")

#adult child plot
adult.child.plot <- ggplot(data = adult.child.graph, mapping = aes(x = Human, y=lena_tags)) + 
  geom_tile(aes(fill=rescale(prop_cat))) +
  geom_text(aes(label = paste(round(prop_cat*100,2),"%"), vjust = -1)) +
  geom_text(aes(label = paste("n =", n), vjust = 2)) + 
  scale_fill_gradient(low = "white", high = "purple", name = "Proportion")+ 
  theme(axis.title.x=element_blank(),
        axis.title.y=element_blank())
  
#male female plot
male.female.plot <- ggplot(data = male.female.graph, mapping = aes(x = Human, y=lena_tags)) + 
  geom_tile(aes(fill=rescale(prop_cat))) +
  geom_text(aes(label = paste(round(prop_cat*100,2),"%"), vjust = -1)) + 
  geom_text(aes(label = paste("n =",n), vjust = 2)) + 
  scale_fill_gradient(low = "white", high = "purple", name = "Proportion") + 
  theme(axis.title.x=element_blank(),
        axis.title.y=element_blank())

#target other plot
target.other.plot <- ggplot(data = target.other.graph, mapping = aes(x = Human, y=lena_tags)) + 
  geom_tile(aes(fill=rescale(prop_cat))) +
  geom_text(aes(label = paste(round(prop_cat*100,2),"%"), vjust = -1)) + 
  geom_text(aes(label = paste("n =",n), vjust = 2)) + 
  scale_fill_gradient(low = "white", high = "purple", name = "Proportion") + 
  theme(axis.title.x=element_blank(),
        axis.title.y=element_blank())

#electronic overlap plot
electronic.overlap.plot <-ggplot(data = electronic.overlap.graph, mapping = aes(x = Human, y=lena_tags)) + 
  geom_tile(aes(fill=rescale(prop_cat))) +
  geom_text(aes(label = paste(round(prop_cat*100,2),"%"), vjust = -1)) + 
  geom_text(aes(label = paste("n =",n), vjust = 2)) + 
  scale_fill_gradient(low = "white", high = "purple", name = "Proportion") + 
  theme(axis.title.x=element_blank(),
        axis.title.y=element_blank())

all.comparison.plot <- ggarrange(adult.child.plot, male.female.plot, target.other.plot, electronic.overlap.plot, ncol=2, nrow=2, common.legend = TRUE, legend="right")

#utterance type plot
utterance.type.plot <- ggplot(data = utterance.type.subj, aes(x = utterance_type, y = mean)) + 
  geom_boxplot(
  outlier.shape = 5,
  outlier.size = 3,
  outlier.fill = "black"
) + geom_jitter(
  shape = 1,
  width = 0.2,
  height = 0,
  size = 1,
  colour = "black"
) + ylab("Classification accuracy")

utterance.type.ac.plot <- ggplot(data =utterance.type.ac.subj, aes(x = utterance_type, y=mean)) +
  geom_boxplot(outlier.shape = 5, outlier.size = 2, outlier.fill = "black") + 
  geom_jitter(shape = 1, width = 0.2, height=0, size = 1, colour = "black") + 
  ggtitle("Adult vs. Child") + 
  theme(axis.title.x=element_blank(),
        axis.title.y=element_blank()) + 
  theme(axis.text.x = element_blank()) + 
  theme(text = element_text(size=12))
  

utterance.type.mf.plot <-ggplot(data =utterance.type.mf.subj, aes(x = utterance_type, y=mean)) + 
  geom_boxplot(outlier.shape = 5, outlier.size = 2, outlier.fill = "black") + 
  geom_jitter(shape = 1, width = 0.2, height=0, size = 1, colour = "black") + 
  ggtitle("Male vs. Female")+ 
  theme(axis.title.x=element_blank(),
        axis.title.y=element_blank()) + 
  theme(axis.text.x = element_blank()) + 
  theme(text = element_text(size=12))

utterance.type.tn.plot <-ggplot(data =utterance.type.tn.subj, aes(x = utterance_type, y=mean)) + 
  geom_boxplot(outlier.shape = 5, outlier.size = 2, outlier.fill = "black") + 
  geom_jitter(shape = 1, width = 0.2, height=0, size = 1, colour = "black") + 
  ggtitle("Target vs. Other Child")+ 
  theme(axis.title.x=element_blank(),
        axis.title.y=element_blank()) + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
  theme(text = element_text(size=12))

utterance.type.on.plot <-ggplot(data =utterance.type.on.subj, aes(x = utterance_type, y=mean)) + 
  geom_boxplot(outlier.shape = 5, outlier.size = 2, outlier.fill = "black") + 
  geom_jitter(shape = 1, width = 0.2, height=0, size = 1, colour = "black") + 
  ggtitle("Electronic vs. Overlap")+ 
  theme(axis.title.x=element_blank(),
        axis.title.y=element_blank()) + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
  theme(text = element_text(size=12))

all.utterance.type.plots <- ggarrange(utterance.type.ac.plot, utterance.type.mf.plot, utterance.type.tn.plot, utterance.type.on.plot, ncol=2, nrow=2) 
