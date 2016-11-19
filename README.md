# Project: Words 4 Music

### [Project Description](doc/Project4_desc.md)

Term: Fall 2016

+ Contributor's name: Kezhi Cai
+ Projec title: Topic Modelling from Music to Word
+ Project summary: kNN was first used as in the presentation PPT. Which performs poor. Then I tried Latent Dirichlet allocation with new features. Random forest was applied to train the model.

1> LDA

The performance of LDA shows below, I chose 20 topics:

![screenshot](https://github.com/TZstatsADS/Fall2016-proj4-ClayCai/blob/master/figs/LDA_20%20Topics.png)	

For example, the top lyric words for topic 20 are:
![screenshot](https://github.com/TZstatsADS/Fall2016-proj4-ClayCai/blob/master/figs/Screen%20Shot%202016-11-19%20at%201.55.53%20PM.png)

LDAvis package was applied for visulization.

2> Features

I chose 191 features from "analysis" data frame, including statistics such as means, length, variances, and quantiles.

3> Randomforest
 
Random forest was used for classification. The responce of the model is topic number 1~20. Then lyric word ranks are predicted using the word ranks of each topic.
