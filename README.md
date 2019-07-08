# GweetR

# Sentiment Analyis for tweets featuring top trending google searches.

https://syborg.shinyapps.io/GweetR/

GweetR is a sentiment analysis tool based on tweets of top trending Google searches. It analyses data retrieved geographically from 8 Australian cities and presents the sentiments and vocabulary used for the trend selected by the user. In a nutshell, it paints the current
'emotionscape', as expressed on Twitter, across the major cities for the most trending queries in Australia at any given moment.

The application was developed using R, RShiny & JavaScript along with a few novel libraries for accessing twitter API, lexical analysis, d3.js+R integration etc.

Screenshots :

The Landing page :

![alt text](/preview/1.png)

The Sentiments map & Stream of words :

![alt text](/preview/2.png)

The Tweets/Dataset :

![alt text](/preview/3.png)

Emotion-Word network :

![alt text](/preview/4.png)

Frequently used words(cloud) :

![alt text](/preview/12.png)

Heatmap of everyday moods :

![alt text](/preview/16.png)

## To do -- 

1. Use VADER for polarity/valence extraction
2. Train a LSTM/GRU model for sentiment inference
3. Transfer learning with pretrained models like GPT-2/BERT/XLNET for better inference/predictions 
