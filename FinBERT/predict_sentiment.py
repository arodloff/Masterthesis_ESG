from finbert.finbert import predict
from transformers import AutoModelForSequenceClassification
#import argparse
import os
import nltk
import pandas as pd
#nltk.download('punkt')

model_path = "models/finbert-sentiment"
output_path = "output/"
input_file =  "ESG_sentences_raw.txt"

if not os.path.exists(output_path):
    os.mkdir(output_path)

with open(input_file,'r') as f:
    text = f.read()

model = AutoModelForSequenceClassification.from_pretrained(model_path,num_labels=3,cache_dir=None)

output = "predictions.csv"
#output2 = "predictions2.csv"
predict(text,model,write_to_csv=True,path=os.path.join(output_path,output))
#pred_finbert = predict(text,model,write_to_csv=True,path=os.path.join(output_path,output))

#pred_finbert[['prediction','logit','sentiment_score']].to_csv(os.path.join(output_path, output2))