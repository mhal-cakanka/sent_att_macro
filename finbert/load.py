import pandas as pd
import torch
import gc
import time
from transformers import AutoTokenizer, AutoModelForSequenceClassification

# Load model
tokenizer = AutoTokenizer.from_pretrained("ProsusAI/finbert")
model = AutoModelForSequenceClassification.from_pretrained("ProsusAI/finbert")

# Load dataset
# twitter = pd.read_csv("tw_text_clean.csv", encoding='unicode_escape')
proquest = pd.read_csv("pq_text_clean.csv", encoding='unicode_escape')

# Define the function
def get_finbert(data, batch=100, textcol="text"):
    start = time.time()

    # Remove NAs from the datset
    data = data.dropna()
    # Get number of rows
    DL = len(data)
    print(f"DL: {DL}")

    # Loop over batches of rows to estimate model and get predictions of sentiment from finbert
    for i in range(0, DL, batch):
        start_batch = time.time()
        # Prepare range of rows
        to = i + batch
        if to > DL+1:
            to = DL+1
        rang = range(i, to)

        # Subset dataset with the new range
        subset = data.iloc[rang]
        # Turn columns into lists
        text_list = subset[textcol].tolist()
        id_list = subset['id'].to_numpy()
        idx_list = subset['idx'].to_numpy()
        del subset
        gc.collect()

        # Tokenize text
        inputs = tokenizer(text_list, padding=True, truncation=True, return_tensors='pt')

        # Estimate model
        with torch.no_grad():
            outputs = model(**inputs)
        del inputs
        gc.collect()

        # Extract predictions, convert the output
        predictions = torch.nn.functional.softmax(outputs.logits, dim=-1)
        del outputs
        gc.collect()

        # Prepare table with results
        positive = predictions[:, 0].tolist()
        negative = predictions[:, 1].tolist()
        neutral = predictions[:, 2].tolist()
        del predictions
        gc.collect()

        table = {'idx': idx_list,
                 'id': id_list,
                 'Text': text_list,
                 "Positive": positive,
                 "Negative": negative,
                 "Neutral": neutral}

        df = pd.DataFrame(table, columns=['idx', 'id', "Text", "Positive", "Negative", "Neutral"])
        del table
        gc.collect()

        print(rang, 'Memory:', "time:", time.time() - start_batch)
        
        # df.to_csv(f"tw_out_{i}.csv")
        df.to_csv(f"pq_out_{i}.csv")
    end = time.time()
    print(f"Taken {end - start}s.")

# Run finbert function
# get_finbert(twitter, batch=100, textcol="text")
get_finbert(proquest, batch=100, textcol="text")