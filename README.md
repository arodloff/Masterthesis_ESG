# Masterthesis: Creating ESG Scores Using NLP

## Recommended Running Order

1. `01_CleanData.R` in `CodeR/`
2. `02_CopySeedwords.R` in `CodeR/`. Copy output from console into `CreateDict_SplitSentence/global_options.py`
3. `parse_parallel.py`,`parse_parallel_split.py`, `clean_and_train.py`, and `create_dict.py` in `CreateDict_SplitSentence/`
4. `03_FurtherProcessing.R`, `04_IdentifyAndPreprocess_ESGSentences.R` and `05_PreprocessFinBERT.R`
5. `predict_sentiment.py` in `FinBERT/`
6. `06_..` until `10_..` in `Code_R/`

## File and Folder Explanatation
- All relevent R skripts are in `Code_R/`
- The (modified) code and files by Li et al. (2020) is in `CreateDict_SplitSentence/`
  - The original repository can be found [here](https://github.com/MS20190155/Measuring-Corporate-Culture-Using-Machine-Learning)
- Everything related to FinBERT is in `FinBERT`
- Notice that the folders `Data/`, `CreateDict_SplitSentence/data/`, and `FinBERt/output/` are not in this repository as the files in them are too large and the redistribution rights are unknown 

### R Files
- `01_CleanData.R` extracts and cleans the earningscalls transcripts and the ESG scores by Refinitiv. The Transcripts and IDs are saved to be processed by the (modified) code by Li et al (2020).
- `02_CopySeedwords.R` transforms the seed words file `berg_draft.xlsx` in to a format which can be copied into `global_options.py`.
- `03_FurtherProcessing.R` uses the output of `parse_parallel.py` and `clean_and_train.py` and does some intensive preprocessing such as generating a list of all tokens and their document and sentence id, which can be used later (takes a lot of time and resources)
- `04_IdentifyAndPreprocess_ESGSentences` identifies ESG sentences, and calculates the tf, idf, tf-idf, and the sentiment scores by the LM2011 dictionary for the ESG words in those sentences.
- `05_PreprocessFinBERT.R` does some preprocessing of the identified ESG sentences so that they can be fed into FinBERT.
- `06_PostprocessFinBERT.R` basically merges the FinBERT predicitons with the table created in `04_IdentifyAndPreprocess_ESGSentences.R`
- `07_ScoreSentences.R` creates all six scores on the sentence level
- `08_ScoreCompanies.R` aggregates the scores on the company level and matches the text-based scores with the Asset4 scores and the FF12-industry classifiers
- `09_TablesAndFigures.R` creates all tables and figures of the thesis, although most tables were modified in LaTeX afterward
- `10_PortfolioSorts.R` simulates the trading strategy and tests it

### Python Files

#### By Li et al (2020):
-  More info is in the `README.md` provided by the authors (and modified by myself) in `CreateDict_SplitSentence/`
- `parse_parallel.py` does all the preprocessing by the CoreNLP package (takes a lot of time and resources)
- `clean_and_train.py` removes stop words and named entities, learns phrases, and trains word2vec algorithm
- `create_dict.py` creates extended dictionary

#### Other

- `parse_parallel_split.py` splits earnings call transcripts into lines. Needed to match FinBERT output to rest
- `predict_sentiment.py` FinBERT predicts sentiment of identified ESG sentences

## Other Important files
- the extended dictionary (with removed words) can be found in `CreateDict_SplitSentence/outputs/dict/expanded_dict.csv`
