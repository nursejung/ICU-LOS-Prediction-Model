import joblib
import pandas as pd
import sys

def get_prob(path_model, path_X):
   model = joblib.load(path_model) 
   input_data = pd.read_csv(path_X)
   input_data = input_data.loc[:,'Age':]
   features = model.feature_names_in_

   input_data.columns = features
   cols = list(input_data.columns)

   # print(features)
   # print(cols)

   predicted_prob = model.predict_proba(input_data)
   predict = model.predict(input_data)
   # print(predict) 

   list_prob = []

   for case in predicted_prob:
       i = int(case[1]*100)
       list_prob.append(i)

   csv_string = "Risk_score\n"

   for prob in list_prob:
      csv_string = csv_string + f"{prob}\n"

   print(csv_string)

   return 

# python get_prob.py ./trained/RF_matrix_trained_random_12hr_60hr.joblib ./file/temp.csv
if __name__ == "__main__":
   get_prob(sys.argv[1], sys.argv[2])
