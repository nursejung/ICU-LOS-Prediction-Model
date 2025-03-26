import pandas as pd
import numpy as np
import sys
import joblib
import shap

def get_shapval(path_model, path_X):
    model = joblib.load(path_model)

    best = model.best_estimator_

    explainer = shap.Explainer(best)

    features = pd.read_csv(path_X)
    
    X = features.iloc[:, 2:]

    shap_values = explainer.shap_values(X)

    name_list = list(features.columns)

	# print("type: ", type(shap_values[0]))
	
    df = pd.DataFrame(shap_values[1], columns = name_list[2:])
	# print(df.shape)
    csv_string = df.to_csv(index=False)
    print(csv_string)
    return csv_string

# python get_shapval.py ./trained/RF_matrix_trained_random_32hr_24hr.joblib ./file/sample_all.csv
if __name__ == "__main__":
    result = get_shapval(sys.argv[1], sys.argv[2])
	# print(result)
    

