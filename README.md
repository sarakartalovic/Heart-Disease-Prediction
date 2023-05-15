# Heart Disease Prediction

In this project the Heart Disease Data Set is explored in the problem of heart disease classification.
The first approach is takinginto account all four classes and making a prediction based on the combined
decision between single binary classifiers for each class. The second approach is combining all disease states in a single class and differentiating between healthy
and disease states.

Datasets consist of the data from 4 hospitals and can be found in the folder data. 
There are 898 entries in total and number of subjects for each hospital are the following: 

* Cleveland - 281,
* Hungarian - 294,
* Switzerland - 123, 
* Long Beach VA - 200.

Feature descriptions can befound in the project report along with its descriptions. The report contains the description of the procedures as well as the figures and R code.

## Results 
* Different features are used by the model for different classes of heart diseases
* The data allows for better classification models between healthy and disease state samples
* Models such as LDA, QDA and KNN can reach 50% accuracy when differentiating between all disease classes, but up to 80% for the binary classification
* Discriminant analysis models classify healthy samples with the highestaccuracy
* According to best subset selection, forward and backward selection, best 5-variable model contains ’sex’, ’cp’, ’thlach’, ’exang’, and ’oldpeak’ features
* Less complex models tend to have higher accuracy

