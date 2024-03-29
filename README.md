# Utilizing machine-learning techniques to optimize the delicate balance between the required number of observations for accurate predictions and the length of the time series: Predicting COVID-19 positive cases in chosen European countries

The pervasive impact of COVID-19 waves on daily life has been apparent since the beginning of 2020. Despite substantial advancements in vaccination efforts reducing the risk of a global pandemic, the emergence of highly mutated variants poses a potential threat to post-vaccination immunity. As a result, accurately predicting upcoming COVID-19 positive cases remains critical. This study employs machine-learning methodologies to forecast the number of new positive cases for a given day, utilizing data on positive, hospitalized, and vaccinated individuals, along with reproduction numbers from a specified number of preceding days.

However, the primary objective of the study is to manage the trade-off between the number of days considered in the past for predicting the next-day positives and the necessary length of the time series, representing each variable in the dataset. Typically, longer time periods, involving a higher number of historical days for prediction, are thought to enhance predictive performance. Nonetheless, this improvement comes at the cost of reducing the number of observations and, consequently, shortening the time series used for model training—an essential factor for accurate predictions.

To address this trade-off, we employ various machine-learning techniques, such as multivariate regression, the least absolute shrinkage and selection operator (LASSO), ridge regression, support vector machines, and random forests. These algorithms are applied to COVID-19 data from selected European countries. Within each algorithm, we systematically search for the optimal time period length that minimizes the root mean square error of the prediction. This process aims to determine the most precise number of previous days needed for predicting the next-day COVID-19 positive cases.

<b>Funding:</b> The study is supported by the grant no. F4/53/2022 "Prediction and visualization of causes-of-deaths seasonality and death excess rates using machine-learning approaches", which has been provided by the Internal Grant Agency of the Prague University of Economics and Business.


<p align="center">
  <img src="https://raw.githubusercontent.com/LStepanek/prediction-of-covid-19-positives-in-selected-european-countries/main/outputs/svm_Czechia_rmse_for_various_t_k_%3D_10.png">
  <figcaption>Czechia</figcaption>
</p>

<br>
<br>

<p align="center">
  <img src="https://raw.githubusercontent.com/LStepanek/prediction-of-covid-19-positives-in-selected-european-countries/main/outputs/svm_France_rmse_for_various_t_k_%3D_10.png">
  <figcaption>France</figcaption>
</p>

<br>
<br>

<p align="center">
  <img src="https://raw.githubusercontent.com/LStepanek/prediction-of-covid-19-positives-in-selected-european-countries/main/outputs/svm_Germany_rmse_for_various_t_k_%3D_10.png">
  <figcaption>Germany</figcaption>
</p>

<br>
<br>

<p align="center">
  <img src="https://raw.githubusercontent.com/LStepanek/prediction-of-covid-19-positives-in-selected-european-countries/main/outputs/svm_Italy_rmse_for_various_t_k_%3D_10.png">
  <figcaption>Italy</figcaption>
</p>

<br>
<br>

<p align="center">
  <img src="https://raw.githubusercontent.com/LStepanek/prediction-of-covid-19-positives-in-selected-european-countries/main/outputs/svm_Netherlands_rmse_for_various_t_k_%3D_10.png">
  <figcaption>Netherlands</figcaption>
</p>

<br>
<br>

<p align="center">
  <img src="https://raw.githubusercontent.com/LStepanek/prediction-of-covid-19-positives-in-selected-european-countries/main/outputs/svm_Spain_rmse_for_various_t_k_%3D_10.png">
  <figcaption>Spain</figcaption>
</p>

<br>
<br>

