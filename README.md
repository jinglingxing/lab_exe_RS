# lab_exe_RS
The recommendation system lab exercises, including three TPs. 
The TPs questions are below. I revised our TPs with the solution that the Professor Michel gave us. Finally, the Recommendation System course includes one practical exam, I did it and also uploaded the sample exam code.

Team members for TP1-3:<br>
Safa GHRIBI<br>
Jinling XING<br>

Author for all exams: Jinling XING

### TP1, user-user collaborative filter recommendations and item-item
TP1 deals with film recommendations with data from GroupLens (1).
You must apply the algorithms seen in the courses to answer the following points:

1. What is the average of the votes by profession ("job") and by age? 
2. What are the 10 most similar films to "Star Trek V: Final Frontier (1989)" according to the measure of cosine and the correlation with the voting matrix, respectively. 
3. Use an item-item approach to calculate the vote for the movie "Star Trek V: The Final Frontier (1989)" for users who do not have a vote for it. Take the 20 nearest neighbors according to the Euclidean distance and use the cosine as weight. If no common vote exists, then the predicted value is set to NA. 
4. Calculate the mean square error of the prediction of the item-item approach to the previous question by comparing it to the observed values. 
5. One user rated the lowest rating (1) for all Star Wars movies and the highest rating (5) for all Star Trek movies. What 10 movies do you recommend? Use a user-user approach for the answer and 20 close neighbors. 
6. I am a new user. You know my profession, my sex and my age. Develop a Bayesian algorithm to recommend 10 movies based on these three categories. 


### TP2. Graphing techniques 
The first question is about recommendations for data represented in the form of a directed graph, for example recommending articles to read using the reference graph. The recommendations will be based on a matrix that indicates which article refers to which other.

A list of 1090 references was extracted from the Citeseer database for the exercise of the TP. A referencing matrix indicating who refers to who was created to facilitate the analysis. For the purpose of this exercise, references outside the 1090 items are not indicated (an article usually refers to other articles outside this set as well). The instructions for downloading the data are provided below.

Work to do :

1. Suppose I read document number '422908' in my matrix. Apply the Page rank algorithm to determine the other recommended readings. In addition to simply recommending references to '422908', apply at least one variation of this basic approach, such as the one in the class that extends the subset S (references) to S '(reference references). Explain the steps you took.
2. Compare the results obtained with an approach based on the similarity of articles in a vector space, similar to the similarity calculation of the item-item approach. The measure of similarity and how to use it to estimate the relevance of similar items is left to your discretion.
3. Use cross-validation to evaluate the performance of the item-item approach. 


### Work to do for TP3 
Use the votes from the u.dataTP 1 matrix to replicate the experiment described in section 3.1.1 of Sarwar et al. 2000 ( local version ).

1. Determine a point of comparison for the forecast of votes (a minimum performance)
2. Apply decomposition SVD (taking care to standardize beforehand)
3. Estimate votes based on SVD with 10 dimensions.
4. Calculate the mean absolute error and the mean squared error.
5. Determine the optimal number of dimensions (without applying cross validation). A graph should indicate performance by number of dimensions (similar to the Sarwar et al. Report)
6. Repeat the forecast based on the optimal number of dimensions, but this time using cross-validation.
7. Compare the performance of this approach with that of a collaborative approach of your choice (with squared error and absolute error averages). Use cross validation.
