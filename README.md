# Data.Analysis_Assignment
A small case Study

Includes complete Analysis on the case study.

1. Complete Analysis has been performed using R studio.

2. The file "Data.Analysis_assignment.Rmd" includes the complete codebook of the analysis.

3. Report has been generated as a Word and HTML document.

4. Recommended report document- Docx file and HTML file, the file "Data.Analysis_assignment.html" can be viewed locally using any web browser or can be remotely viewed at the GitHub
	repository.

## A brief intro to the data set

The data pertains to a Direct Marketing campaign of a European Banking institution. 
The marketing campaigns were conducted thru phone calls. 
Often, more than one contact to the same client was required, in order to access if the product subscribed (bank term deposit) would be ('yes') or ('no') 
 
## Variables:
   
   1 - age (numeric)
   2 - job : type of job (categorical: "admin.","blue-collar","entrepreneur","housemaid","management","retired","self-employed","services","student","technician","unemployed","unknown")
   3 - marital : marital status (categorical: "divorced","married","single","unknown"; note: "divorced" means divorced or widowed)
   4 - education (categorical: "basic.4y","basic.6y","basic.9y","high.school","illiterate","professional.course","university.degree","unknown")
   5 - default: has credit in default? (categorical: "no","yes","unknown")
   6 - housing: has housing loan? (categorical: "no","yes","unknown")
   7 - loan: has personal loan? (categorical: "no","yes","unknown")
   
 ##related with the last contact of the current campaign:
 
   8 - contact: contact communication type (categorical: "cellular","telephone") 
   9 - month: last contact month of year (categorical: "jan", "feb", "mar", ..., "nov", "dec")
  10 - day_of_week: last contact day of the week (categorical: "mon","tue","wed","thu","fri")
  11 - duration: last contact duration, in seconds (numeric). Yet, the duration is not known before a call is performed. Also, after the end of the call y is obviously known. 

 ##other attributes:
 
  12 - campaign: number of contacts performed during this campaign and for this client (numeric, includes last contact)
  13 - pdays: number of days that passed by after the client was last contacted from a previous campaign (numeric; 999 means client was not previously contacted)
  14 - previous: number of contacts performed before this campaign and for this client (numeric)
  15 - poutcome: outcome of the previous marketing campaign (categorical: "failure","nonexistent","success")

 ##social and economic context attributes
 
  16 - emp.var.rate: employment variation rate - quarterly indicator (numeric)
  17 - cons.price.idx: consumer price index - monthly indicator (numeric)     
  18 - cons.conf.idx: consumer confidence index - monthly indicator (numeric)     
  19 - euribor3m: euribor 3 month rate - daily indicator (numeric)
  20 - nr.employed: number of employees - quarterly indicator (numeric)

 # Response 
 
  21 - y - has the client subscribed a term deposit? (binary: "yes","no")
