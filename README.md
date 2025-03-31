# Simulating Mortgage Default Risk

This project walks through a full end-to-end simulation of mortgage default modeling using real-world mortgage rates and synthetically generated borrower data. The goal was to evaluate how variables like credit score, loan amount, and mortgage rate influence default risk — and to compare model performance with and without class weighting.

I pulled Freddie Mac 30-year fixed mortgage rates via API and used them as the foundation for simulating 1,000 loan records. Each loan was assigned a credit score, loan amount, and a binary default outcome generated via a logistic function. I bucketed credit scores, explored loan-to-rate ratios, and summarized trends before running models.

## Modeling Approach

I trained two logistic regression models:

**Baseline Model (no class weights):**
- Predictors: mortgage rate, credit score, loan amount
- Threshold tuned to 0.46 based on accuracy
- Accuracy: 0.83  
- F1 Score: 0.74  
- Credit score was not statistically significant  
- ROC curve showed decent separation but room to improve recall

**Weighted Model:**
- Weights were inversely proportional to class frequency
- Threshold adjusted to 0.59 to optimize classification
- Accuracy: 0.825  
- F1 Score: 0.74  
- Recall improved noticeably, credit score became significant  
- ROC-AUC slightly improved

I used confusion matrices, classification metrics, and ROC curve overlays to compare model behavior. The second model captured more true defaults while trading off a small drop in precision, making it a better fit in contexts where false negatives are more costly than false positives.

## Takeaway

This was an exercise in building more than just a predictive model — it was about controlling for imbalance, tuning thresholds thoughtfully, and interrogating model behavior with domain logic. For teams working in risk, these kinds of tradeoffs are critical, and a basic model like this can be a good starting point for real-world experimentation.
