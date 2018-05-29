# bestNormalize 1.1.0

- Clarified that the transformations are standardized by default, and providing option to not standardize in transformations
- Updated tests to run a bit faster and to use proper S3 classes

# bestNormalize 1.0.1

- Added references for original papers (Van der Waerden, Bartlett) 
  that cite the basis for the orderNorm transformation, as well as discussion 
  in Beasley (2009)
- Edited description to clarify that this procedure is a new adaptation of an 
  older technique rather than a new technique in itself

# bestNormalize 1.0.0

- Added feature to estimate out-of-sample normality statistics in bestNormalize 
  instead of in-sample ones via repeated cross-validation
    - Note: set `out_of_sample = FALSE` to maintain backward-compatibility with prior versions
      and set `allow_orderNorm = FALSE` as well so that it isn't automatically selected

- Improved extrapolation of the ORQ (orderNorm) method
    - Instead of linear extrapolation, it uses binomial (logit-link) model on ranks
    - No more issues with Cauchy transformation
    
- Added plotting feature for transformation objects

- Cleared up some documentation

# bestNormalize 0.2.2

- Changed the name of the orderNorm technique to "Ordered Quantile normalization".

# bestNormalize 0.2.1

- Made description more clear in response to comments from CRAN 

# bestNormalize 0.2.0

## First submission to CRAN
