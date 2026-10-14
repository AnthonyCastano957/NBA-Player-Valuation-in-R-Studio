# NBA-Player-Valuation-in-R-Studio
Using salaries from Basketball-Reference and the hoopR R package, I built a non-linear GAM with smooth splines to map production to the log-salary scale using data from the 2025-2026 season.
# Objectives
Standard OLS regression models don't capture the nonlinear structural nature of salary in the NBA that occurs due to jumps from players going from rookie contracts to mid-level exceptions, role player contracts, and long-term max deals.
# Key Findings
Using weighted per-36 counting stats (Prod_Index) and Minutes as my two independent variables, the model 
explained about 39.7% of salary deviance. (R^2 = .392)
I tested DBPM, TS%, and TOV% and found no actual change in the deviance.
The model serves as a good basis for analyzing how contracts are valued in most front offices.
The model fails to capture the advanced impact players such as Rudy Gobert have that would justify his contract.
# Market Insight
While teams use analytics heavily on the court, raw box scores still dominate NBA salary compensation. 
# Final Thoughts
In reality, data analysts use much more complicated models that capture the nuance that doesn't exist within regular counting stats.
I would like to study further models that front offices use that capture advanced metrics more accurately, such as machine learning and Bayesian analysis that help build better valuation models.
# Workflow
## Importing
Imported a CSV from Basketball-Reference with salary numbers for the 2025-2026 season.
Cleaned messy data, such as salary numbers that included $ symbols, destringing, and filtered out players with no salary (Exhibit 10/two-way deals)
## Fetching Box Score data
Gathered box score data, filtered out players with fewer than 150 minutes played, created per-36 numbers, and used standard weights to create a production index. (one of the independent variables)
## Merging
Inner-joined salary data and box score data, and logged salary to deal with nonlinearity. 
# GAM model
K = 5 across two variables, and used the REML method used. 
