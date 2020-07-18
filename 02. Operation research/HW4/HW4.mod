reset;

set comp;
set date;

param acceptable_risk default 0.001;
param matrix {date, comp};
param return_rate {j in comp} := (sum{i in date} matrix[i, j])/card(date);
param cov {i in date, j in comp} := matrix[i, j] - return_rate[j];

var allocation{comp} >= 0;

data HW4.dat;

maximize expected_return: sum{j in comp} return_rate[j] * allocation[j];

subject to variance: sum{i in date} (sum{j in comp} cov[i, j] *allocation[j])^2 / card{date} <= acceptable_risk;
subject to budget_con: sum{j in comp} allocation[j] = 1;

solve;

display _varname, _var;