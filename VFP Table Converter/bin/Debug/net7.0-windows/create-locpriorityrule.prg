OPEN DATABASE "C:\Thrive\sfdata\vista.dbc" EXCLUSIVE

CREATE TABLE locpriorityrule ;
	LocationPriorityRule C(255), ;
	Priority I(4), ;
	Rule C(255), ;
	RuleType I(4), ;
	RuleName C(255), ;
	RuleDescription C(255), ;
	IsEnabled L, ;
	ExactMatch L, ;
	isnew L)

INDEX ON  TAG 

CLOSE DATABASE
