long_colnames_replacements <- list(
  "Number of Policy" = "Number.of.Policy",
  "Type of Policy" = "Type.of.Policy",
  "Name of Policy" = "Name.of.Policy",
  "Year(a)" = "Year.a.",
  "Focus of Amendments to Policy" = "Focus.of.Amendments.to.Policy",
  "Legislative Basis" = "Legislative.Basis",
  "Valid from (b)" = "Valid.from.b.",
  "Valid until (c)" = "Valid.until.c.",
  "Valid from childbirth related date (d)" = "Valid.from.childbirth.related.date.d.",
  "Valid until childbirth related date (e)" = "Valid.until.childbirth.related.date.e.",
  "Recipient" = "Recipient",
  "General Functioning Structure" = "General.Functioning.Structure",
  "NON MONETARY ENTITLEMENTS: Types of entitlement (f)" = "NON.MONETARY.ENTITLEMENTS.Types.of.entitlement.f.",
  "NON MONETARY ENTITLEMENTS: Period of entitlement (g)" = "NON.MONETARY.ENTITLEMENTS.Period.of.entitlement.g.",
  "NON MONETARY ENTITLEMENTS: Length of entitlement (h)" = "NON.MONETARY.ENTITLEMENTS.Length.of.entitlement.h.",
  "NON MONETARY ENTITLEMENTS: Conditions of entitlement Employment related conditions" = "NON.MONETARY.ENTITLEMENTS.Conditions.of.entitlement.Employment.related.conditions",
  "NON MONETARY ENTITLEMENTS: Conditions of entitlement Conditions related to relationship to other family members" = "NON.MONETARY.ENTITLEMENTS.Conditions.of.entitlement.Conditions.related.to.relationship.to.other.family.members",
  "NON MONETARY ENTITLEMENTS Conditions of entitlement Other entitlement conditions" = "NON.MONETARY.ENTITLEMENTS.Conditions.of.entitlement.Other.entitlement.conditions",
  "NON MONETARY ENTITLEMENTS Other details" = "NON.MONETARY.ENTITLEMENTS.Other.details",
  "MONETARY ENTITLEMENTS Types of entitlement (f)" = "MONETARY.ENTITLEMENTS.Types.of.entitlement.f.",
  "MONETARY ENTITLEMENTS Period of entitlement" = "MONETARY.ENTITLEMENTS.Period.of.entitlement",
  "MONETARY ENTITLEMENTS Length of entitlement" = "MONETARY.ENTITLEMENTS.Length.of.entitlement",
  "MONETARY ENTITLEMENTS Rate of entitlement" = "MONETARY.ENTITLEMENTS.Rate.of.entitlement",
  "MONETARY ENTITLEMENTS Other details" = "MONETARY.ENTITLEMENTS.Other.details",
  "MONETARY ENTITLEMENTS Conditions of entitlement Age related conditions" = "MONETARY.ENTITLEMENTS.Conditions.of.entitlement.Age.related.conditions",
  "MONETARY ENTITLEMENTS Conditions of entitlement Employment related conditions" = "MONETARY.ENTITLEMENTS.Conditions.of.entitlement.Employment.related.conditions",
  "MONETARY ENTITLEMENTS Conditions of entitlement Earnings related conditions" = "MONETARY.ENTITLEMENTS.Conditions.of.entitlement.Earnings.related.conditions",
  "MONETARY ENTITLEMENTS Conditions of entitlement Income related conditions" = "MONETARY.ENTITLEMENTS.Conditions.of.entitlement.Income.related.conditions",
  "MONETARY ENTITLEMENTS Conditions of entitlement Assets savings related conditions" = "MONETARY.ENTITLEMENTS.Conditions.of.entitlement.Assets.savings.related.conditions",
  "MONETARY ENTITLEMENTS Conditions of entitlement Conditions related to the relationship to other family members" = "MONETARY.ENTITLEMENTS.Conditions.of.entitlement.Conditions.related.to.the.relationship.to.other.family.members",
  "MONETARY ENTITLEMENTS Conditions of entitlement Other entitlement conditions" = "MONETARY.ENTITLEMENTS.Conditions.of.entitlement.Other.entitlement.conditions",
  "Territorial application" = "Territorial.application"
)

initial_columns <- c(
  "Type.of.Policy",
  "Name.of.Policy",
  "Year.a.",
  "Valid.from.b.",
  "Valid.until.c.",
  "General.Functioning.Structure"
)

match(initial_columns, long_colnames_replacements)

colnames(timeline_data)[match(initial_columns, long_colnames_replacements)]


cols_to_show <- match(initial_columns, long_colnames_replacements)
cols_not_to_show <- setdiff(1:ncol(timeline_data), cols_to_show)

cols_not_to_show - 1


# 
# displayable_columns <- c(
#   "Type.of.policy",
#   "Name.Policy",
#   "Year.a.",
#   "Focus.of.Amendments.to.Policy",
#   "Legislative.basis",
#   "Valid.from.b.",
#   "Valid.until..c...",
#   "Valid.from...childbirth.related.date..d.",
#   "Valid.until...childbirth.related.date..e..",
#   "Recipient",
#   "type...Monetary.",
#   "type..time.",
#   "type..services.",
#   "General.functioning.structure",
#   "Territorial.application"
# )

date_of_childbirth_tickbox <- c("Valid.from.childbirth.related.date.d.", "Valid.until.childbirth.related.date.e.")



medium_and_long_columns <- list(
  "Focus of Amendments to Policy" = "medium",
  "Legislative Basis" = "medium",
  "General Functioning Structure" = "long",
  "NON MONETARY ENTITLEMENTS: Types of entitlement (f)" = "long",
  "NON MONETARY ENTITLEMENTS: Period of entitlement (g)" = "long",
  "NON MONETARY ENTITLEMENTS: Length of entitlement (h)" = "long",
  "NON MONETARY ENTITLEMENTS: Conditions of entitlement Employment related conditions" = "long",
  "NON MONETARY ENTITLEMENTS: Conditions of entitlement Conditions related to relationship to other family members" = "long",
  "NON MONETARY ENTITLEMENTS Conditions of entitlement Other entitlement conditions" = "long",
  "NON MONETARY ENTITLEMENTS Other details" = "long",
  "MONETARY ENTITLEMENTS Types of entitlement (f)" = "long",
  "MONETARY ENTITLEMENTS Period of entitlement" = "long",
  "MONETARY ENTITLEMENTS Length of entitlement" = "long",
  "MONETARY ENTITLEMENTS Rate of entitlement" = "long",
  "MONETARY ENTITLEMENTS Other details" = "long",
  "MONETARY ENTITLEMENTS Conditions of entitlement Age related conditions" = "long",
  "MONETARY ENTITLEMENTS Conditions of entitlement Employment related conditions" = "long",
  "MONETARY ENTITLEMENTS Conditions of entitlement Earnings related conditions" = "long",
  "MONETARY ENTITLEMENTS Conditions of entitlement Income related conditions" = "long",
  "MONETARY ENTITLEMENTS Conditions of entitlement Assets savings related conditions" = "long",
  "MONETARY ENTITLEMENTS Conditions of entitlement Conditions related to the relationship to other family members" = "long",
  "MONETARY ENTITLEMENTS Conditions of entitlement Other entitlement conditions" = "long",
  "Territorial application" = "medium"
)
