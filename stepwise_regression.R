data <- read.csv("combined.txt", header = TRUE)
str(data)

# run initial regression on all variables
model1 <- lm(graduation_4_year_school_pct_year_2 ~ ., data)
# running on all variables doesn't work initially because of the error below that indicates that a column that is a character or factor has only 1 unique value
# Error in `contrasts<+`(`*tmp*`, value = contr.funs[1 + isOF[nn]]) : 
# contrasts can be applied only to factors with 2 or more levels

# check which variables only have 1 unique value
sapply(lapply(data, unique), length)
# looks like it's the "student_pct_other_ethnicity" variable. Will drop this from the model
# tried running full model without that variable and it is still erroring out. Will try another approach
model1 <- lm(graduation_4_year_school_pct_year_2 ~ . - student_pct_other_ethnicity, data)

# run regression on variables that we identified as important in project proposal as a baseline model (also added in the drop out rate because that was identified as a strong linear relationship in EDA)
# no City_Region variable?
model1 <- lm(graduation_4_year_school_pct_year_2 ~ overall_rating + dress_code + student_pct_low_income + student_attainment_rating + culture_climate_rating + school_survey_involved_families + school_survey_supportive_environment + school_survey_ambitious_instruction + school_survey_effective_leaders + school_survey_collaborative_teachers + student_attendance_year_2_pct + sat_grade_11_score_school + one_year_dropout_rate_year_2_pct, data)
summary(model1)
# r-squared value is .8007, adjusted r-squared is .7819. Overall regression is significant with p-value of <2.2e-16
# significant variables at alpha of 0.05 include:
# overall_rating (all 5 dummy variables are significant)
# student_pct_low_income
# student_attainment_rating (1 of the 5 dummy variables is significant)
# school_survey_involved_families (2 of the 5 dummy variables are significant)
# school_survey_ambitious_instruction (1 of the 5 dummy variables is significant)
# school_survey_collaborative_teachers (1 of the 5 dummy variables is significant)
# student_attendance_year_2_pct
# one_year_dropout_rate_year_2_pct

# now build model with as many variables as I could fit without any errors
model2 <- lm(graduation_4_year_school_pct_year_2 ~ overall_rating + dress_code + student_pct_low_income + student_attainment_rating + culture_climate_rating + school_survey_involved_families + school_survey_supportive_environment + school_survey_ambitious_instruction + school_survey_effective_leaders + school_survey_collaborative_teachers + student_attendance_year_2_pct + sat_grade_11_score_school + attainment_act_sat_grade_11_pct + school_type + student_growth_rating + school_survey_student_response_rate_pct + school_survey_teacher_response_rate_pct + school_survey_safety + suspensions_per_100_students_year_2_pct + misconducts_to_suspensions_year_2_pct + average_length_suspension_year_2_pct + school_survey_school_community + school_survey_parent_teacher_partnership + school_survey_quality_of_facilities + teacher_attendance_year_2_pct + one_year_dropout_rate_year_2_pct + freshmen_on_track_school_pct_year_2 + growth_act_sat_grade_11_pct + college_enrollment_school_pct_year_2 + college_persistence_school_pct_year_2 + mobility_rate_pct + chronic_truancy_pct + school_hours + after_school_hours + classroom_languages + bilingual_services + refugee_services + hard_of_hearing + visual_impairments + transportation_bus + transportation_el + transportation_metra + rating_status + network + student_count_total + student_pct_special_ed + student_pct_english_learners + student_pct_black + student_pct_hispanic + student_pct_white + student_pct_asian + student_pct_native_american + student_pct_asian_pacific_islander + student_pct_multi + student_pct_hawaiian_pacific_islander + student_pct_ethnicity_not_available, data)
summary(model2)
# lots of NA values as coefficients in this model - why is this the case? Could be perfect collinearity between variables, could be too many NA values in the data itself?

# since there are so many variables, using forward selection as a variable selection tool might make sense rather than using the above full model
# start with model with only an intercept and run forward selection on that model
model3 <- lm(graduation_4_year_school_pct_year_2 ~ 1, data)
summary(model3)
# intercept shows that average graduation rate for all observations is 72.1994
forward_selection <- step(model3, scope = formula(lm(graduation_4_year_school_pct_year_2 ~ overall_rating + dress_code + student_pct_low_income + student_attainment_rating + culture_climate_rating + school_survey_involved_families + school_survey_supportive_environment + school_survey_ambitious_instruction + school_survey_effective_leaders + school_survey_collaborative_teachers + student_attendance_year_2_pct + sat_grade_11_score_school + attainment_act_sat_grade_11_pct + school_type + student_growth_rating + school_survey_student_response_rate_pct + school_survey_teacher_response_rate_pct + school_survey_safety + suspensions_per_100_students_year_2_pct + misconducts_to_suspensions_year_2_pct + average_length_suspension_year_2_pct + school_survey_school_community + school_survey_parent_teacher_partnership + school_survey_quality_of_facilities + teacher_attendance_year_2_pct + one_year_dropout_rate_year_2_pct + freshmen_on_track_school_pct_year_2 + growth_act_sat_grade_11_pct + college_enrollment_school_pct_year_2 + college_persistence_school_pct_year_2 + mobility_rate_pct + chronic_truancy_pct + school_hours + after_school_hours + classroom_languages + bilingual_services + refugee_services + hard_of_hearing + visual_impairments + transportation_bus + transportation_el + transportation_metra + rating_status + network + student_count_total + student_pct_special_ed + student_pct_english_learners + student_pct_black + student_pct_hispanic + student_pct_white + student_pct_asian + student_pct_native_american + student_pct_asian_pacific_islander + student_pct_multi + student_pct_hawaiian_pacific_islander + student_pct_ethnicity_not_available, data)), direction = "forward")
# results in error:
# Error in step(model3, scope = formula(lm(graduation_4_year_school_pct_year_2 ~  : 
# number of rows in use has changed: remove missing values?

# try forward selection using model1 as the scope instead
forward_selection <- step(model3, scope = formula(lm(graduation_4_year_school_pct_year_2 ~ overall_rating + dress_code + student_pct_low_income + student_attainment_rating + culture_climate_rating + school_survey_involved_families + school_survey_supportive_environment + school_survey_ambitious_instruction + school_survey_effective_leaders + school_survey_collaborative_teachers + student_attendance_year_2_pct + attainment_act_sat_grade_11_pct + one_year_dropout_rate_year_2_pct, data)), direction = "forward")
# still getting error

# try running with na.omit on the data to omit NAs
model4 <- lm(graduation_4_year_school_pct_year_2 ~ overall_rating + dress_code + student_pct_low_income + student_attainment_rating + culture_climate_rating + school_survey_involved_families + school_survey_supportive_environment + school_survey_ambitious_instruction + school_survey_effective_leaders + school_survey_collaborative_teachers + student_attendance_year_2_pct + attainment_act_sat_grade_11_pct + one_year_dropout_rate_year_2_pct, na.omit(data))
summary(model4)
forward_selection_2 <- step(model3, scope = formula(lm(graduation_4_year_school_pct_year_2 ~ overall_rating + dress_code + student_pct_low_income + student_attainment_rating + culture_climate_rating + school_survey_involved_families + school_survey_supportive_environment + school_survey_ambitious_instruction + school_survey_effective_leaders + school_survey_collaborative_teachers + student_attendance_year_2_pct + attainment_act_sat_grade_11_pct + one_year_dropout_rate_year_2_pct, na.omit(data))), direction = "forward")
# still errors out

# omitting all NAs takes the data set down from 645 to 155 observations - not a feasible option
data2 <- na.omit(data)
nrow(data2)

# what about just omitting NAs from variables we care about from the project proposal?
missing <- is.na(data3$overall_rating) |
  is.na(data3$dress_code) |
  is.na(data3$student_pct_low_income) |
  is.na(data3$student_attainment_rating) |
  is.na(data3$culture_climate_rating) |
  is.na(data3$school_survey_involved_families) |
  is.na(data3$school_survey_supportive_environment) |
  is.na(data3$school_survey_ambitious_instruction) |
  is.na(data3$school_survey_effective_leaders) |
  is.na(data3$school_survey_collaborative_teachers) |
  is.na(data3$student_attendance_year_2_pct) |
  is.na(data3$sat_grade_11_score_school) |
  is.na(data3$one_year_dropout_rate_year_2_pct)
sum(missing)
# there are 135 NA values among these variables

# remove NAs, there are now 510 observations in the data set
data3 <- subset(data3, subset = !missing)
nrow(data3)

# run regression now
model5 <- lm(graduation_4_year_school_pct_year_2 ~ overall_rating + dress_code + student_pct_low_income + student_attainment_rating + culture_climate_rating + school_survey_involved_families + school_survey_supportive_environment + school_survey_ambitious_instruction + school_survey_effective_leaders + school_survey_collaborative_teachers + student_attendance_year_2_pct + sat_grade_11_score_school + one_year_dropout_rate_year_2_pct, data3)
summary(model5)

# try backward elimination rather than forward selection
backward_elimination <- step(model5, direction = "backward")
# culture_climate_rating, school_survey_supportive_environment, school_survey_effective_leaders, dress_code were all removed

# run regression without those 4 variables
model6 <- lm(graduation_4_year_school_pct_year_2 ~ overall_rating + student_pct_low_income + student_attainment_rating + school_survey_involved_families + school_survey_ambitious_instruction + school_survey_collaborative_teachers + student_attendance_year_2_pct + sat_grade_11_score_school + one_year_dropout_rate_year_2_pct, data3)
summary(model6)
# r-squared is .7944, adjusted r-squared is .7819 (same adjusted r-squared as model1)
# important variables at alpha of 0.05 are now:
# overall_rating (all 5 dummy variables are significant)
# student_pct_low_income
# student_attainment_rating (1 of the 5 dummy variables is significant)
# school_survey_involved_families (1 of the 5 dummy variables is significant)
# school_survey_ambitious_instruction (2 of the 5 dummy variables are significant)
# school_survey_collaborative_teachers (2 of the 5 dummy variables are significant)
# student_attendance_year_2_pct
# one_year_dropout_rate_year_2_pct
# all variables are significant except for sat_grade_11_score_school

# test assumptions with residual analysis
plot(model6$fitted.values, model6$residuals, xlab = "Fitted Values", ylab = "Residuals")
abline(h = 0, lty = 2)
# constant variance might be violated as the variance seems to get smaller as the fitted values go up, but independence should hold

library(ggplot2)
library(ggpubr)
hist(model6$residuals, xlab = "Residuals")
qqnorm(residuals(model6))
qqline(residuals(model6), col = 2)
# histogram and qq-plot suggest relative normality of the residuals

# build model without dropout rate and overall rating since these are directly tied to graduation rate
model7 <- lm(graduation_4_year_school_pct_year_2 ~ student_attendance_year_2_pct + sat_grade_11_score_school + mobility_rate_pct + school_type + student_growth_rating + student_attainment_rating + culture_climate_rating + school_survey_student_response_rate_pct + school_survey_teacher_response_rate_pct + school_survey_involved_families, data)
summary(model7)
backward_elimination_2 <- step(model7, direction = "backward")
# culture_climate_rating, school_survey_teacher_response_rate_pct, school_survey_student_response_rate_pct were removed

# build model without these variables
model8 <- lm(graduation_4_year_school_pct_year_2 ~ sat_grade_11_score_school + school_survey_involved_families + school_type + student_attendance_year_2_pct + student_attainment_rating + student_growth_rating + mobility_rate_pct, data)
summary(model8)
# r-squared is only .7568





# use final imputed data to re-run analysis
imputed_data <- read.csv("final_imputed_fewer_columns.csv", header = TRUE)
str(imputed_data)
head(imputed_data)

# build model with all variables (except for X, school_id, and long_name)
imputed_model1 <- lm(graduation_4_year_school_pct_year_2 ~ . - X - school_id - long_name, imputed_data)
summary(imputed_model1)
# r-squared is .766, adjusted r-squared is .7421

# run backward elimination
imputed_backward_elimination <- step(imputed_model1, direction = "backward")

# build model using selected variables
imputed_model2 <- lm(graduation_4_year_school_pct_year_2 ~ school_type + beginning_year + 
                       school_survey_student_response_rate_pct + average_length_suspension_year_2_pct + 
                       student_attendance_year_2_pct + freshmen_on_track_school_pct_year_2 + 
                       college_persistence_school_pct_year_2 + mobility_rate_pct + 
                       refugee_services + visual_impairments + network + student_pct_low_income + 
                       student_pct_special_ed + student_pct_english_learners + student_growth_rating_ordinal + 
                       student_attainment_rating_ordinal + culture_climate_rating_ordinal + 
                       school_survey_involved_families_ordinal + school_hours_num_hours + 
                       transportation_el_num_distinct + transportation_metra_num_distinct, imputed_data)
summary(imputed_model2)
# r-squared is .7603, barely worse than full model
# adjust r-squared is .7467, better than full model
# overall p-value of model is 2.2e-16, suggesting overall model is stastically significant

# run forward selection
blank_model <- lm(graduation_4_year_school_pct_year_2 ~ 1, imputed_data)
imputed_forward_selection <- step(blank_model, scope = formula(imputed_model1), direction = "forward")

# build model using selected variables
imputed_model3 <- lm(graduation_4_year_school_pct_year_2 ~ mobility_rate_pct + student_pct_special_ed + 
                       freshmen_on_track_school_pct_year_2 + college_persistence_school_pct_year_2 + 
                       beginning_year + student_attendance_year_2_pct + student_growth_rating_ordinal + 
                       school_type + transportation_el_num_distinct + average_length_suspension_year_2_pct + 
                       transportation_metra_num_distinct + school_hours_num_hours + 
                       refugee_services + network + culture_climate_rating_ordinal + 
                       visual_impairments + school_survey_involved_families_ordinal + 
                       student_pct_low_income + student_attainment_rating_ordinal + 
                       school_survey_student_response_rate_pct + student_pct_english_learners, imputed_data)
summary(imputed_model3)
# same exact model as we got using backward elimination

# try forward-backward
imputed_forward_backward <- step(imputed_model1, scope = list(lower = formula(blank_model), upper = formula(imputed_model1)), direction = "both")

# build model using selected variables
imputed_model4 <- lm(graduation_4_year_school_pct_year_2 ~ school_type + beginning_year + 
                       school_survey_student_response_rate_pct + average_length_suspension_year_2_pct + 
                       student_attendance_year_2_pct + freshmen_on_track_school_pct_year_2 + 
                       college_persistence_school_pct_year_2 + mobility_rate_pct + 
                       refugee_services + visual_impairments + network + student_pct_low_income + 
                       student_pct_special_ed + student_pct_english_learners + student_growth_rating_ordinal + 
                       student_attainment_rating_ordinal + culture_climate_rating_ordinal + 
                       school_survey_involved_families_ordinal + school_hours_num_hours + 
                       transportation_el_num_distinct + transportation_metra_num_distinct, imputed_data)
summary(imputed_model4)
# same exact model as we got using backward elimination and forward selection