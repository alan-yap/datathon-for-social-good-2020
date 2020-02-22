
#--- Runs all project code and reproduces output for code review.
# Jen Shen, 19/01/2020

source ('setup.R')

#--- segment into three sections: Fundamental, Standard and Quick build for comprehensiveness vs. speed

run_all_r_scripts ('2_Code', push_through = TRUE)


#--- Custom error checks
# check 1
# check 2
# check 3

cleanup ()
