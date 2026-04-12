devtools::load_all("h:/Myositis/TrajectoryDashboard")
#launch_trajectory_dashboard()


setwd("~/Myositis/TrajectoryDashboard")
# Real OMOP data - SAFE
con <- create_connection_from_env(".env")
# Real OMOP data - SAFER
con <- create_safer_connection("R.env")

launch_trajectory_dashboard(con)


