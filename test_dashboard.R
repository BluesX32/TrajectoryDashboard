devtools::load_all("h:/Myositis/TrajectoryDashboard")
#launch_trajectory_dashboard()


setwd("~/Myositis/TrajectoryDashboard")
# Real OMOP data - SAFE
con <- TrajectoryDashboard::create_connection_from_env()(".env")
# Real OMOP data - SAFER
con <- TrajectoryDashboard::create_safer_connection("R.env")

launch_trajectory_dashboard(con)


