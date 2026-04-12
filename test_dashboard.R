devtools::load_all("h:/Myositis/TrajectoryDashboard")
#launch_trajectory_dashboard()


setwd("~/Myositis/TrajectoryDashboard")
# Real OMOP data
con <- create_connection_from_env(".env")
launch_trajectory_dashboard(con)

