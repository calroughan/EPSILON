using Distributed, Random, JuMP, Gurobi, Test, CategoricalArrays
if length(workers())==1
   addprocs(2)
end
@everywhere using  JuDGE, DelimitedFiles, DataFrames, Glob, CSV, Statistics

println("\n\nLDC model called")
println("\nWorkers: ", workers())
println("Processes: ", nprocs())

# JuDGE v0.55
# 19th September 2020
# cal.roughan@outlook.com

function master()

   @everywhere treefile = "45node-tree.csv"

   # Sets of constraints in operation
   global includeShutdownTech = false
   global normalHydroYear = true
   global fullyRenewable = false
   global oneStageLag = false

   # Can't have these on together
   @assert(fullyRenewable * normalHydroYear == 0)

   # Define a Gurobi environment and solver attributes
   # https://www.gurobi.com/documentation/9.0/refman/parameter_descriptions.html
   env = Gurobi.Env()
   JuDGE_MP_Solver = optimizer_with_attributes(() -> Gurobi.Optimizer(env), "OutputFlag" => 0, "Method" => 2, "Crossover" => 0, "MIPGap" => 0.0)
   JuDGE_SP_Solver = optimizer_with_attributes(() -> Gurobi.Optimizer(env), "OutputFlag" => 0, "MIPGap" => 0.0)
   JuDGE_DE_Solver = optimizer_with_attributes(() -> Gurobi.Optimizer(env), "OutputFlag" => 0, "MIPGap" => 0.0)

   # Load the n-node tree and ASCII-print the tree to the REPL
   global mytree, data = tree_from_file(joinpath(@__DIR__,treefile))
   @everywhere global dem_scale = CSV.read(joinpath(@__DIR__,(treefile)), DataFrame)
   JuDGE.print_tree(mytree,data[:demand_scale])

   # Define where the unprocessed Trading Period (TP) data is, and where the
   # generated LDCs / LBs should be stored
   @everywhere global TP_dir  = joinpath(@__DIR__, "TP_Portfolio_CSVs")
   @everywhere global LDC_dir = joinpath(@__DIR__, "Generated_LDCs")

   # CSV data extraction and processing functions
   function loadblock_demand(node)
      # Model the for each load block in each node
      # Returns the selected row, melted back to a 4x10 matrix of 10 load blocks
      # per season. Multiplied by whatever compounding factor or increase in
      # at each node that you specify

      nodename = string(node.name)

      if !ispath(joinpath(LDC_dir, nodename, "total_demand.csv"))

         global dem = []
         global dem_ref = []
         for season in ["Summer", "Autumn", "Winter", "Spring"]

            temp   = readdlm(joinpath(LDC_dir, nodename, "demand_"*season*".csv"), ',')[:, 2:end]
            temp_r = readdlm(joinpath(LDC_dir, nodename, "demand_"*season*".csv"), ',')[:, 1]
            if size(dem)[1] == 0
               global dem = deepcopy(temp)
               global dem_ref = deepcopy(temp_r)
            else
               global dem = hcat(dem, temp)
               global dem_ref = hcat(dem_ref, temp_r)
            end
         end
         CSV.write(joinpath(LDC_dir, nodename, "total_demand.csv"), DataFrame(dem), header=false)
         CSV.write(joinpath(LDC_dir, nodename, "Reference_matrix.csv"), DataFrame(dem_ref), header=false)
      else
         dem = readdlm(joinpath(LDC_dir, nodename, "total_demand.csv"), ',')
      end
      # dem = dem[1, :]
      return(dem)
   end

   function loadblock_hours(node)

      # Hours are stored in the root folder as they are constant across the tree
      # for each portfolio option
      nodename = "1"

      if !ispath(joinpath(LDC_dir, nodename, "total_hours.csv"))

         global hour = []
         for season in ["Summer", "Autumn", "Winter", "Spring"]

            # Note that this removes the identifying column from each season's CSV
            temp = readdlm(joinpath(LDC_dir, nodename, "hours_"*season*".csv"), ',')[:, 2:end]
            if size(hour)[1] == 0
               global hour = deepcopy(temp)
            else
               global hour = hcat(hour, temp)
            end
         end
         CSV.write(joinpath(LDC_dir, nodename, "total_hours.csv"), DataFrame(hour), header=false)
      else
         hour = readdlm(joinpath(LDC_dir, nodename, "total_hours.csv"), ',')
      end

      # hour = transpose(repeat(hour[1, :], 1, 32))

      return hour
   end

   function discount_investment_costs(node, original_cost, discount_rate)
      # Scale down the cost of investing in a plant at future nodes
      nodenumber = length(node.name)

      return original_cost * (discount_rate)^(nodenumber-1)
   end

   function AlphaHydroR(hours_matrix)

      hours_matrix = hours_matrix[1, :]

      # Read in the default alpha values
      alpha_High = permutedims(reshape(readdlm(joinpath(@__DIR__,("RunOfRiverNI_Alpha_values.csv")), ',')', (10, 4)))[:, 1]

      alpha_Low = zeros(Float64,4, 1)
      for t = 0:3
         alpha_Low[t+1] = 1 + (1 - alpha_High[t+1]) * sum(hours_matrix[10*t+1:10*t+3]) / sum(hours_matrix[10*t+8:10*t+10])
      end
      return hcat(repeat(alpha_High, 1, 3), ones(Int64, (4, 4)), repeat(alpha_Low, 1, 3))
   end

   function Scale_HydroR(tech_num, HydroR_number, year, mu_HydroR, alpha_HydroR)
      # Determine the LB MW production scaling concstraint multiplier, currently
      # only scaling for run-of-river (HydroR)

      function Scale_HydroR_Mu(tech_num, HydroR_number, year, mu_HydroR)
         # Returns a 1 x 4 matrix of mu values for a given tech type, or just
         # a 4 x 10 matrix of ones if the tech type will not be scaling LB max
         # load capacity.

         # Note that the input value of year should be a number

         # Define the run if river (Hydro_R) mu maxtrix for each year
         # mu_HydroR = readdlm(joinpath(@__DIR__,("RunOfRiverNI_Mu_values.csv")), ',')

         # Extract the years
         years = convert.(Int64, mu_HydroR[1, :])

         # Extract the actual data
         mu_HydroR = mu_HydroR[2:end, :]

         # Baseline scaling factor otherwise
         baseline = ones(Float64, (1, 4))

         if tech_num in HydroR_number
            for (idx, i) in enumerate(years)
               if i == year
                  # Return the seasonal mu values for a given year type
                  return mu_HydroR[:, idx]'
               end
            end
         else
            return baseline
         end
      end

      function Scale_HydroR_alphas(tech_num, HydroR_number, alpha_HydroR)
         # Returns a 4 x 10 matrix of alpha values for a given tech type, or just
         # a 4 x 10 matrix of ones if the tech type will not be scaling LB max
         # load capacity.

         # Define the run if river (Hydro_R) alpha value for each load block
         # alpha_HydroR = readdlm(joinpath(@__DIR__,("RunOfRiverNI_Alpha_values.csv")), ',')

         if tech_num in HydroR_number
            # return permutedims(reshape(alpha_HydroR', (10, 4)))
            return alpha_HydroR
         else
            return ones(Float64, (4, 10))
            # return baseline'
         end
      end

      # Determine the year type
      years = convert.(Int64, mu_HydroR[1, :])

      # Alpha and Mu values
      alpha = Scale_HydroR_alphas(tech_num, HydroR_number, alpha_HydroR)
      mu = Scale_HydroR_Mu(tech_num, HydroR_number, years[year], mu_HydroR)

      output = mu' .* alpha
      return permutedims(reshape(output', (40, 1)))
   end

   function HydroS_nu(nu_HydroS, year)

      # Returns a 1 x 4 matrix of mu values for a given tech type, or just
      # a 4 x 10 matrix of ones if the tech type will not be scaling LB max
      # load capacity.

      # Note that the input value of year should be a number

      # Define the stored hydro (Hydro_S) nu maxtrix for each type of year
      # nu_HydroS = readdlm(joinpath(@__DIR__,("StoredHydroNI_Nu_values.csv")), ',')

      # Extract the years
      years = convert.(Int64, nu_HydroS[1, :])

      # Extract the actual data
      nu_HydroS = nu_HydroS[2:end, :]

      # Baseline scaling factor otherwise
      baseline = ones(Float64, (1, 4))

      for (idx, i) in enumerate(years)
         if i == years[year]
            # Return the seasonal nu values for a given year type
            return nu_HydroS[:, idx]'
         end
      end
   end

   # Load Duration Curve (LDC) processing, split by root and not-root nodes
   function LDC_file_builder_at_root_helper(nodename, numblocks)

      # Loop over each season and extract the build files
      buildFiles = []
      for season in ["Summer", "Autumn", "Winter", "Spring"]
         for file in split.(glob("*_"*season*".csv", TP_dir), "\\")
            push!(buildFiles, split(file[end], ".")[1])
         end
      end

      println("LDC generator called for the root node")
      pmap(n -> LB_build(n, Int64(numblocks/4), nodename), buildFiles)
   end

   function LDC_file_builder_at_root_concatenation(LDC_dir, T, numPort)
       # This function turns each LDC CSV into rows of two matrices corresponding to
       # the demand in each block and the hours in each block

       # Listing of all LDC files in LDC_dir
       global demand_tot  = Array{Float64}(undef, numPort, 4*T)
       global hours_tot   = Array{Float64}(undef, numPort, 4*T)
       global ref_matrix  = Array{String}(undef, numPort, 4)
       for (j, season) in enumerate(["Summer", "Autumn", "Winter", "Spring"])

           # Get the files fpr each season
           global demand = []
           for file in split.(glob("demand_*"*season*".csv", joinpath(LDC_dir, "1")), "\\")
               push!(demand, split(file[end], ".")[1])
           end
           global hours = []
           for file in split.(glob("hours_*"*season*".csv", joinpath(LDC_dir, "1")), "\\")
               push!(hours, split(file[end], ".")[1])
           end
           global indices = []
           for file in split.(glob("indices_*"*season*".csv", joinpath(LDC_dir, "1")), "\\")
               push!(indices, split(file[end], ".")[1])
           end
           @assert(length(demand) == length(hours))
           @assert(length(demand) == length(indices))

           global indices_cat = Array{Float64}(undef, length(demand), T+1)
           for i in 1:numPort

             demand_tot[i, 10*(j-1)+1:10*j] = readdlm(joinpath(LDC_dir, "1", demand[i] * ".csv"), ',')[2:end]
             hours_tot[i, 10*(j-1)+1:10*j]  = readdlm(joinpath(LDC_dir, "1", hours[i] * ".csv"), ',')[2:end]
             indices_cat[i, :] = readdlm(joinpath(LDC_dir, "1", indices[i] * ".csv"), ',')[2:end]
             ref_matrix[i, j] = demand[i]

             rm(joinpath(LDC_dir, "1", demand[i] * ".csv"))
             rm(joinpath(LDC_dir, "1", hours[i] * ".csv"))
             rm(joinpath(LDC_dir, "1", indices[i] * ".csv"))
          end
          CSV.write(joinpath(LDC_dir, "1", "indices_"*season*".csv"), DataFrame(indices_cat), header=false)
       end

       CSV.write(joinpath(LDC_dir, "1", "total_demand.csv"), DataFrame(demand_tot), header=false)
       CSV.write(joinpath(LDC_dir, "1", "total_hours.csv"), DataFrame(hours_tot), header=false)
       CSV.write(joinpath(LDC_dir, "1", "Reference_matrix.csv"), DataFrame(ref_matrix), header=false)
   end

   function LDC_file_builder_at_root(node, LDC_tech, numblocks)

      # Check if the output directory exists
      nodename = string(node.name)
      @assert(nodename == "1")
      println("\n\nLooking at node ", nodename)

      if ispath(joinpath(LDC_dir, nodename))
         # Get all of the files in this dir
         files = glob("hours_*.csv", joinpath(LDC_dir, nodename))

         # Check that there are enough of them (one for each season)
         if ispath(joinpath(LDC_dir, nodename, "total_demand.csv"))
            println("LDCs already exist for node ", nodename, ". No action taken.")
         else
            println("No LDCs exist for node ", nodename, ". Generating LDCs.")
            LDC_file_builder_at_root_helper(nodename, numblocks)
            LDC_file_builder_at_root_concatenation(LDC_dir, Int64(numblocks/4), 2^LDC_tech)
            println("LDCs generated for node ", nodename)
         end
      else # If the path doesn't exist
         # Create the directory
         println("Directory does not exist for node ", nodename, ". Creating a directory.\nGenerating LDCs.")
         mkdir(joinpath(LDC_dir, nodename))

         # Run LDC code
         LDC_file_builder_at_root_helper(nodename, numblocks)
         LDC_file_builder_at_root_concatenation(LDC_dir, Int64(numblocks/4), 2^LDC_tech)
         println("LDCs generated for node ", nodename)
      end
   end

   @everywhere function LDC_file_fit_not_root_helper(nodename, numblocks)

      # Loop over each season and extract the fit files
      for season in ["Summer", "Autumn", "Winter", "Spring"]
         fitFiles = []
         for file in split.(glob("*_"*season*".csv", TP_dir), "\\")
            push!(fitFiles, split(file[end], ".")[1])
         end

         println("Fitting files for node ", nodename, " in ", season)
         LB_fit(fitFiles, Int64(numblocks/4), nodename, season)
      end
   end

   @everywhere function LDC_file_fit_not_root(node, LDC_tech, numblocks)

      # Check if the output directory exists
      nodename = string(node.name)
      @assert(nodename != "1")
      println("\n\nLooking at node ", nodename)

      if ispath(joinpath(LDC_dir, nodename))
         # Get all of the files in this dir
         files = glob("hours_*.csv", joinpath(LDC_dir, nodename))

         # Check that there are enough of them (one for each season)
         if ispath(joinpath(LDC_dir, nodename, "total_demand.csv"))
            println("LDCs already exist for node ", nodename, ". No action taken.")
         # elseif  length(files) > 0
         #    println("Number of LDCs does not align for node ", nodename, ". Terminating run.")
         #    error("Incorrect number of LDC files in node ", nodename)
         else
            println("No LDCs exist for node ", nodename, ". Generating LDCs.")

            LDC_file_fit_not_root_helper(nodename, numblocks)
            println("LDCs generated for node ", nodename)
         end
      else # If the path doesn't exist,

         # Create the directory
         println("Directory does not exist for node ", nodename, ". Creating a directory.\nGenerating LDCs.")
         mkdir(joinpath(LDC_dir, nodename))

         # Run LDC code
         LDC_file_fit_not_root_helper(nodename, numblocks)
         println("LDCs generated for node ", nodename)
      end
   end

   # Load block (LB) functions
   @everywhere function LB_BellmanFord(graph, src, dest, blks)
      ##  Shortest Path Algorithm to compute optimal load blocks from a weighted cost-matrix
      #   Returns a sequence of optimal indices for ordered dataset
       #Initialise
       n = length(graph[1, :])
       V = fill(Inf, n, blks)
       P = fill(Inf, n, blks)

       for k = 1:blks
           V[1, k] = 0
           P[1, k] = 1
       end

       for j = 2:n
           for k = 1:blks
               V[j, k] = graph[1, j]
               P[j, k] = 1
           end

           for i = 1:j-1
               for k = 2:blks
                   if V[i, k-1] + graph[i, j] < V[j, k]
                       V[j, k] = V[i, k-1] + graph[i, j]
                       P[j, k] = i
                   end
               end
           end
       end

       seq = Array{Int64}(undef, 1, blks + 1)
       seq[blks+1] = n
       seq[blks] = P[n, blks]

       for i = blks-1:-1:1
           prevState = P[seq[i+1], i]
           seq[i] = prevState
       end

       return seq
   end

   @everywhere function LB_duration_builder(blocks, indices)
      ##  Compute block duration of optimal blocks
       for i = 2:length(indices)
           blocks[i-1] = (indices[i] - indices[i-1]) / 2
       end
   end

   @everywhere function LB_processing_file_helper(D, costs, y, p)
       n = length(y)
       Threads.@threads for i = 1:n
           dk = y[i]
           @inbounds begin
               for j = (i+1):n
                   dk += y[j]
                   db = dk / (j - i + 1)
                   D[i, j] += db
                   z = 0.0
                   for k = i:j
                       z += p[k] * abs(y[k] - db)
                   end
                   costs[i, j] += z
               end
           end
       end
       return
   end

   @everywhere function LB_processing_file(files::Array, nodename)
      ##  Data pre-processing to compute mean demands & cost matrix for DP

       # Initialise D, costs for first file
       # n = length(readdlm(joinpath(@__DIR__, (files[1] * ".csv")), ',')[2:end,1])
       n = length(readdlm(joinpath(TP_dir, (files[1] * ".csv")), ',')[2:end,1])

       # Get the demand scaling increase
       scale_factor = dem_scale[dem_scale[!, :n] .== parse(Int64, nodename), :demand_scale]
       solar_MW = dem_scale[dem_scale[!, :n] .== parse(Int64, nodename), :solar_MW]

       D = zeros(n, n)
       costs = zeros(n, n)

       for f = 1:length(files)
           data = readdlm(joinpath(TP_dir, (files[f] * ".csv")), ',')
           # Below is explained in comments in the function LB_fit()
           raw_load = data[2:end, 7]
           solar = data[2:end, 6]
           y = Float64.(data[2:end, 2])
           y = y .+ (raw_load .* scale_factor) .- raw_load - solar_MW .* solar
           y[y .< 0] .= 0
           p = Float64.(data[2:end, 3])

           # println("LB_processing_file_helper function called")
           LB_processing_file_helper(D, costs, y, p)
           # println("LB_processing_file_helper function ended")
       end
       return D, costs, n
   end

   @everywhere function LB_average_load(D, path)
      ##  Form a sequence containing the average demand in computed load blocks
      #   Input: Mean Demand Matrix D; path= sequence of indices corresponding to optimal LB starts
       avgedLoad = []
       for i = 1:length(path)-1
           s = path[i]
           t = path[i+1]
           push!(avgedLoad, D[s, t])
       end
       return avgedLoad
   end

   @everywhere function LB_fit(files_fit, T, nodename, season)

      # Get the demand scaling increase
      scale_factor = dem_scale[dem_scale[!, :n] .== parse(Int64, nodename), :demand_scale]
      solar_MW = dem_scale[dem_scale[!, :n] .== parse(Int64, nodename), :solar_MW]

      # Read in the indices
      indices = Int64.(readdlm(joinpath(LDC_dir, "1", "indices_"*season*".csv"), ','))

      # Define an empty matrix
      avg = zeros(Float64, T, length(files_fit))

      # loop through each file in files_fit
      for f = 1:length(files_fit)

         df = readdlm(joinpath(TP_dir, (files_fit[f] * ".csv")), ',')

         # Read in the raw load, this has had no wind generation subtracted such
         # that the MW scaling increase can be calculated
         raw_load = df[2:end, 7]
         solar = df[2:end, 6]

         # Read in the data and add to each TP the incremental MW scaling factor
         # based on the demand. Also subtract the rooftop solar
         data = df[2:end, 2] .+ (raw_load .* scale_factor) .- raw_load - solar_MW .* solar
         # Ensuring non-negativity of TP data
         data[data .< 0] .= 0

         # Loop through the number of time periods and compute the mean
         for i in 1:T
            avg[i,f] = Statistics.mean(data[indices[f, i]:indices[f, i+1]])
         end
      end

      df = DataFrame(hcat(Array{Any}(DataFrame(split.(files_fit, "_"))[3, :]), permutedims(avg)))
      CSV.write(joinpath(LDC_dir, nodename, "demand_"*season*".csv"), DataFrame(df), header=false)
   end

   @everywhere function LB_build(files_build, T, nodename)

      season = split(files_build, "_")[4]
      portfolio = split(files_build, "_")[3]

      println("Running ", files_build)
      #Data pre-processing to compute optimal Load Blocks
      D, costs, n = LB_processing_file([files_build], nodename)

      #Calling functions to compute output
      indices = LB_BellmanFord(costs, 1, n, T)
      avgLoad = LB_average_load(D, Int64.(indices))

      blocklengths = Array{Float64}(undef, 1, T)
      LB_duration_builder(blocklengths, indices)

      df = DataFrame(hcat(files_build, permutedims(avgLoad)))
      df_hours = DataFrame(hcat(portfolio, blocklengths))
      df_inds = DataFrame(hcat(portfolio, indices))

      CSV.write(joinpath(LDC_dir, nodename, "demand_"*portfolio*"_"*season*".csv"), DataFrame(df), header=false)
      CSV.write(joinpath(LDC_dir, nodename, "hours_"*portfolio*"_"*season*".csv"), DataFrame(df_hours), header=false)
      CSV.write(joinpath(LDC_dir, nodename, "indices_"*portfolio*"_"*season*".csv"), DataFrame(df_inds), header=false)
   end

   function sub_problems(node)
      # node = mytree
      println("Initialising node: ", node.name)

      # Define the model to be optimised using Gurobi
      # model = Model(optimizer_with_attributes(Gurobi.Optimizer, "OutputFlag" => 0))
      model = Model(JuDGE_SP_Solver)

      # Read in files
      mu_HydroR = readdlm(joinpath(@__DIR__,("RunOfRiverNI_Mu_values.csv")), ',')
      nu_HydroS = readdlm(joinpath(@__DIR__,("StoredHydroNI_Nu_values.csv")), ',')
      data = readdlm(joinpath(@__DIR__,("Costs_and_parameters.csv")), ',')
      demand_matrix = loadblock_demand(node)
      hours_matrix = loadblock_hours(node)
      alpha_HydroR = AlphaHydroR(hours_matrix)

      # # # # # # # # # # # # User-defined parameters # # # # # # # # # # # # #
      # Ensure that the variables in the CSV line up with those here.
      # Descriptions can be found in the accompanying csv
      K = Array{Float64}(data[2, 3:end])
      C = Array{Float64}(data[3, 3:end])
      L = Array{Float64}(data[4, 3:end])
      U = Array{Float64}(data[5, 3:end])
      u = Array{Float64}(data[6, 3:end])
      em = Array{Float64}(data[7, 3:end])

      K_shut = Array{Float64}(data[8, 3:4])
      C_shut = Array{Float64}(data[9, 3:4])
      L_shut = Array{Float64}(data[10, 3:4])
      U_shut = Array{Float64}(data[11, 3:4])
      em_shut = Array{Float64}(data[12, 3:4])

      # Number of base_tech and LDC_tech types
      # Note that LDC_tech alter the LDC selected, while base_tech don't
      # It is a requirement to list all base tech first, then include LDC tech,
      # based on how the constraints have been coded
      base_tech, LDC_tech, total_tech, shutoff_tech = data[13:16, 3]
      @assert(total_tech == base_tech + LDC_tech)
      @assert(shutoff_tech == 2)

      Huntly_num, Huntly_shut = data[17:18, 3]
      geot_inds = Array{Int64}(data[19, 3:6])
      HydroR_number = Array{Int64}(data[20, 3:6])
      num_HydroR = data[21, 3]
      HydroS_number, num_HydroS, gatewidth = data[22:24, 3]

      battery_inds, num_battery, battery_efficiency = data[25:27, 3]
      beta = Array{Float64}(data[28, 3:(2+num_battery)])

      V, Em, discount_rate, numblocks, histyears, hydro_year_em = data[29:34, 3]
      @assert(numblocks % 4 == 0)

      # Extract the carbon price from the tree
      c_carbon = dem_scale[dem_scale[:, :n] .== parse(Int64, node.name), :carbon][1]

      # # # # # # # # # # # # # # # MIP variables # # # # # # # # # # # # # # #

      # At each node, we have one binary variable x which models the whether or not an expansion will take place at the node
      # Note that this includes an investment delay. Any new investment will not be operational until the next stage.
      if oneStageLag
         @expansion(model, x[1:total_tech], 1)
      else
         @expansion(model, x[1:total_tech])
      end

      # Define the variable x_dummy have a 1-1 correspondence with the number of investment variables
      @variable(model, x_dummy[1:LDC_tech], Bin)

      # Define the variable lambda which selects the chosen LDC based on the investment portfolio
      @variable(model, lambda[1:2^LDC_tech], lower_bound = 0)

      # Define the variable r which models the load shed in each load block
      @variable(model, r[1:numblocks, 1:histyears], lower_bound = 0)

      # Define the variable Y models the MW produced in each load block, for each technology type
      @variable(model, y[1:total_tech, 1:numblocks, 1:histyears], lower_bound = 0)

      # Define the variable z which models the MWh capacity invested in a specific technology
      @variable(model, z[1:total_tech, 1:numblocks], lower_bound = 0)

      # Define the variable exp which models the expansion capacity of each tech type
      @variable(model, exp[1:total_tech, 1:numblocks], lower_bound = 0)

      # Define the variable hydrogates, which models centre of the bands through which each season's
      # hydro storage must flow through over all historical years. The seasons are -1, 0, 1, 2, 3
      # Note that season_-1 == season_3
      @variable(model, hydrogates[1:5], lower_bound = 0)

      # Model the actual stored hydro in each season and historical year scenario
      @variable(model, stored_hydro[1:5, 1:histyears], lower_bound = 0)

      # Variables for storing battery charge and discharge amounts across each season (MWh)
      @variable(model, Charge[1:numblocks, 1:histyears, 1:num_battery], lower_bound = 0)
      @variable(model, Discharge[1:numblocks, 1:histyears, 1:num_battery], lower_bound = 0)

      # Define a variable that tracks the selected LDC
      @variable(model, SelectedLDC[1:numblocks, 1:histyears])

      if includeShutdownTech
         @shutdown(model, x_shut[1:shutoff_tech])
         @variable(model, y_shut[1:shutoff_tech, 1:numblocks, 1:histyears], lower_bound = 0)
         @variable(model, z_shut[1:shutoff_tech, 1:numblocks], lower_bound = 0)
         U[1] = 0
         U[2] = 0
      end

      # Define a binary variable that determines whether emissions are allowed in that year
      if (normalHydroYear) & (length(node.name) >= 4)
         @variable(model, allowed_Emissions[1:histyears], Bin)
      end

      # Halt all emissions post 2030
      if (fullyRenewable) & (length(node.name) >= 3)
         Em = 0
      end

      # Based on whichever stage we decide to shut Huntly down in, remove all available capacity. This is an explicit shutdown.
      if length(node.name) >= Huntly_shut
         U[Huntly_num] = 0
      end

      # # # # # # # # # # # # # MIP constraints # # # # # # # # # # # # #
      # The cost of expansion is the discounted capital cost of investment * whether we invest * the investment size.
      # However this has been replaced with @ongoingcosts(). This is the cost per stage annualised over the lifespan.
      @ongoingcosts(model, sum(5 * discount_investment_costs(node, K[k], discount_rate) * u[k] * x[k] for k in 1:total_tech))

      # Salvage value of NZ$ 5 million per site (discounted)
      if includeShutdownTech
         @capitalcosts(model, - sum(discount_investment_costs(node, 5e6, discount_rate) * x_shut[k] for k in 1:shutoff_tech))
      end

      # Investment variables are upper bounds on the dummy vars
      # Was an @expansionconstraint() in older versions of JuDGE
      @constraint(model, C1_x_dummy_UB[k in 1:LDC_tech], x_dummy[k] <= x[k+base_tech])

      # Only one 𝚲 can be chosen
      @constraint(model, C2_OneLambda, sum(lambda[k] for k in 1:2^LDC_tech) == 1)

      # Calculate the binary representation for the 'i'th combination of investments.
      # Pad the MSBs with zeros for binary representations that are shorter than
      # the number of investments
      # Iterate through each bit of bin_rep, if it is a 1, add the value x_dummy[idx]
      # to the constraint, otherwise add 1 - x_dummy[idx]. Sum over each x_dummy,
      # Finally subtract LDC_tech - 1 from the constraint.
      # Collectively referred to as 'Constraint 3'
      for i in 0:2^LDC_tech - 1
         bin_rep = string(i, base = 2, pad = LDC_tech)
         @constraint(model, lambda[i+1] >= sum(parse(Int, j, base = 10)*(2*x_dummy[idx]-1) + 1 - x_dummy[idx] for (idx, j) in enumerate(bin_rep)) - (LDC_tech - 1))
      end

      # Constrain the LDC selection
      @constraint(model, C4_LDC_selection[b in 1:numblocks, h in 1:histyears], SelectedLDC[b, h] == sum(lambda[i]*demand_matrix[i, b]*hours_matrix[i, b] for i in 1:2^LDC_tech) + sum(Charge[b, h, k] - Discharge[b, h, k] for k in 1:num_battery))

      # MWh Capacity invested in each tech
      @constraint(model, C5_z_UB[k in 1:total_tech, b in 1:numblocks], z[k, b] <= U[k] * sum(lambda[i]*hours_matrix[i, b] for i in 1:2^LDC_tech) + exp[k, b])

      if includeShutdownTech
         @constraint(model, C5_z_UB_sh[k in 1:shutoff_tech, b in 1:numblocks], z_shut[k, b] <= U_shut[k] * sum(lambda[i]*hours_matrix[i, b] for i in 1:2^LDC_tech))
         @constraint(model, C5_z_UB_sh2[k in 1:shutoff_tech, b in 1:numblocks], z_shut[k, b] <= 1e15 * (1 - x_shut[k]))
      end

      # MWh expansion for each tech type, using a BigM approach
      @constraint(model, C6_exp_UB1[k in 1:total_tech, b in 1:numblocks], exp[k, b] <= 1e8 * x[k])
      @constraint(model, C7_exp_UB2[k in 1:total_tech, b in 1:numblocks], exp[k, b] <= u[k] * sum(lambda[i]*hours_matrix[i, b] for i in 1:2^LDC_tech))

      # The load shed cannot exceed the total load in each load block of the specific LDC chosen
      @constraint(model, C8_r_UB[b in 1:numblocks, h in 1:histyears], r[b, h] <= SelectedLDC[b, h])

      # MW Production must be less than the invested capacity
      # Note that the MW production is only scaled for HydroR technology (so far)
      @constraint(model, C9_hydroR_UB[k in 1:total_tech, b in 1:numblocks, h in 1:histyears], y[k, b, h] <= z[k, b] * Scale_HydroR(k, HydroR_number, h, mu_HydroR, alpha_HydroR)[b])

      if includeShutdownTech
         @constraint(model, C9_hydroR_UB_sh[k in 1:shutoff_tech, b in 1:numblocks, h in 1:histyears], y_shut[k, b, h] <= z_shut[k, b])
      end


      # Gate levels of the first and last season must be the same.
      # Set an arbitrary lower bound too (implied minimum reserves etc etc)
      @constraint(model, C10_hydrogates, hydrogates[1] == hydrogates[5])
      @constraint(model, C10_test[t in 1:5], hydrogates[t] >= 4e5)

      # The amount of stored hydro in each historical year must pass through the specific season's gates
      @constraint(model, C11_storedhydro_UB[t in 1:5, h in 1:histyears], stored_hydro[t, h] <= hydrogates[t] + gatewidth)
      @constraint(model, C12_storedhydro_LB[t in 1:5, h in 1:histyears], stored_hydro[t, h] >= hydrogates[t] - gatewidth)

      # Generation scaling for stored hydro for each season, accounting for the gates
      # @constraint(model, C13_hydro[t in 0:3, h in 1:histyears], sum(y[HydroS_number, b+10*t, h] for b in 1:10) <= HydroS_nu(nu_HydroS, h)[t+1] * sum(z[HydroS_number, bb+10*t] for bb in 1:10) - stored_hydro[t+2, h] + stored_hydro[t+1, h])
      @constraint(model, C13_hydro[t in 0:3, h in 1:histyears], sum(sum(y[zz, b+10*t, h] for zz in HydroS_number:(HydroS_number+num_HydroS-1)) for b in 1:10) <= HydroS_nu(nu_HydroS, h)[t+1] * sum(sum(z[zz, bb+10*t]  for zz in HydroS_number:(HydroS_number+num_HydroS-1)) for bb in 1:10) - stored_hydro[t+2, h] + stored_hydro[t+1, h])

      # MW Production + load shed must exceed the demand at each node
      # The difference between the two options is to constrain the LB on all y variables,
      # or just the base techs. As far as I can tell, this does not change the final solution
      if includeShutdownTech
         @constraint(model, C14_y_and_r_LB[b in 1:numblocks, h in 1:histyears], sum(y[k, b, h] for k in 1:base_tech) + sum(y_shut[kk, b, h] for kk in 1:shutoff_tech) >=  SelectedLDC[b, h] - r[b, h])
      else
         @constraint(model, C14_y_and_r_LB[b in 1:numblocks, h in 1:histyears], sum(y[k, b, h] for k in 1:base_tech) >=  SelectedLDC[b, h] - r[b, h])
      end

      # Battery round trip efficiency constraint: eta * sum(charge) == sum(discharge)
      @constraint(model, C15_charge_eta[t in 0:3, h in 1:histyears, k in 1:num_battery], battery_efficiency * sum(Charge[b+10*t, h, k] for b = 1:10) == sum(Discharge[bb+10*t, h, k] for bb = 1:10))

      # Maximum charge amount of battery must be less than the total possible charge over a season, assuming one charge per day
      @constraint(model, C16_max_charge[t in 0:3, h in 1:histyears, k in 1:num_battery], sum(Charge[b+10*t, h, k] for b = 1:10) <= (1 / 24) * sum(z[battery_inds+k-1, bb+10*t] for bb = 1:10))

      # Battery charge rate constraint
      @constraint(model, C17_batt_charge[t in 0:3, b in 1:10, h in 1:histyears, k in 1:num_battery], Charge[b+10*t, h, k] <= beta[k] * z[battery_inds+k-1, b+10*t])

      # Battery "generation" is accounted for in the charge and discharge constraints, hence we don't want any MWh generation
      # In the corresponding y variables. This is valid and similar to how wind is treated
      @constraint(model, C17_batt[k in 0:(num_battery-1), b in 1:numblocks, h in 1:histyears], y[battery_inds + k, b, h] <= 0)


      # Define a binary variable that determines whether emissions are allowed in that year
      if (normalHydroYear) & (length(node.name) >= 4)
         @constraint(model, C19_norm_hydro_year, sum(allowed_Emissions[h] for h in 1:histyears) <= hydro_year_em)
         if includeShutdownTech
            @constraint(model, C18_emissions[h in 1:histyears], sum(em[k]*sum(y[k, b, h] for b in 1:numblocks) for k in 1:total_tech if !issubset(k, geot_inds)) + sum(em_shut[k]*sum(y_shut[k, b, h] for b in 1:numblocks) for k in 1:shutoff_tech) <=  Em * allowed_Emissions[h])
         else
            @constraint(model, C18_emissions[h in 1:histyears], sum(em[k]*sum(y[k, b, h] for b in 1:numblocks) for k in 1:total_tech if !issubset(k, geot_inds)) <=  Em * allowed_Emissions[h])
         end
      end

      # Halt all emissions post 2030
      if (fullyRenewable) & (length(node.name) >= 3)
         if includeShutdownTech
            @constraint(model, C18_emissions[h in 1:histyears], sum(em[k]*sum(y[k, b, h] for b in 1:numblocks) for k in 1:total_tech if !issubset(k, geot_inds)) + sum(em_shut[k]*sum(y_shut[k, b, h] for b in 1:numblocks) for k in 1:shutoff_tech) <= Em)
         else
            # @constraint(model, C18_emissions[h in 1:histyears], sum(em[k]*sum(y[k, b, h] for b in 1:numblocks) for k in 1:total_tech if !issubset(k, geot_inds)) <= Em)
            @constraint(model, C18_emissions[h in 1:histyears], sum(em[k]*sum(y[k, b, h] for b in 1:numblocks) for k in 1:total_tech) <= Em)
         end
      end

      # @constraint(model, C18_emissions[h in 1:histyears], sum(em[k]*sum(y[k, b, h] for b in 1:numblocks) for k in 1:total_tech) <= Em)


      # Minimise the sum of the annual maintenance of the invested capacity + the sum over all load blocks of the variable cost * MW produced minus the VOLL * load shed
      # @sp_objective(model, sum(sum(L[kk] * (U[kk] + u[kk] * x[kk]) for kk in 1:total_tech) + sum(sum((C[k] + c_carbon * em[k]) * y[k, b, h] for k in 1:total_tech)  + V*r[b, h] for b in 1:numblocks) for h in 1:histyears))
      # @sp_objective(model, sum(L[k] * (U[k] + u[k] * x[k]) for k in 1:total_tech) +
      #                      sum(L_shut[k] * (U_shut[k] * (1-x_shut[k])) for k in 1:shutoff_tech) +
      #                      sum(sum(sum((C[k] + c_carbon * em[k]) * y[k, b, h] for k in 1:total_tech) +
      #                      sum((C_shut[k] + c_carbon * em_shut[k]) * y_shut[k, b, h] for k in 1:shutoff_tech) +
      #                      V*r[b, h] for b in 1:numblocks) for h in 1:histyears))
      if includeShutdownTech
         @sp_objective(model, 5*(sum(L[k] * (U[k] + u[k] * x[k]) for k in 1:total_tech) +
                              sum(L_shut[k] * (U_shut[k] * (1-x_shut[k])) for k in 1:shutoff_tech) +
                              sum(sum(sum((C[k] + c_carbon * em[k]) * y[k, b, h] for k in 1:total_tech) +
                              # sum((C[k+battery_inds-1] + c_carbon * em[k+battery_inds-1]) * Charge[b, h, k] for k in 1:num_battery) +
                              sum((C_shut[k] + c_carbon * em_shut[k]) * y_shut[k, b, h] for k in 1:shutoff_tech) +
                              V*r[b, h] for b in 1:numblocks) for h in 1:histyears)))
      else
         @sp_objective(model, 5*(sum(L[k] * (U[k] + u[k] * x[k]) for k in 1:total_tech) +
                              sum(sum(sum((C[k] + c_carbon * em[k]) * y[k, b, h] for k in 1:total_tech) +
                              # sum((C[k+battery_inds-1] + c_carbon * em[k+battery_inds-1]) * Charge[b, h, k] for k in 1:num_battery) +
                              V*r[b, h] for b in 1:numblocks) for h in 1:histyears)))
      end

      return model
   end

   # LDC CSV generation function call
   LDC_tech = 5
   numblocks = 40
   @assert(numblocks % 4 == 0)      # Must be a multiple of 4

   # A separate root builder exists that pmaps internally
   LDC_file_builder_at_root(mytree, LDC_tech, numblocks)
   pmap(n->LDC_file_fit_not_root(n, LDC_tech, numblocks),collect(mytree)[2:end])

   judy = JuDGEModel(mytree, ConditionallyUniformProbabilities, sub_problems, JuDGE_MP_Solver)
   JuDGE.solve(judy)

   println("\nObjective: ", objective_value(judy.master_problem), "\n")

   # Resolve the model to fix the expansions such that once an expansion is made
   # at a parent node, all child nodes can make use of such an expansion.
   # JuDGE.print_expansions(judy,onlynonzero=false)
   JuDGE.fix_expansions(judy)
   println("\nRe-solved Objective: ", JuDGE.resolve_fixed(judy))
   JuDGE.print_expansions(judy,onlynonzero=true)
   # JuDGE.print_expansions(judy,onlynonzero=false)

   # Export the output to a csv file
   # JuDGE.write_solution_to_file(judy,joinpath(@__DIR__,"JuDGE_expansions_3node2_case_study_1.csv"))
   JuDGE.write_solution_to_file(judy,joinpath(@__DIR__,"JuDGE_expansions_3.csv"))
   # JuDGE.write_solution_to_file(judy,joinpath(@__DIR__,"JuDGE_expansions_341node2_case_study_3.csv"))

   # Deteq takes yonks to run, only run this for small problems
   # deteq = DetEqModel(mytree, ConditionallyUniformProbabilities, sub_problems, JuDGE_DE_Solver)
   # JuDGE.solve(deteq)
   # println("Deterministic Equivalent Objective: ", objective_value(deteq.problem))
   return objective_value(judy.master_problem)
end

# # # # Should be fixed now, however I've left the code for it if returns.
# This annoying error appears when running master() for the first time, however
# does not appear when run a second time. This try / catch clause should catch
# that.
# "Error: The applicable method may be too new: running in world age xx,
# while current world is yy"
# Seems to be a Julia bug or an issue with the parallelisation.
# try
#    @time begin
#    master()
#    end
# catch err
#    if isa(err, MethodError)
#       println("\n\'Running in world age\' error appeared. Rerunning master()\n")
#       @time begin
#       master()
#       end
#    end
# end

@time begin
master()
end
