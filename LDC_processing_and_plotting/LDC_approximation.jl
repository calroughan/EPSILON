using DelimitedFiles, Glob, Plots

## Helper functions
#= Input demand matrix computed for all combinations of i/j and the shortest path computed,
    then return the mean load for the load blocks computed =#

##  Form a sequence containing the average demand in computed load blocks
#   Input: Mean Demand Matrix D; path= sequence of indices corresponding to optimal LB starts

function meanLoad(D, path)
    avgedLoad = []
    for i = 1:length(path)-1
        s = path[i]
        t = path[i+1]
        push!(avgedLoad, D[s, t])
    end
    return avgedLoad
end

##  Shortest Path Algorithm to compute optimal load blocks from a weighted cost-matrix
#   Returns a sequence of optimal indices for ordered dataset

function BellmanFord(graph, src, dest, blks)
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

##  Compute block duration of optimal blocks

function blockDuration(blocks, indices)
    for i = 2:length(indices)
        blocks[i-1] = (indices[i] - indices[i-1]) / 2
    end

end

## Write LB TO CSV

function writeLBtoCSV(filename, filelist, indices, avgLoad, blocklengths, T)

    #Print outputs to new file
    # writefile = joinpath(@__DIR__, ("LDC_" * filename * ".csv"))
    writefile = joinpath(dirrr, ("LDC_" * filename * ".csv"))
    file = open(writefile, "w")

    print(file, "LB_Indices")
    for i= 1:length(avgLoad[1,:])
        print(file, ",Mean_Load_",  filelist[i])
    end
    println(file, ",LB_Duration")

    for i = 1:T
        print(file, string(indices[i]))
        for f= 1:length(filelist)
            print(file, "," * string(avgLoad[i,f]))
        end
        println(file, "," * string(blocklengths[i]))
    end

    close(file)
end

## Delete Leap Year Data
#   Leap year data can cause issue with Standardizing that assumes constant n

function deleteLY(data::Array)
    isLY= length(findall(x -> x == "29/2/*"), data[:,4])
    if isLY > 0
        for i= 1:length(isLY)
            data[]
        end
    end
end

## Visualise the Load block approximation of an input LDC
function plotLB(file::String, loadfile::String, title::String)

        #file = file name
        #n = number of data points in original file that LB defined from (so number of days*48)

        #File name for LB data to be read in
        filename= joinpath(@__DIR__,(file*".csv"))
        data= readdlm(filename, ',')
        data= data[2:end, :]

        #File name for LDC data to be read in
        filename= joinpath(@__DIR__,(loadfile*".csv"))
        loaddata= readdlm(filename, ',')
        loaddata= loaddata[2:end, :]
        numHrs= (length(loaddata[:,2])-1)/2

        #Define a rectangle plotting object for the graph
        #args are width, height, x start and y start
        rectangle(w, h, x, y) = Shape(x.+ [0,w,w,0], y .+ [0,0,h,h])

        #Plot curve
        plotobj= plot(legend=false, margin = 5Plots.mm) #,  background_color = :transparent)                                                            #empty plot object
<<<<<<< HEAD
        plot!(rectangle(data[1,3], data[1,2], 0, 0), color= "blue", opacity = 0.7, linewidth = 0.1)
        for i= 2:length(data[:,1])
            plot!(rectangle(data[i,3], data[i,2] ,sum(data[1:i-1,3]) ,0), opacity=0.7, color= "blue", linewidth = 0.1)
        end

        plot!([x for x=0:0.5:numHrs], loaddata[:,2], linewidth = 3, color= "magenta")
=======
        plot!(rectangle(data[1,3], data[1,2], 0, 0), color= "darkgreen", opacity = 0.5, linewidth = 0.15)
        for i= 2:length(data[:,1])
            plot!(rectangle(data[i,3], data[i,2] ,sum(data[1:i-1,3]) ,0), opacity=0.5, color= "darkgreen", linewidth = 0.15)
        end

        plot!([x for x=0:0.5:numHrs], loaddata[:,2], linewidth = 3, color= "royalblue")
>>>>>>> e36df7e0622d6ef050f4a8658cefa290f9136723

        title!(title)
        yaxis!("Load (MW)")
        xaxis!("Hours Utilised")
        display(plotobj)

        #Save pic
        savefig("LDC.png")
 end

##  Data pre-processing to compute mean demands & cost matrix for DP

function process_file(files::Array)

    # Initialise D, costs for first file
    # n = length(readdlm(joinpath(@__DIR__, (files[1] * ".csv")), ',')[2:end,1])           # Assumption: if multiple files, every file has same n
    n = length(readdlm(joinpath(dirrr, (files[1] * ".csv")), ',')[2:end,1])
    D = zeros(n, n)
    costs = fill(Inf, n, n)

    for f= 1:length(files)
        # filename= joinpath(@__DIR__, (files[f] * ".csv"))
        filename= joinpath(dirrr, (files[f] * ".csv"))
        data = DelimitedFiles.readdlm(filename, ',')
        y = Float64.(data[2:end, 2])
        p = Float64.(data[2:end, 3])

        _inner(D, costs, y, p)
    end
    return D, costs, n
end

function _inner(D, costs, y, p)
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
                if costs[i,j] == Inf
                    costs[i, j] = z
                else
                    costs[i, j] += z
                end
            end
        end
    end
    return
end

##  Main.
#LDC fitting function based on prices
#= Input:
    # files: array of the names of the .csv files to be read in the same directory as this function
    # T= number of segments desired
    Output: Nothing specifically returned.
            .csv output files corresponding to each input file containing the indices that form
            the beginning of optimal load blocks + mean load in defined blocks
    Define:
    n= number of points
=#

## Orignal Function: Suitable for computing individual sets of load blocks for each input dataset
function LDC(files::Array, T::Int64)

    # Read in and initialise data + arrays needed
    print("\nTask In Progress ....")
    for f = 1:length(files)
        #filename = joinpath(@__DIR__, (files[f] * ".csv"))

        #Data pre-processing to compute optimal Load Blocks
        D, costs, n = process_file([files[f]])

        #Calling functions to compute output
        indices = BellmanFord(costs, 1, n, T)
        avgLoad = meanLoad(D, indices)
        blocklengths = Array{Float64}(undef, 1, T)
        blockDuration(blocklengths, indices)
        writeLBtoCSV(files[f], [files[f]], indices, avgLoad, blocklengths, T)
    end
    println("\rTask Completed             \n")
end

## Standardizing Function: Only returns one set of load blocks for all input datasets.
# IMPORTANT: Ensure each dataset input has same number of data points (Eg. autumn -> autumn etc.)
#            Cases such as daylight savings that cause different dataset sizes will break the code.
#  Assumes files_compute is a subset of files_fit
# (Aka the files in compute are also in fit array)

function LDC_std(files_build::Array, files_fit::Array, T::Int64)

        print("\nTask In Progress ....")

        #Data pre-processing to compute optimal Load Blocks
        D, costs, n = process_file(files_build)

        #Calling functions to compute output
        indices = BellmanFord(costs, 1, n, T)
        avgLoad= zeros(Float64, T, length(files_fit))
        for f= 1:length(files_fit)
            D1, = process_file([files_fit[f]])
            avgLoad[:,f]= meanLoad(D1, indices)
        end

        blocklengths = Array{Float64}(undef, 1, T)
        blockDuration(blocklengths, indices)

        # Added the season type to the name of the CSV output file
        writeLBtoCSV("LB_fitted_Data_"*split(files_fit[1], "_")[1], files_fit, indices, avgLoad, blocklengths, T)

        println("\rTask Completed             \n")
end

global dirrr = @__DIR__
# LDC(["netdemandddtest_Summer_2019_0"], 10)

# plotLB("LDC_netdemand_Summer_2019", "netdemand_Summer_2019", "Example Season: 10 Block LDC Approximation")
plotLB("LDC_df_net_00000_Summer", "df_net_00000_Summer", "2019 Summer: 10 Block Stepwise Approximation")
 ##
# Two options:
# First:
# Run script on all CSV files that start with "netdemand" in @__DIR__

#=    files = []
    for file in split.(glob("netdemand*.csv", @__DIR__), "\\")
        push!(files, split(file[end], ".")[1])
    end=#

# Second:
# Specify the names of files to run

#=

global dirrr = "C:\\Users\\calro\\Documents\\Academic\\JuDGE project\\LDC_important_trials\\Wind_LDC_2019\\Test"
files_build = []
files_fit = []
push!(files_build, ["Summer_Gross"])
push!(files_fit, ["Summer_Gross", "Summer_Net"])

push!(files_build,["Autumn_Gross"])
push!(files_fit, ["Autumn_Gross", "Autumn_Net"])

push!(files_build,["Winter_Gross"])
push!(files_fit, ["Winter_Gross", "Winter_Net"])

push!(files_build,["Spring_Gross"])
push!(files_fit, ["Spring_Gross", "Spring_Net"])

for i in 1:4
    @time begin
    println("FIT:    ", files_build[i])
    println("BUILD:   ", files_fit[i])
    LDC_std(files_build[i], files_fit[i], 10)
    end
end

# dir = []
#
# push!(dir, "C:\\Users\\calro\\Documents\\Academic\\JuDGE project\\LDC_important_trials\\Solar_blocksizes_2019_Ver2\\Inv_0")
# push!(dir, "C:\\Users\\calro\\Documents\\Academic\\JuDGE project\\LDC_important_trials\\Solar_blocksizes_2019_Ver2\\Inv_1000")
# push!(dir, "C:\\Users\\calro\\Documents\\Academic\\JuDGE project\\LDC_important_trials\\Solar_blocksizes_2019_Ver2\\Inv_2000")
# push!(dir, "C:\\Users\\calro\\Documents\\Academic\\JuDGE project\\LDC_important_trials\\Solar_blocksizes_2019_Ver2\\Inv_4000")
#
# files_build = []
# files_fit = []
#
# push!(files_build, ["netdemanddeductpreSort_Summer_2019"])
# push!(files_fit, ["netdemanddeductpreSort_Summer_2019", "netdemanddeductpostSort_Summer_2019", "netdemandddtest_Summer_2019", "netdemandddtest_Summer_2019_SOLAR"])
#
# push!(files_build,["netdemanddeductpreSort_Autumn_2019"])
# push!(files_fit, ["netdemanddeductpreSort_Autumn_2019", "netdemanddeductpostSort_Autumn_2019", "netdemandddtest_Autumn_2019", "netdemandddtest_Autumn_2019_SOLAR"])
#
# push!(files_build,["netdemanddeductpreSort_Winter_2019"])
# push!(files_fit, ["netdemanddeductpreSort_Winter_2019", "netdemanddeductpostSort_Winter_2019", "netdemandddtest_Winter_2019", "netdemandddtest_Winter_2019_SOLAR"])
#
# push!(files_build,["netdemanddeductpreSort_Spring_2019"])
# push!(files_fit, ["netdemanddeductpreSort_Spring_2019", "netdemanddeductpostSort_Spring_2019", "netdemandddtest_Spring_2019", "netdemandddtest_Spring_2019_SOLAR"])

# files_build = ["netdemanddeductpreSort_Summer_2019"]
# files_fit= ["netdemanddeductpreSort_Summer_2019", "netdemanddeductpostSort_Summer_2019", "netdemandddtest_Summer_2019", "netdemandddtest_Summer_2019_SOLAR"]
#
# files_build = ["netdemanddeductpreSort_Autumn_2019"]
# files_fit= ["netdemanddeductpreSort_Autumn_2019", "netdemanddeductpostSort_Autumn_2019", "netdemandddtest_Autumn_2019", "netdemandddtest_Autumn_2019_SOLAR"]
#
# files_build = ["netdemanddeductpreSort_Winter_2019"]
# files_fit= ["netdemanddeductpreSort_Winter_2019", "netdemanddeductpostSort_Winter_2019", "netdemandddtest_Winter_2019", "netdemandddtest_Winter_2019_SOLAR"]
#
# files_build = ["netdemanddeductpreSort_Spring_2019"]
# files_fit= ["netdemanddeductpreSort_Spring_2019", "netdemanddeductpostSort_Spring_2019", "netdemandddtest_Spring_2019", "netdemandddtest_Spring_2019_SOLAR"]
#
# dir = []
# push!(dir, "C:\\Users\\calro\\Documents\\Academic\\JuDGE project\\LDC_important_trials\\Solar_standardised_blocks_2019\\Summer")
# push!(dir, "C:\\Users\\calro\\Documents\\Academic\\JuDGE project\\LDC_important_trials\\Solar_standardised_blocks_2019\\Autumn")
#
# files_build = ["netdemanddeductpostSort_Summer_2019_0","netdemanddeductpostSort_Autumn_2019_0"]
# files_fit = []
#
#
#
# for j in 1:2
#     files_fit = []
#     global dirrr = dir[j]
#
#     for file in split.(glob("netdemand*.csv", dirrr), "\\")
#         push!(files_fit, split(file[end], ".")[1])
#     end
#
#     println("FIT:    ", files_build[j])
#     println("BUILD:   ", files_fit)
#     @time begin
#     LDC_std([files_build[j]], files_fit, 10)
#     end
# end




# for j in 1:2
#     global dirrr = dir[j]
#     for i in 1:2
#         @time begin
#         LDC_std(files_build[i], files_fit[i], 10)
#         end
#     end
# end



# for file in split.(glob("netdemand*.csv", dirrr), "\\")
#     push!(files_fit, split(file[end], ".")[1])
# end

# println("Files to be processed:\n", files)
# LDC(files, 10)                            #Change # of load blocks desired
# @time begin
# LDC_std(files_build, files_fit ,10)
# end

#=
TO DO: For LDC_Std, consider how leap years/different season with different n
       prevent code from working (And a roundabout for that)

       Change the LDC_Std to compute only one set of LB
       for all input datasets, then fit the LB widths to
       input data and find mean demand for each one


       => 2 arrays, files to build load blocks on, then fit on all second ones
       => 2 functions, indices passe into the next one
       -> printing into each csv
       #Delete leap years
=#
=#
