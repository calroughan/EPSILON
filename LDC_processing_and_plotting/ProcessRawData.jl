using Glob, CSV, DataFrames, Dates

# Change this to the directory which stores EMI and Cliflo data CSVs
# This does not have to be stored in the same location as this script
dir = "C:\\Users\\calro\\Documents\\Academic\\JuDGE project\\LDC_New"
# dir = "C:\\Users\\calro\\Documents\\Academic\\JuDGE project\\LDC_Jamie\\Solar\\Raw"

validation = true   # Run the validation script
only2019data = true     # Multiple years of data (Rep days) or just 2019 (LDCs)

function windfarm_dict()
    # Dictionary of wind farm gen_codes => actual names
    # and the existing capacity at each wind farm
    # Note that the MW amounts are EXISTING capacity, not investments.

    # 1. Names
    # keys: Gen_Code, vals: Location

    # 2. Capcity
    # keys: Location, vals: capacity

    Gen_Code = ["te_apiti"
    , "te_rere_hau"
    , "te_uku"
    , "twf_12"
    , "twf_3"
    , "west_wind"
    , "white_hill"]

    Location = ["TeApiti"
    , "TeRereHau"
    , "TeUku"
    , "Tararua_Embed"
    , "Tararua"
    , "Westwind"
    , "Whitehill"]

    Capacity = [90.75
    , 48.5
    , 0.0
    , 0.0
    , 93.0
    , 143.0
    , 0.0]

    # Gen_Code = ["te_apiti"
    # , "te_rere_hau"
    # , "te_uku"
    # , "twf_12"
    # , "twf_3"
    # , "west_wind"
    # , "white_hill"]
    #
    # Location = ["TeApiti"
    # , "TeRereHau"
    # , "TeUku"
    # , "Tararua"
    # , "Tararua"
    # , "Westwind"
    # , "Whitehill"]
    #
    # Capacity = [90.75
    # , 48.5
    # , 64.4
    # , 161.0
    # , 143
    # , 58]

    # return Dict(zip(Gen_Code, Location)), Dict(zip(unique(Location), Capacity))
    return Dict(zip(Gen_Code, Location)), Dict(zip(Location, Capacity))
end

function solar_dict()
    # Dictionary of solar cells that we have collected data for.
    # Note that the MW amounts are EXISTING capacity, not investments.

    Station = ["Pukekohe Ews"
    , "Lauder Ews"
    , "Blenheim Research Ews"
    , "Christchurch Aero"]

    Region = ["Auckland"
    , "Otago"
    , "Blenheim"
    , "Christchurch"]

    Island = ["North"
    , "South"
    , "South"
    , "South"]

    # North = 77.72 MW
    # South = 36.68 MW
    # MW = [77.72
    # , 36.68]

    MW = [0.0
    , 0.0]

    # First returns a dictionary of station name and island
    # Second returns the MW production in each island
    return Dict(zip(Station, Island)), Dict(zip(unique(Island), MW))
end

function investments()
    # Input an array of all individual investments, this script will then
    # iterate over the power set of all investments.

    # This is a tad tricky. I would recommend initially running all functions up
    # to and including "Merge()" to see what the column names are for each gen
    # type.
    # CURRENTLY:
        # The WIND sites can be referred to by their LOCATION in
        # windfarm_dict(),
        # SOLAR sites are to be referred to as: "Solar_NI" and "Solar_SI" for
        # the amount in either of these islands. This will probably change

    # Probably worth adding some assert statements that validate that the user
    # input is correct, but for now, you're on your own.

    Investment_Type = ["Wind"
                        , "Wind"
                        , "Wind"
                        , "Wind"
                        , "Wind"
                        ]

    Investment_Location = ["TeApiti"
                            , "Westwind"
                            , "TeUku"
                            , "Tararua"
                            , "Whitehill"
                            ]

    Investment_Amount = [600.0
                        , 700.0
                        , 800.0
                        , 900.0
                        , 1000.0
                        ]

    return Dict(zip(Investment_Location, Investment_Type)),
         Dict(zip(Investment_Location, Investment_Amount))
end

function validate_capacity()
    # This function prints out the current existing capacity of windsites,
    # solarsites, and any proposed combination of investments.
    WindLocs, Windcap = windfarm_dict()
    SolarLocs, Solarcap = solar_dict()
    InvLocs, Invs = investments()

    cnt = length(InvLocs)

    println("\n\nCurrent windfarm capacity:")
    println(stack(DataFrame(Windcap)))
    println("\n\nCurrent solarfarm capacity:")
    println(stack(DataFrame(Solarcap)))

    # Convert investment df into a more readable form
    Invs = stack(DataFrame(Invs))
    rename!(Invs, Dict(:variable => "Investment_Location", :value => "Investment_Amount"))
    Invs[!, :Generation_Type] .= ""
    InvLocs = DataFrame(InvLocs)
    for i in names(InvLocs)
        Invs[Invs.Investment_Location .== i, :Generation_Type] .= InvLocs[1, i]
    end

    println("\n\nAll investments (individual):")
    println(Invs)

    println("\n\nIndividual investments: ", cnt, ".")
    println("Total portfolio size: ", 2^cnt, " investment options.")
end

function powerset()

    println("\n\nGenerating portfolio of investments ...")
    InvLocs, _ = investments()
    length(InvLocs)
    bin_rep = []

    #Power set of all investments
    for i in 0:2^length(InvLocs) - 1
     # Calculate the binary representation for the 'i'th combination of
     # investments. Pad the MSBs with zeros for binary representations that
     # are shorter than the number of investments
     push!(bin_rep, string(i, base = 2, pad = length(InvLocs)))
    end
    return bin_rep
end

function Format_TP_Demand(dir)
    println("\n\nFormatting demand data ...")
    # Read in raw demand data
    demand = reduce(vcat, CSV.read.(glob("*_import.csv", dir)))
    # Reshape the demand df such that each trading period becomes a row
    select!(demand, Not([:POC, :NWK_Code, :GENERATION_TYPE, :TRADER, :UNIT_MEASURE, :FLOW_DIRECTION, :STATUS]))
    # This is the new version of "melt" : Produces the same output as: demand = melt(demand, [:TRADING_DATE])
    demand = stack(demand, names(demand)[2:end])
    # Sum all TP on the same day, convert load from kWh to MW
    demand = combine(groupby(demand, [:TRADING_DATE, :variable]), :value => sum)
    demand[!, :value_sum] = demand[!, :value_sum] * 2 / 1000
    # Create a new column and input the numerical value of which trading period it is
    demand[!, :TP] .= parse.(Int64, reduce(hcat, split.(String.(demand[!, :variable]), "P"))[2, :])
    # Drop unnecessary columns and reorder the remaining columns
    # select!(demand, Not(:variable))
    select!(demand, [:TRADING_DATE, :TP, :value_sum])
    rename!(demand,  Dict(:TRADING_DATE => "Trading_date", :TP => "Trading_period", :value_sum => "Load"))
    # Drop TP's > 48 (Daylight savings)
    delete!(demand, demand[!, :Trading_period] .> 48)
    # Parse the trading date and add seasons
    demand[!, :Trading_date] = Date.(demand[!, :Trading_date],"d/m/y")
    seasons = ["Summer", "Autumn", "Winter", "Spring", "Summer"]
    insertcols!(demand, 4, :Season => seasons[convert.(Int64,floor.(Dates.month.(demand[!, :Trading_date]) / 3) .+ 1)])
    # Sort chronologically and return
    sort!(demand, [:Trading_date, :Trading_period], rev = false)
    demand[!, :Load_raw] = demand[!, :Load]                         # Store the gross load
    # CSV.write(joinpath(@__DIR__, "demanddata.csv"),  DataFrame(demand), header=true)
    return demand
end

function Format_TP_Price(dir)
    println("Formatting price data ...")
    # Read in price data
    price = reduce(vcat, CSV.read.(glob("*_prices.csv", dir)))
    # Prices at PEN0221
    price = price[price[!, :Node] .== "PEN0221",:]
    # Convert from KWh to MW - Not sure about this
    # price[!, :Price] = price[!, :Price] * 2 / 1000
    # Sort chronologically and return
    select!(price, Not(:Node))
    sort!(price, [:Trading_date, :Trading_period], rev = false)
    # CSV.write(joinpath(@__DIR__, "pricedata.csv"),  DataFrame(price), header=true)
    return price
end

function Format_TP_Wind(dir)
    println("Formatting wind data ...")
    # Extract wind wind data
    wind = reduce(vcat, CSV.read.(glob("*Generation_MD.csv", dir)))
    wind = wind[wind[!, :Tech_Code] .== "Wind" ,:]
    select!(wind, Not([:Site_Code, :Nwk_Code, :Fuel_Code, :Tech_Code]))
    # Melt data
    wind = stack(wind, names(wind)[4:end])
    rename!(wind, :value => :Load)
    # Extract the number from TP and assign to Trading_period and drop TP's > 48
    wind[!, :Trading_period] .= parse.(Int64, reduce(hcat, split.(String.(wind[!, :variable]), "P"))[2, :])
    delete!(wind, wind[!, :Trading_period] .> 48)
    # Parse the trading date and add seasons
    # Only run the commented line below if Trading_date is not parsed already
    # wind[!, :Trading_date] = Date.(wind[!, :Trading_date],"d/m/y")
    seasons = ["Summer", "Autumn", "Winter", "Spring", "Summer"]
    insertcols!(wind, 7, :Season => seasons[convert.(Int64,floor.(Dates.month.(wind[!, :Trading_date]) / 3) .+ 1)])
    # Change names of locations
    locationdict, _ = windfarm_dict()
    wind[:, :Location] .= ""
    [wind[i, :Location] = locationdict[wind[i, :Gen_Code]] for i in 1:size(wind)[1]]
    # Sum over all TP on the same day, convert load from kWh to MW
    select!(wind, [:Trading_period, :Trading_date, :Season, :Load, :Location])
    dropmissing!(wind)
    wind = combine(groupby(wind, [:Trading_date, :Trading_period, :Season, :Location]), :Load => sum)
    wind[!, :Load_sum] = wind[!, :Load_sum] * 2 / 1000
    # Transform rows back to wide format and remove the two empty daylight
    # savings rows in September. If more than two TPs disappear, check what's up
    wind = unstack(wind, [:Trading_period, :Trading_date, :Season], :Location, :Load_sum)
    dropmissing!(wind)
    # Divide each element by the max of each column to get the wind scale factors
    [wind[!, i+3] = wind[!, i+3] / maximum(wind[!, i+3]) for i in 1:7]
    sort!(wind, [:Trading_date, :Trading_period], rev = false)
    # CSV.write(joinpath(@__DIR__, "windfactordata.csv"),  DataFrame(wind), header=true)
    return wind
end

function Format_TP_Solar(dir)
    println("Formatting solar data ...")
    # Read in solar data
    solar = reduce(vcat, CSV.read.(glob("NIWA_Solar_*.csv", dir)))
    rename!(solar, ["aa", "Station", "date", "Irradiance", "bb", "cc", "dd"])
    # Extract date and hour
    solar[!, :Trading_date] = Date.(solar[!, :date],"yyyymmdd:HM")
    transform!(solar, :date => x -> chop.(x, head=9, tail=0))
    select!(solar, [:Station, :Trading_date, :date_function, :Irradiance])
    rename!(solar, :date_function => :Hour)
    # Format the trading_periods (hourly data)
    solar[!, :Hour] .= parse.(Int64, string.(solar[!, :Hour]))
    solar[!, :Hour] .= convert.(Int64, floor.(solar[!, :Hour] / 100))
    # Format island column by calling the dictionary
    locationdict, _ = solar_dict()
    solar[:, :Island] .= ""
    [solar[i, :Island] = locationdict[solar[i, :Station]] for i in 1:size(solar)[1]]
    # Calculate the W produced from the MJ/(hr m^2)
    # # # Commenting this out for the moment seeings as we are working with
    # "solar-factors" anyway?
    # solar[!, :Irradiance] .= solar[!, :Irradiance].* 1000 .*area.*efficiency./3600
    # Halve it for the half hourly data
    solar[!, :Irradiance] .= solar[!, :Irradiance] ./ 2
    solar2 = DataFrame(Station = String[], Trading_date = Date[], Hour = Float64[], Irradiance = Float64[], Island = String[])
    # Duplicate each row such that
    for i in 1:size(solar)[1]
        push!(solar2, solar[i, :])  # Push the original, hourly data
        push!(solar2, solar[i, :])   # Add an additional row to account for the half hour TP
        solar2[end, :Hour] = solar2[end, :Hour] + 0.5
    end
    # Assign the trading_period labels
    solar2[!, :Hour] .= convert.(Int64, 2 .* (solar2[!, :Hour] .+ 0.5))
    rename!(solar2, :Hour => :Trading_period)
    # Melt the data back from long to wide format
    unique!(solar2)
    solar = unstack(solar2, [:Trading_date, :Trading_period], :Island, :Irradiance)
    solar[!, :Solar] = solar[!, :North] + solar[!, :South]
    select!(solar, [:Trading_date, :Trading_period, :Solar])
    dropmissing!(solar)
    # 2019 data
    delete!(solar, Dates.year.(solar[!, :Trading_date]) .<2019)
    delete!(solar,  ((Dates.year.(solar[!, :Trading_date]) .<=2019) .* (Dates.month.(solar[!, :Trading_date]) .<=2)))
    # Divide each element by the max of each column to get the solar scale factors
    # [solar[!, i+2] = round.(solar[!, i+2] / maximum(solar[!, i+2]); digits = 6) for i in 1:2]
    solar[!, :Solar] = solar[!, :Solar] ./ maximum(solar[!, :Solar])
    # CSV.write(joinpath(@__DIR__, "CSV_files", "solardata.csv"),  DataFrame(solar), header=true)
    return solar
end

function Merge(demand, price, wind, solar, file)
    println("Merging raw data ...")
    # Merge price, demand, wind, and solar data
    merged = innerjoin(demand, price, on = [:Trading_date, :Trading_period])
    merged = innerjoin(merged, wind, on = [:Trading_date, :Trading_period, :Season])
    merged = innerjoin(merged, solar, on = [:Trading_date, :Trading_period])
    # rename!(merged, [:North => "Solar_NI", :South => "Solar_SI"])
    # Return the df chronologically sorted
    # select!(merged, [:Trading_period, :Load, :Price, :Trading_date, :Season])
    sort!(merged, [:Trading_date, :Trading_period], rev = false)
    CSV.write(joinpath(@__DIR__, file),  DataFrame(merged), header=true)
    println("Raw data exported to CSV: ", file)
    return merged
end

function Capacity_Processing(filename, portfolio)
    println("\n\nSubtracting preexisting generation ...")
    _, Windcap = windfarm_dict()
    # _, Solarcap = solar_dict()
    # existingcap = hcat(DataFrame(Windcap), DataFrame(Solarcap))
    # Call a function to subtract existing generation
    # df = Capacity_Subtraction(filename[1], existingcap)
    df = Capacity_Subtraction(filename[1], DataFrame(Windcap))
    # Store this as a CSV too
    CSV.write(joinpath(@__DIR__, filename[2]),  DataFrame(df), header=true)
    # Prepare investment
    _, Invs = investments()
    # Iterate over portfolio
    println("Subtracting investment portfolio ...")
    for set in portfolio
        invscap = deepcopy(DataFrame(Invs))
        for (j, inv) in enumerate(set)
            invscap[1, j] = invscap[1, j] * parse(Int, inv, base = 10)
        end
        # Call a function to subtract what is required
        netdf = Capacity_Subtraction(filename[2], invscap)
        # Save the CSV based on the binary representation in the name
        # Note that this saves into the Portfolio_Files dir
        CSV.write(joinpath(dirname(@__DIR__), "Portfolio_Files", filename[3]*set*".csv"),  DataFrame(netdf), header=true)
    end
    println("Files are stored in: ", joinpath(dirname(@__DIR__), "Portfolio_Files"))
end

function Capacity_Subtraction(file, capacity)
    # Read in the dataframe from a csv
    df = CSV.read(joinpath(@__DIR__, file), DataFrame)
    df[!, :Load] = collect(skipmissing(df[!, :Load]))
    # Iterate over all locs in capacity and subtract each's generation from Load
    for loc in names(capacity)
        df[!, :Load] = df[!, :Load] .- (df[!, loc] .* capacity[1, loc])
    end

    # Any negative values are just set to 0
    df[df[!, :Load] .< 0, :Load] .= 0

    return df
end

function Portfolio_CSV(filename)
    println("Dividing each portfolio into seasons ...")

    dir = joinpath(dirname(@__DIR__), "Portfolio_Files")

    # Extract a list of all files in the Portfolio database
    files = []
    for f in split.(glob(filename[3]*"*", dir), "\\")
        push!(files, split(f[end], ".")[1])
    end

    for file in files
        df = CSV.read(joinpath(dir, file*".csv"))
        select!(df, [:Trading_period, :Load, :Price, :Trading_date, :Season, :Solar, :Load_raw])

        # Create a column YearSeasonStarts, such that we can correctly assign
        # the summer season which crosses years
        df[!, :YearSeasonStarts] .= Dates.year.(df[!, :Trading_date])
        df[Int64.(Dates.month.(df[!, :Trading_date])) .<= 2, :YearSeasonStarts] .-= 1
        df[!, :YearSeasonStarts] .= string.(df[!, :YearSeasonStarts])

        for seas in unique(df[!, :Season])

            output = deepcopy(df[df[!, :Season] .== seas, :])
            output = output[output[!, :YearSeasonStarts] .== "2019", :]

            sort!(output, [:Load], rev = true)
            select!(output, [:Trading_period, :Load, :Price, :Trading_date, :Season, :Solar, :Load_raw])
            if size(output)[1] > 0
                CSV.write(joinpath(dirname(@__DIR__), "LDC_CSV_Files"
                        , file * "_" * seas * ".csv")
                        , DataFrame(output), header=true)
            end
        end
    end
    println("CSVs stored in: ", joinpath(dirname(@__DIR__), "LDC_CSV_Files"))
end

function solar_dict_Adapted_For_Jamie()
    # Dictionary of solar cells that we have collected data for.
    # Note that the MW amounts are EXISTING capacity, not investments.

    Station = ["Pukekohe Ews"
    , "Baring Head"
    , "Lauder Ews"
    , "Blenheim Research Ews"
    , "Lincoln, Broadfield Ews"]

    Region = ["Auckland"
    , "Wellington"
    , "Queenstown"
    , "Blenheim"
    , "Christchurch"]

    # First returns a dictionary of station name and region
    return Dict(zip(Station, Region))
end

function Portfolio_CSV_Demand_Adapted_For_Jamie(filename)
    println("Dividing each portfolio into seasons ...")

    # dir = joinpath(dirname(@__DIR__), "Portfolio_Files")
    dir = joinpath(dirname(@__DIR__), "temp")

    file = "demanddata"
    df = CSV.read(joinpath(dir, file*".csv"))
    # select!(df, [:Trading_period, :Load, :Price, :Trading_date, :Season])
    select!(df, [:Trading_period, :Load, :Trading_date, :Season])

    # Create a column YearSeasonStarts, such that we can correctly assign
    # the summer season which crosses years
    df[!, :YearSeasonStarts] .= Dates.year.(df[!, :Trading_date])
    df[Int64.(Dates.month.(df[!, :Trading_date])) .<= 2, :YearSeasonStarts] .-= 1
    df[!, :YearSeasonStarts] .= string.(df[!, :YearSeasonStarts])

    for yrs in unique(df[!, :YearSeasonStarts])
        for seas in unique(df[!, :Season])

            output = deepcopy(df[df[!, :YearSeasonStarts] .== yrs, :])
            output = deepcopy(output[output[!, :Season] .== seas, :])

            sort!(demand, [:Trading_date, :Trading_period], rev = false)
            select!(output, [:Trading_period, :Load, :Trading_date, :Season])
            if size(output)[1] > 0
                CSV.write(joinpath(dirname(@__DIR__), "temp"
                        , "Demand_" * seas * "_" * yrs * ".csv")
                        , DataFrame(output), header=true)
            end
        end
    end
    println("CSVs stored in: ", joinpath(dirname(@__DIR__), "LDC_CSV_Files"))
end

function Portfolio_CSV_Solar_Adapted_For_Jamie(filename)
    println("Dividing each portfolio into seasons ...")

    # dir = joinpath(dirname(@__DIR__), "Portfolio_Files")
    dir = joinpath(dirname(@__DIR__), "temp")

    file = "solardata"
    df = CSV.read(joinpath(dir, file*".csv"))
    # select!(df, [:Trading_period, :Load, :Price, :Trading_date, :Season])
    select!(df, [:Trading_period, :Irradiance, :Trading_date, :Region])

    seasons = ["Summer", "Autumn", "Winter", "Spring", "Summer"]
    insertcols!(df, 4, :Season => seasons[convert.(Int64,floor.(Dates.month.(df[!, :Trading_date]) / 3) .+ 1)])

    # Create a column YearSeasonStarts, such that we can correctly assign
    # the summer season which crosses years
    df[!, :YearSeasonStarts] .= Dates.year.(df[!, :Trading_date])
    df[Int64.(Dates.month.(df[!, :Trading_date])) .<= 2, :YearSeasonStarts] .-= 1
    df[!, :YearSeasonStarts] .= string.(df[!, :YearSeasonStarts])

    for site in unique(df[!, :Region])
        for seas in unique(df[!, :Season])

            output = deepcopy(df[df[!, :Season] .== seas, :])
            output = deepcopy(output[output[!, :Region] .== site, :])

            sort!(output, [:Trading_date, :Trading_period], rev = false)
            select!(output, [:Trading_period, :Irradiance, :Trading_date, :Season, :Region])
            if size(output)[1] > 0
                CSV.write(joinpath(dirname(@__DIR__), "temp"
                        , "Solar_" * seas * "_" * site * ".csv")
                        , DataFrame(output), header=true)
            end
        end
    end
    println("CSVs stored in: ", joinpath(dirname(@__DIR__), "temp"))
end

function Format_TP_Solar_Adapted_For_Jamie(dir)
    println("Formatting solar data ...")
    # Read in solar data
    solar = reduce(vcat, CSV.read.(glob("NIWA_Solar_*.csv", dir)))
    rename!(solar, ["Station", "date", "Irradiance", "bb", "cc", "dd"])
    # Extract date and hour
    solar[!, :Trading_date] = Date.(solar[!, :date],"yyyymmdd:HM")
    transform!(solar, :date => x -> chop.(x, head=9, tail=0))
    select!(solar, [:Station, :Trading_date, :date_function, :Irradiance])
    rename!(solar, :date_function => :Hour)
    # Format the trading_periods (hourly data)
    solar[!, :Hour] .= parse.(Int64, string.(solar[!, :Hour]))
    solar[!, :Hour] .= convert.(Int64, floor.(solar[!, :Hour] / 100))
    # Format island column by calling the dictionary
    locationdict = solar_dict_Adapted_For_Jamie()
    solar[:, :Region] .= ""
    [solar[i, :Region] = locationdict[solar[i, :Station]] for i in 1:size(solar)[1]]
    # Calculate the W produced from the MJ/(hr m^2)
    # # # Commenting this out for the moment seeings as we are working with
    # "solar-factors" anyway?
    # solar[!, :Irradiance] .= solar[!, :Irradiance].* 1000 .*area.*efficiency./3600
    # Halve it for the half hourly data
    solar[!, :Irradiance] .= solar[!, :Irradiance] ./ 2
    solar2 = DataFrame(Station = String[], Trading_date = Date[], Hour = Float64[], Irradiance = Float64[], Region = String[])
    # Duplicate each row such that
    for i in 1:size(solar)[1]
        push!(solar2, solar[i, :])  # Push the original, hourly data
        push!(solar2, solar[i, :])   # Add an additional row to account for the half hour TP
        solar2[end, :Hour] = solar2[end, :Hour] + 0.5
    end
    # Assign the trading_period labels
    solar2[!, :Hour] .= convert.(Int64, 2 .* (solar2[!, :Hour] .+ 0.5))
    rename!(solar2, :Hour => :Trading_period)
    # Melt the data back from long to wide format
    unique!(solar2)

    temp = unstack(solar2, [:Station, :Trading_date, :Region], :Trading_period, :Irradiance)
    dropmissing!(temp)
    solar3 = stack(temp, names(temp)[4:end])
    rename!(solar3, Dict(:variable => "Trading_period", :value => "Irradiance"))



    # solar = unstack(solar2, [:Trading_date, :Trading_period], :Region, :Irradiance)
    # dropmissing!(solar)
    # # Divide each element by the max of each column to get the wind scale factors
    # [solar[!, i+2] = round.(solar[!, i+2] / maximum(solar[!, i+2]); digits = 6) for i in 1:2]
    CSV.write(joinpath(@__DIR__, "solardata.csv"),  DataFrame(solar3), header=true)
    return solar2
end

# Raw data processing
@time begin
println("\n\nProcessing ...") # Doesn't always print this line. Def a Julia bug.

filename = ["df_Raw.csv", "df_Gen_Sub.csv", "df_net_"]
demand   = Format_TP_Demand(dir)
price    = Format_TP_Price(dir)
wind     = Format_TP_Wind(dir)
solar    = Format_TP_Solar(dir)
merged   = Merge(demand, price, wind, solar, filename[1])

# Validate the set of investments and existing capacities
if validation
    validate_capacity()
end

# Power set of all investments
portfolio = powerset()
Capacity_Processing(filename, portfolio)
Portfolio_CSV(filename)
println("\nFinished ...\n")
end
