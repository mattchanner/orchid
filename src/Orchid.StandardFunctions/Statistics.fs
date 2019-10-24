namespace Orchid.Lib.Functions

open System
open Orchid.TypeSystem
open Orchid.Runtime

module public Statistics =
    
    [<Function("Array Functions", "This will return the largest value in a given range")>]
    let Max (input: double[]) = Array.max input

    [<Function("Array Functions", "This will return the smallest value in a given range")>]
    let Min (input: double[]) = Array.min input

    [<Function("Array Functions", "This will return the difference between the maximum 
    value and the minimum value for a given data range")>]
    let Range (input: double[]) = Max(input) - Min(input)

    [<Function("Aggregate Functions", "Computes the sum of the source data", true)>]
    let Sum (input: double[]) = if input.Length > 0 then Array.reduce (+) input else 0.0

    [<Function("Aggregate Functions", "Computes the size of the source data", true)>]
    let Size (input: IVariable) = input.Length        

    [<Function("Math Functions", "Determines the Median of a data set", true)>]
    let Median (input: double[]) =
        let len = input.Length
        match len with
        | 0 -> nan
        | 1 -> input.[0]
        | _ ->
            let sorted = Array.sort input
            if len % 2 = 0 then
                let midPointLeft = int ((float len / 2.0) - 1.0)
                let midPointRight = midPointLeft + 1
                let diff = sorted.[midPointRight] - sorted.[midPointLeft]
                (diff / 2.0) + sorted.[midPointLeft]
            else
                let midPointLeft = int (float len / 2.0)
                sorted.[midPointLeft]

    [<Function("Aggregate Functions", "A measure of location is usually not sufficient to describe 
    the distribution of a random variable. We therefore introduce the variance of a random variable 
    as a measure of spread of the distribution. In several widely used probability models, the mean 
    along with the variance of a random variable are adequate to describe its distribution", true)>]
    let Variance (input: seq<double>) =
        let mutable variance = 0.0
        let mutable t = 0.0
        let mutable j = 0L
        use iterator = input.GetEnumerator()
        if iterator.MoveNext() then
            j <- j + 1L
            t <- iterator.Current
            while iterator.MoveNext() do
                j <- j + 1L
                let xi = iterator.Current
                t <- t + xi
                let diff = ((float j) * xi) - t
                variance <- variance + (diff * diff) / float (j * (j - 1L))
        variance / float (j - 1L)

    [<Function("Aggregate Functions", "Calculates the biased population 
    variance estimator (on a dataset of size N will use an N normalizer).", true)>]
    let PopulationVariance (input: seq<double>) =
        let mutable variance = 0.0
        let mutable t = 0.0
        let mutable j = 0L
        use iterator = input.GetEnumerator()
        if iterator.MoveNext() then
            j <- j + 1L
            t <- iterator.Current
            while iterator.MoveNext() do
                j <- j + 1L
                let xi = iterator.Current
                t <- t + xi
                let diff = ((float j) * xi) - t
                variance <- variance + (diff * diff) / (float j * (float j - 1.0))
        variance / float j

    [<Function("Aggregate Functions", "An average is a representative figure that is used to give 
    some impression of the size of all the items in a given population. This is a measure of central 
    tendency. By this we mean that while a population may range in values, these values will be 
    distributed around a central point. This central point is therefore in some way representative of 
    the population as a whole. Averages are also known as measures of location, because they show 
    roughly where data are located on a scale of values", true)>]
    let Average (input: seq<double>) = 
        let mutable mean = 0.0
        let mutable m = 1L
        for item in input do
            mean <- mean + (item - mean) / float m
            m <- (m + 1L)
        mean

    [<Function("Aggregate Functions", "A measure of the central tendency of a data set that minimizes 
    the effects of extreme values", true)>]
    let GeoMean (input: double[]) = exp ((Sum(Array.map log input)) / float input.Length)

    [<Function("Statistics", "A type of average (measure of central tendency) which is defined as the 
    quotient of n divided by the sum of the reciprocals of all the values in a set of numerical data.", true)>]
    let HarMean (input: double[]) = 1.0 / ((1.0 / float input.Length) * Sum(Array.map (fun x -> 1.0 / x) input))

    [<Function("Aggregate Functions", "The standard deviation represents a set of numbers indicating 
    the spread, or typical distance between a single number and the set's average. A standard deviation 
    value of 15, for example, would be interpreted as: a mean of 22.5 and standard deviation of 15 means 
    that the most typical number of cases fall into a range from 7.5 to 37.5", true)>]
    let StDev (input: seq<double>) = sqrt (Variance input)

    [<Function("Aggregate Functions", "The standard deviation represents a set of numbers indicating 
    the spread, or typical distance between a single number and the set's average. A standard deviation 
    value of 15, for example, would be interpreted as: a mean of 22.5 and standard deviation of 15 means 
    that the most typical number of cases fall into a range from 7.5 to 37.5", true)>]
    let StdDev (input: seq<double>) = StDev(input)

    [<Function("Aggregate Functions", "Calculates the biased sample standard deviation (on a 
    dataset of size N will use an N normalizer).", true)>]
    let StDevP (input: seq<double>) = sqrt (PopulationVariance(input))

    [<Function("Math Functions", "Gauss error function ('http://en.wikipedia.org/wiki/Error_function')")>]
    let ErrorFunction (x: double) =
        let a = [|0.398942280444; 0.399903438504; 5.75885480458; 
                    29.8213557808; 2.62433121679; 48.6959930692; 5.92885724438|]
        let b = [|0.398942280385; 0.000000038052; 1.00000615302; 0.000398064794;
                    1.98615381364; 0.151679116635; 5.29330324926; 4.8385912808; 
                    15.1508972451; 0.742380924027; 30.789933034; 3.99019417011|]

        let mutable z = x * sqrt 2.0
        let mutable p = 0.0
        let mutable up = true
        if z < 0.0 then
            z <- -(z)
            up <- false
        if z < 7.0 || up && z < 18.66 then
            let y = z * z / 2.0
            if z > 1.28 then
                let divisor = (z - b.[1] + b.[2] / (z + b.[3] + b.[4] / (z - b.[5] + b.[6] / (z + b.[7] - b.[8] / (z + b.[9] + b.[10] / (z + b.[11]))))))
                p <- b.[0] * Math.Exp(-y) / divisor
            else
                let divisor = (y + a.[2] - a.[3] / (y + a.[4] + a.[5] / (y + a.[6])))
                p <- 0.5 - z * (a.[0] - a.[1] * y / divisor)

        if not up then p <- (1.0 - p)
        1.0 - 2.0 * p

    [<Function("Math Functions", "Returns the standard normal cumulative distribution")>]
    let NormSDist (x: double) = 0.5 + 0.5 * ErrorFunction(x / Math.Pow(2.0, 0.5))

    [<Function("Math Functions", "Median Absolute Deviation", true)>]
    let MAD (input: double[]) =
        let center = Median(input)
        input 
        |> Array.map (fun x -> abs (x - center))
        |> Median

    [<Function("Math Functions", "Clamps a value between low and high")>]
    let Clamp (x: double, low: double, high: double) = if x > high then high elif x < low then low else x

    let private beta c size =
        let correctedC = c * sqrt (1.0 - (1.0 / float size))
        let theta = 1.0 - (2.0 * (1.0 - NormSDist(correctedC)))
        let beta = theta + ((Math.Pow(correctedC, 2.0)) * (1.0 - theta)) -
                    ((2.0 * correctedC * Math.Exp(-(Math.Pow(correctedC, 2.0)) / 2.0)) / sqrt ((2.0 * System.Math.PI)))

        beta, correctedC

    [<Function("Correction Functions", "Returns a corrected array of data points based on a robust means algorithm.
    The tuning constant usually has a default value of 1.5", true)>]
    let RobustCorrection (input: double[], tuningConstant: double): double[] =
        
        let len = input.Length
        if len <= 1 then input else

        let MaxIterations = 100
        let ConvergenceLimit = 1.0
        let b, correctedC = beta tuningConstant len
        let median = Median(input)
        let initialStDev = StdDev(input)
        let mad = MAD(input)

        let mutable stdev = if mad = 0.0 then mad else mad / 0.6745
        
        let mutable firstPass = true
        let mutable stdevPrev = 0.0
        let mutable converged = false

        let cutHigh = ref (median + (correctedC * stdev))
        let cutLow = ref (median - (correctedC * stdev))
        let iterCount = ref 0

        let mutable iterArray = Array.copy input
        while (not converged) && !iterCount < MaxIterations do
            iterCount := !iterCount + 1
            iterArray <- input |> Array.map (fun x -> Clamp(x, !cutLow, !cutHigh))
            let currentAver = Average(iterArray)
            stdev <- StdDev(iterArray)
            if not firstPass then
                if stdev > 0.0 && stdevPrev > 0.0 then
                    let pctChange = ((stdevPrev / stdev ) * 100.0) - 100.0
                    if (abs pctChange) < ConvergenceLimit then
                        converged <- true
                elif stdev = 0.0 then 
                    converged <- true
            firstPass <- false
            stdevPrev <- stdev
            cutLow := currentAver - (correctedC * stdev)
            cutHigh := currentAver + (correctedC * stdev)
        if stdev > initialStDev then input else iterArray

    [<Function("Correction Functions", "Returns a corrected array of data points based on a robust 
    means algorithm. The tuning constant usually has a default value of 1.5.  
    If padReturnValue is true, the return array will be the same size as the input.  
    Knocked out, or null values in the source array will be ignored by the algorithm, 
    but will be returned in the same location as the input ", false, "RobustCorrection")>]
    let RobustCorrectionWithPadding(input: IVariable, tuningConstant: double, padReturnValue: bool) =
        if not padReturnValue then
            let dbls = VariableConverter.ConvertTo<double[]> input true
            VariableFactory.MakeVariable(RobustCorrection(dbls, tuningConstant))
        else
            let dbls, kos = input.ToDoubleArrayWithKnockOut()
            let nonEmptySlots = ResizeArray<int>()
            let collected = ResizeArray<double>()

            // Clean out ko'd \ nan points, and record the position of the valid values
            for i in 0 .. (dbls.Length - 1) do
                if not (Double.IsNaN(dbls.[i])) && kos.[i] = 0 then
                    collected.Add(dbls.[i])
                    nonEmptySlots.Add(i)

            // Perform the correction on the cleaned data
            let result = RobustCorrection(collected.ToArray(), tuningConstant)

            // The result should be the same size as the non empty slots
            assert (result.Length = nonEmptySlots.Count)

            // Create an array of the recorded slots, zip them up with the data
            // and reassign these values back to the source array.
            // Note that the source array can be modified here as it is a copy of the
            // data provided by the input variable, and will not modify the underlying type
            let slotArr = nonEmptySlots.ToArray()
            result 
            |> Array.zip slotArr 
            |> Array.iter (fun (idx, value) -> dbls.[idx] <- value)
            VariableFactory.MakeVariable(dbls, kos)

    [<Function("Correction Functions", "Returns the standard deviation for a data set using robust correction")>]
    let RobustStdDev (input: double[], tuningConstant: double) =
        let initialStdev = StDev(input)
        let correctedData = RobustCorrection(input, tuningConstant)
        let betaRoot, _ = beta tuningConstant (input.Length)
        let robustStdev = StDev(correctedData) / betaRoot
        if robustStdev > initialStdev then initialStdev else robustStdev

    [<Function("Statistics", "Standard Deviation of Average", true)>]
    let AveStdDev (input: double[]) =
        let ave = Average(input)
        let var = Sum(Array.map (fun x -> (x - ave) ** 2.0) input) / (float input.Length - 1.0)
        (var / float input.Length) ** 0.5