
// Helper constants
module Factor =
    let kilo = 1.0E3
    let nano = 1.0E-9

// Measurement data with capacitors bank and 10 nF in parallel
module MeasurementDataWith10nF =

    let private getF s =
        match s with
        | 0  -> 14.22
        | 1  -> 13.82
        | 2  -> 13.7
        | 3  -> 13.33
        | 4  -> 13.57
        | 5  -> 13.22
        | 6  -> 13.1
        | 7  -> 12.82
        | 8  -> 12.38
        | 9  -> 12.14
        | 10 -> 12.02
        | 11 -> 11.79
        | 12 -> 11.9
        | 13 -> 11.68
        | 14 -> 11.57
        | 15 -> 11.36
        | 16 -> 11.79
        | 17 -> 11.57
        | 18 -> 11.47
        | 19 -> 11.26
        | 20 -> 11.36
        | 21 -> 11.16
        | 22 -> 11.11
        | 23 -> 10.92
        | 24 -> 10.68
        | 25 -> 10.5
        | 26 -> 10.42
        | 27 -> 10.26
        | 28 -> 10.36
        | 29 -> 10.2
        | 30 -> 10.1
        | 31 -> 10.0
        | _  -> 0.0

    let getFrequency s =
        let f = getF s
        Factor.kilo * f

// Measurement data with capacitors bank and without 10 nF in parallel
module MeasurementDataWithout10nF =

    let private getF s =
        match s with
        | 0  -> 131.58
        | 1  -> 54.82
        | 2  -> 46.68
        | 3  -> 36.92
        | 4  -> 42.92
        | 5  -> 34.88
        | 6  -> 32.47
        | 7  -> 28.54
        | 8  -> 24.51
        | 9  -> 22.73
        | 10 -> 21.93
        | 11 -> 20.60
        | 12 -> 21.55
        | 13 -> 20.27
        | 14 -> 19.74
        | 15 -> 18.75
        | 16 -> 20.60
        | 17 -> 19.53
        | 18 -> 19.04
        | 19 -> 18.2
        | 20 -> 18.75
        | 21 -> 17.94
        | 22 -> 17.61
        | 23 -> 16.82
        | 24 -> 15.89
        | 25 -> 15.34
        | 26 -> 15.15
        | 27 -> 14.71
        | 28 -> 14.97
        | 29 -> 14.62
        | 30 -> 14.37
        | 31 -> 13.97
        | _  -> 0.0

    let getFrequency s =
        let f = getF s
        Factor.kilo * f

// Data of the capacitors bank
module CapacityBank =

    let private C0 = 0.56
    let private C1 = 0.82
    let private C2 = 1.0
    let private C3 = 3.3
    let private C4 = 4.7

    let private getSwitchState x =
        match x with
        | 0  -> "00000"
        | 1  -> "00001"
        | 2  -> "00010"
        | 3  -> "00011"
        | 4  -> "00100"
        | 5  -> "00101"
        | 6  -> "00110"
        | 7  -> "00111"
        | 8  -> "01000"
        | 9  -> "01001"
        | 10 -> "01010"
        | 11 -> "01011"
        | 12 -> "01100"
        | 13 -> "01101"
        | 14 -> "01110"
        | 15 -> "01111"
        | 16 -> "10000"
        | 17 -> "10001"
        | 18 -> "10010"
        | 19 -> "10011"
        | 20 -> "10100"
        | 21 -> "10101"
        | 22 -> "10110"
        | 23 -> "10111"
        | 24 -> "11000"
        | 25 -> "11001"
        | 26 -> "11010"
        | 27 -> "11011"
        | 28 -> "11100"
        | 29 -> "11101"
        | 30 -> "11110"
        | 31 -> "11111"
        | _  -> "00000"

    let private getC s C =
        match s with
        | "0" -> 0.0
        | "1" -> C
        | _   -> 0.0

    let computeC x =
        let state = getSwitchState x
        let s4 = state.[0..0]
        let s3 = state.[1..1]
        let s2 = state.[2..2]
        let s1 = state.[3..3]
        let s0 = state.[4..4]
        let totalC = getC s0 C0 + getC s1 C1 + getC s2 C2 + getC s3 C3 + getC s4 C4
        Factor.nano * totalC

// Functions related to L computation
module L =
    // Compute L using resonance frequency f and capacity C
    let compute f C =
        let w = 2.0 * System.Math.PI * f
        let w2 = w * w
        1.0 / (w2 * C)

// Measurement data of ferrite antenna 1
module Antenna1 =
    let private C1 = 3.3E-9
    let private f1 = 37.19E3
    let L1 = L.compute f1 C1

    let private C2 = 4.7E-9
    let private f2 = 31.38E3
    let L2 = L.compute f2 C2

    let f_own = 216E3

// Measurement data of ferrite antenna 2
module Antenna2 =
    let private C1 = 3.3E-9
    let private f1 = 40.98E3
    let L1 = L.compute f1 C1

    let private C2 = 4.7E-9
    let private f2 = 34.56E3
    let L2 = L.compute f2 C2

    let f_own = 239E3

// Measurement data of ferrite antenna 3
module Antenna3 =
    let private C1 = 3.3E-9
    let private f1 = 52.97E3
    let L1 = L.compute f1 C1

    let private C2 = 4.7E-9
    let private f2 = 44.64E3
    let L2 = L.compute f2 C2

    let f_own = 309E3

printfn "Antenne 1: L1 = %f; L2 = %f" Antenna1.L1 Antenna1.L2
printfn "Antenne 2: L1 = %f; L2 = %f" Antenna2.L1 Antenna2.L2
printfn "Antenne 3: L1 = %f; L2 = %f" Antenna3.L1 Antenna3.L2

// Capactor bank switchs
let capSwitches = [| 0 .. 31 |]

printfn "Measurement result with capacitor bank without 10 nF"
for s in capSwitches do
    let f = MeasurementDataWithout10nF.getFrequency s
    let C = CapacityBank.computeC s
    let L = L.compute f C
    printfn "%d: %f" s L

printfn "Measurement result with capacitor bank with 10 nF"
for s in capSwitches do
    let f = MeasurementDataWith10nF.getFrequency s
    let C = 10E-9 + CapacityBank.computeC s
    let L = L.compute f C
    printfn "%d: %f" s L