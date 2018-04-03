// Measurement data with capacitors bank and 10 nF in parallel
module MeasurementDataWith10nF =

    let Data = [
        (0, 14.22)
        (1, 13.82)
        (2, 13.7)
        (3, 13.33)
        (4, 13.57)
        (5, 13.22)
        (6, 13.1)
        (7, 12.82)
        (8, 12.38)
        (9, 12.14)
        (10, 12.02)
        (11, 11.79)
        (12, 11.9)
        (13, 11.68)
        (14, 11.57)
        (15, 11.36)
        (16, 11.79)
        (17, 11.57)
        (18, 11.47)
        (19, 11.26)
        (20, 11.36)
        (21, 11.16)
        (22, 11.11)
        (23, 10.92)
        (24, 10.68)
        (25, 10.5)
        (26, 10.42)
        (27, 10.26)
        (28, 10.36)
        (29, 10.2)
        (30, 10.1)
        (31, 10.0)
    ]

// Measurement data with capacitors bank and without 10 nF in parallel
module MeasurementDataWithout10nF =

    let Data = [
        (0, 131.58)
        (1, 54.82)
        (2, 46.68)
        (3, 36.92)
        (4, 42.92)
        (5, 34.88)
        (6, 32.47)
        (7, 28.54)
        (8, 24.51)
        (9, 22.73)
        (10, 21.93)
        (11, 20.60)
        (12, 21.55)
        (13, 20.27)
        (14, 19.74)
        (15, 18.75)
        (16, 20.60)
        (17, 19.53)
        (18, 19.04)
        (19, 18.2)
        (20, 18.75)
        (21, 17.94)
        (22, 17.61)
        (23, 16.82)
        (24, 15.89)
        (25, 15.34)
        (26, 15.15)
        (27, 14.71)
        (28, 14.97)
        (29, 14.62)
        (30, 14.37)
        (31, 13.97)
    ]

// Data of the capacitors bank
module CapacityBank =

    let private nanoFactor = 1.0E-9
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
        let resultC = nanoFactor * totalC
        resultC

let main argv =
    printfn "%A" argv
    0
