include "../../parser/split.dfy"
include "../../parser/parseInt.dfy"
include "../../libraries/src/dafny/Collections/Seqs.dfy"
module Problem1 {
    import opened Split
    import opened ParseInt
    import opened Dafny.Collections.Seq

    method print_seq<T>(xs: seq<T>)
    {
        var i := 0;
        while i < |xs|
        {
            print xs[i];
            i := i + 1;
        }
    }


    method parseInput(input: string) returns (result: seq<int>) 
    {
        // Filters spaces after splitting the input on breaks
        var lines := Filter(line => line != "", splitOnBreak(input));

        // Map filtering all the non-numbers
        var numsStr := Map(line => Filter(charInNC, line), lines);

        // Make the two-digit number in strings
        var trunk := Map(line => 
            assume {:axiom} |line| > 2;
            [line[0],line[|line|-1]], numsStr);

        // Convert strings to integers
        result := Map(Integer, trunk);

        return result;   
    }

    // TODO - figure if we can split this to do it using maps or folds
    function parseLine(result: seq<char>, input:  string) :  seq<char> 
        decreases input
    {
        if |input| == 0 then result else
            if "one" <= input then 
                parseLine(result+['1'],input[1..])
            else if "two" <= input then
                parseLine(result+['2'], input[1..])
            else if "three" <= input then
                parseLine(result+['3'],input[1..])
            else if "four" <= input then
                parseLine(result+['4'],input[1..])
            else if "five" <= input then
                parseLine(result+['5'],input[1..])
            else if "six" <= input then
                parseLine(result+['6'],input[1..])
            else if "seven" <= input then
                parseLine(result+['7'],input[1..])
            else if "eight" <= input then
                parseLine(result+['8'],input[1..])
            else if "nine" <= input then
                parseLine(result+['9'],input[1..])
            else if charInNC(input[0]) then
                parseLine(result+[input[0]],input[1..])
            else 
                parseLine(result,input[1..])
    }

    method parseInput2(input: string) returns (result: seq<int>)
    {
        // Split the lines 
        var lines := Filter(line => line != "", splitOnBreak(input));

        // parse each token and convert if its possible
        var line := Map(line => parseLine([], line),lines);
        
        // Make the 2 digit number
        var twoDigit := Map(line => 
            assume  {:axiom}  |line| > 2;
            [line[0],line[|line|-1]],line);
        
        // Convert to Integer
        result := Map(Integer, twoDigit);

        return result;
    }

    method problem1_1(input: string) returns (x: int)
    {
        var data := parseInput(input);

        // Sum the data -> FoldLeft takes (function, initial value, data)
        return FoldLeft((a,b) => a + b, 0, data);
    }

    method problem1_2(input: string) returns (x: int)
    {
        var data := parseInput2(input);

        return FoldLeft((a,b) => a + b, 0, data);
    }
}