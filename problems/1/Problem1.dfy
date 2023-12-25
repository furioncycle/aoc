module Problem1 {
    import Std.BoundedInts
    import Std.Collections.Seq
    import Std.Strings   
    import Std.Strings.DecimalConversion
    import Std.Unicode.UnicodeStringsWithUnicodeChar
    import Std.Wrappers

    function CalibrationValue(line: string):  Wrappers.Result<nat,string> 
    {
        var firstDigitIndex :- Seq.IndexByOption(line, DecimalConversion.IsDigitChar).ToResult("No Digits");
        var lastDigitIndex :- Seq.LastIndexByOption(line, DecimalConversion.IsDigitChar).ToResult("No Digits");

        var resultStr := [line[firstDigitIndex], line[lastDigitIndex]];

        Wrappers.Success(Strings.ToNat(resultStr))
    }

    method problem1_1(input: string) returns (x: int)
    {
        var lines := Seq.Split(input, '\n');
        
        var calibrationValues :- expect Seq.MapWithResult(CalibrationValue, lines);

        return Seq.FoldLeft((a,b) => a + b, 0, calibrationValues);
    }

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
            else if DecimalConversion.IsDigitChar(input[0]) then
                parseLine(result+[input[0]],input[1..])
            else 
                parseLine(result,input[1..])
    }

    method problem1_2(input: string) returns (x: int)
    {
        var lines := Seq.Map(line => parseLine([], line), Seq.Split(input, '\n'));

        var calibrationValues :- expect Seq.MapWithResult(CalibrationValue, lines);

        return Seq.FoldLeft((a,b) => a + b, 0, calibrationValues);
    }
}