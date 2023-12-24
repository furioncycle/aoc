// include "../../libraries/src/dafny/Collections/Seqs.dfy"
module Problem1 {
    // import opened Split
    // import opened ParseInt
    // import opened Dafny.Collections.Seq
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

    method problem1_2(input: string) returns (x: int)
    {
        // var data := parseInput2(input);

        return 2; //FoldLeft((a,b) => a + b, 0, data);
    }
}