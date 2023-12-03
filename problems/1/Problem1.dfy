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
        var lines := Filter(line => line != "", splitOnBreak(input));
        var numsStr := Map(line => Filter(charInNC, line), lines);
        var trunk := Map(line => [line[0],line[|line|-1]], numsStr);
        result := Map(Integer, trunk);
        return result;   
    }

    method problem1_1(input: string) returns (x: int)
    {
        var data := parseInput(input);
        return FoldLeft((a,b) => a + b, 0, data);
    }

    method problem1_2(input: string) returns (x: int)
    {
        return 2;
    }
}