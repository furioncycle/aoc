include "../../parser/split.dfy"
include "../../parser/parseInt.dfy"
include "../../libraries/src/dafny/Collections/Seqs.dfy"

module Problem2 {

    import opened Split
    import opened ParseInt
    import opened Dafny.Collections.Seq
    
    datatype Cube = Red | Green | Blue
    datatype Game = Game(id: int, cubes: seq<multiset<Cube>> )

    predicate possibleGame(a: seq<multiset<Cube>>, b: multiset<Cube>) 
    {
        forall i :: 0 <= i < |a| ==> a[i] <= b
    }

    // This can be a Fold * Map ? Composable?
    method findAmount(g: seq<seq<string>>) returns (gameList: seq<multiset<Cube>>)
    { 
        // TODO - Implement in a Fold
        for i := 0 to |g| 
        {
            var s := Map(c => split(c, " "), g[i]);
            // TODO - Implement in a Map
            var colorSet: multiset<Cube> := multiset{};
            for j := 0 to |s|
            {
                var numberOfColors := Integer(s[j][1]);
                var color: Cube := match s[j][2] 
                {
                    case "red" => Red
                    case "green" => Green
                    case "blue" => Blue
                    case _ => Red
                };
                colorSet := colorSet[color :=  numberOfColors];
            }
            gameList := gameList + [colorSet];
        }
        return gameList;
    }

    method problem2_1(input: string) returns (x: int) {
        var lines := Filter(line => line != "", splitOnBreak(input));

        //Split by game
        var games := Map(game => split(game, ":"), lines);
                  // Parse game lists
                  var listGames: seq<Game> := [];
                for i := 0 to |games| 
        {
            var  grabbed := Map(hands => split(hands,";"), games[i][1..]);
            var game := Map(g => split(g, ","),grabbed[0]);
            var bag := findAmount(game);
            listGames := listGames + [Game(id := i + 1, cubes := bag)];
        }
        
        //Find the games that adhere to limit
        var limit: multiset<Cube> := multiset{};
        limit := limit[Blue := 14];
        limit := limit[Red := 12];
        limit := limit[Green := 13];
        var results := Filter((g: Game) => possibleGame(g.cubes, limit),listGames);
        
        //Get sum of Ids
        for j := 0 to |results|
        {
            x := x + results[j].id; 
        }
        return x;
    }

    method problem2_2(input: string) returns (x: int) {
        return 4;
    }
}