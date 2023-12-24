include "../../parser/split.dfy"
include "../../parser/parseInt.dfy"

module Problem2 {

    import opened Split
    import opened ParseInt
    import opened Std.Collections.Seq
    
    datatype Cube = Red | Green | Blue
    datatype Game = Game(id: int, cubes: seq<multiset<Cube>> )
    datatype Game2 = Game2(id: int, min:  ((int, Cube), (int, Cube), (int,Cube)))

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
            var bag := findAmount(Map(game => split(game, ","), grabbed[0]));
            listGames := listGames + [Game(id := i + 1, cubes := bag)];
        }
        
        //Find the games that adhere to limit
        var limit: multiset<Cube> := multiset{};
        limit := limit[Blue := 14];
        limit := limit[Red := 12];
        limit := limit[Green := 13];
        var results := Filter((g: Game) => possibleGame(g.cubes, limit),listGames);

        //Get sum of Ids - How to use Fold for this
        for j := 0 to |results|
        {
            x := x + results[j].id; 
        }
        return x;
    }
 
    method findMin(g: seq<seq<string>>) returns (gameList: ((int, Cube), (int, Cube), (int, Cube)))
    { 
        var greenMin: (int, Cube) := (0, Green);
        var redMin: (int, Cube) := (0, Red);
        var blueMin: (int, Cube) := (0, Blue);
        // TODO - Implement in a Fold
        for i := 0 to |g| 
        {
            var s := Map(c => split(c, " "), g[i]);
            // TODO - Implement in a Map
            // var colorSet: multiset<Cube> := multiset{};
            for j := 0 to |s|
            {
                var numberOfColors := Integer(s[j][1]);
                match s[j][2] 
                {
                    case "red" => 
                        if redMin.0 < numberOfColors {
                            redMin := (numberOfColors, Red);
                        }
                        // Red
                    case "green" => 
                        if greenMin.0 < numberOfColors {
                            greenMin := (numberOfColors,Green);
                        } 
                        // Green
                    case "blue" => 
                        if blueMin.0 < numberOfColors {
                            blueMin := (numberOfColors, Blue);
                        }
                        // Blue
                }
            }
        }
        return (redMin, greenMin, blueMin);
    }
    method problem2_2(input: string) returns (x: int) {
        var lines := Filter(line => line != "", splitOnBreak(input));

        //Split by game
        var games := Map(game => split(game, ":"), lines);
                  // Parse game lists
        var listGames: seq<Game2> := [];
        for i := 0 to |games| 
        {
            var  grabbed := Map(hands => split(hands,";"), games[i][1..]);

            // Need to figure how to get the min of the game
            var bag := findMin(Map(game => split(game, ","), grabbed[0])); 
            listGames := listGames + [Game2(id := i + 1, min := bag)];
        }

        var minMult: seq<int> := [];
        //Get sum of Ids - How to use Fold for this
        for j := 0 to |listGames|
        {
            minMult := minMult + [listGames[j].min.0.0 * listGames[j].min.1.0 * listGames[j].min.2.0];
        }
        return FoldLeft((a,b) => a + b, 0, minMult);
    }
}