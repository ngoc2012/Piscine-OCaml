let crossover a b =
        let len = min (List.length a) (List.length b) in
        List.init len (fun i -> a.(i) ^ b.(i))
