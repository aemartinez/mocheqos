Not (True U 
    [
        repeat {
            U -> M : compute;
            M -> W : task;
            {
                M -> W : task
                |
                W -> M : result
            };
            {
                M -> U : wip
                |
                W -> M : result
            };
            M -> U : result
        }
    ]
    (Not 
        qos{(and (<= (* 6 tnum) p) (< p (* 12.5 tnum)))}
    ))
