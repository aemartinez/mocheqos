qos{(<= p (* 12.5 tnum))} U 
    [
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
    ]
    (Not (True U
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
        (Not qos{(<= p (* 12.5 2))})
    ))
