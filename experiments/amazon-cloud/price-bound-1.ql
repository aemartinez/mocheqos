Not (True U 
    [
        C -> A: cred;
        A -> C: token;
        C -> S: token;
        S -> C: ok;
        C -> S: helo;
        S -> C: int;
        C -> S: read;
        S -> C: size;
        C -> S: retr;
        S -> C: msg;
        C -> S: ack
    ] Not qos{(<= (+ (* (/ executionTime 3600) hourlyRateCompute) (* (/ executionTimeServer 3600) hourlyRateServerSoftware) (* (/ dataTransferredOut (* 1024 1024)) transferGBRate) (* usersAuthorized ratePerUserAuthorized) clientPriceIncomingEmails) 1)})