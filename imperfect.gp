A061020(n) = {my(f=factorint(n)); prod(k=1, #f[,2], ((-f[k,1])^(f[k,2]+1)-1)/(-f[k,1]-1))} \\ _Andrew Lelechenko_, Apr 22 2014

A049417(n) = {my(b, f=factorint(n)); prod(k=1, #f[,2], b = binary(f[k,2]); prod(j=1, #b, if(b[j], 1+f[k,1]^(2^(#b-j)), 1)))} \\ _Andrew Lelechenko_, Apr 22 2014

A034448(n) = {my(f=factorint(n)); prod(k=1, #f[,2], f[k,1]^f[k,2]+1)} \\ _Andrew Lelechenko_, Apr 22 2014

A051377 (n) = {my(f=factor(n)); prod(i=1, #f[, 1], sumdiv(f[i, 2], d, f[i, 1]^d))}

A051377_new (n) = {my(f=factor(n)); prod(i=1, #f[, 1], sumdiv(f[i, 2]+1, d, f[i, 1]^(d-1)))}

A000026(n) = ((a)->factorback(a[,1])*factorback(a[,2])) (factorint(n))

isA000028(n) = vecsum(apply(hammingweight,factorint(n)[,2]))%2

A000030(n) = {my(n10); logint(n,10,&n10); n\n10}

A037445(n) = factorback(apply(a -> 2^hammingweight(a), factorint(n)[,2])) \\ _Andrew Lelechenko_, May 10 2014

biunitary_numdiv(n) = factorback(apply(a -> a+a%2, factorint(n)[,2]))
