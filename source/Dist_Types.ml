(* Van Chan Ngo - 2017 *)

(* Bounded distributions *)
(* Bernoulli distribution - ber(pa, pb), k = 0, 1 *)
(* Binomial distribution - bin(n, pa, pb), k = 0,...,n *)
(* Hyper-geometric distribution - hyper(n, r, m), r <= n, m <= n, k = 0,...,m *)
(* Uniform distribution - unif(a, b), k = a,...,b *)

(* Unbounded distributions *)
(* Geometric distribution, - geo(pa, pb), k = 1,... *)
(* Negative binomial distribution - nbin(r, pa, pb), k = r,... *)
(* Poisson distribution - pois(la, lb), k = 0,... *) 


type dist_type = 
	| Det
	| Ber
	| Bin
	| Geo
	| Nbin
	| Pois
	| Hyper
	| Unif

type dist_op = 
	| Dadd
	| Dsub
	| Dmul

