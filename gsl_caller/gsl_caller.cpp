/* Van Chan Ngo - 2017 */
/*
	This file calls GSL library to compute 
	the probability, expectation, and other
	parameters of a given probability distribution.
	The outputs are sent to the standard output in 
	order to read by an OCaml reader.
 */

#include <iostream>
#include <fstream>
#include <vector>
#include <set>
#include <algorithm>
#include <functional>
#include <string>
#include <time.h>
/* fixed # digits for printing double */
#include <limits>

/* gsl library */
#include <gsl/gsl_rng.h>
#include <gsl/gsl_randist.h>
#include <gsl/gsl_cdf.h>

using namespace std;

typedef std::numeric_limits< double > dbl;
enum Dist_Type { Ber = 0, Bin, Geo, Nbin, Pois, Hyper, Unif };


/* Check the validity of parameters */
bool check_papameters (Dist_Type dt, int argc, char *argv[]) {

	return true;
}


/* Computes the probability p(k) of obtaining k from a Bernoulli distribution */
void ber_get_prob (double p) {

	cout.precision(dbl::max_digits10);
	for (unsigned int k = 0; k <= 1; k++) {
		cout << fixed << gsl_ran_bernoulli_pdf(k, p) << "\n";
	}
}

/* Computes the probability p(k) of obtaining k from a Binomial distribution */
void bin_get_prob (unsigned int n, double p) {

	cout.precision(dbl::max_digits10);
	for (unsigned int k = 0; k <= n; k++) {
		cout << fixed << gsl_ran_binomial_pdf(k, p, n) << "\n";
	}
}

/* Computes the probability p(k) of obtaining k from a Hyper-geometric distribution 
   n_1 = r
   n_2 = n - r
   t = m
*/
void hyper_get_prob (unsigned int n, unsigned int r, unsigned int m) {

	cout.precision(dbl::max_digits10);
	for (unsigned int k = 0; k <= m; k++) {
		cout << fixed << gsl_ran_hypergeometric_pdf(k, r, n - r, m) << "\n";
	}
}

/* Computes the probability p(k) of obtaining k from a uniform distribution */
void unif_get_prob (int a, int b) {

	cout.precision(dbl::max_digits10);
	double p = (double)1 / (double)(b - a + 1);
	for (int k = a; k <= b; k++) {
		cout << fixed << p << "\n";
	}	
}

int main(int argc, char *argv[]) {

	const gsl_rng_type *T;
	gsl_rng *r;

	gsl_rng_env_setup();
	T = gsl_rng_default;
	r = gsl_rng_alloc(T);

	// seed the generator
	srand(time(NULL));
	gsl_rng_set(r,random());


	// get the distribution type
	if (argc < 2) {
		// release the generator
		gsl_rng_free (r);
		return 1;
	}
	else {
		int dt = stoi(argv[1]);
		switch (dt)
		{
			case Ber:
			{
				if (check_papameters(Ber, argc, argv)) {
					double p = stod(argv[2]);
					ber_get_prob(p);
				}
				else {
					// release the generator
					gsl_rng_free (r);
					return 1;
				}
			}
			break;

			case Bin: 
			{
				if (check_papameters(Bin, argc, argv)) {
					unsigned int n = stoul(argv[2]);
					double p = stod(argv[3]);
					bin_get_prob(n, p);
				}
				else {
					// release the generator
					gsl_rng_free (r); 
					return 1;
				}
			}
			break;

			case Geo: 
			{
				// release the generator
				gsl_rng_free (r);
				return 1;
			}

			case Nbin: 
			{
				// release the generator
				gsl_rng_free (r);
				return 1;
			}

			case Pois: 
			{
				// release the generator
				gsl_rng_free (r);
				return 1;
			}

			case Hyper:
			{
				if (check_papameters(Hyper, argc, argv)) {
					unsigned int n = stoul(argv[2]);
					unsigned int r = stoul(argv[3]);
					unsigned int m = stoul(argv[4]);
					hyper_get_prob(n, r, m);
				}
				else {
					// release the generator
					gsl_rng_free (r);
					return 1;
				}
			}
			break;

			case Unif: 
			{
				if (check_papameters(Unif, argc, argv)) {
					unsigned int a = stoi(argv[2]);
					unsigned int b = stoi(argv[3]);
					unif_get_prob(a, b);
				}
				else {
					// release the generator
					gsl_rng_free (r);
					return 1;
				} 
			}

			default: 
			{
				// release the generator
				gsl_rng_free (r);	
				return 1;
			}
		}
	}

	// release the generator
	gsl_rng_free (r);

	return 0;
}