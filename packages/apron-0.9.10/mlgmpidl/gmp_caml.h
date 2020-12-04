/* ********************************************************************** */
/* gmp_caml.h */
/* ********************************************************************** */

/* This file is part of the MLGmpIDL interface, released under LGPL license.
   Please read the COPYING file packaged in the distribution  */

#ifndef __GMP_CAML_H__
#define __GMP_CAML_H__

#include <stdio.h>
#include <assert.h>
#include "gmp.h"
#if defined(HAS_MPFR)
#if HAS_MPFR!=0
#include "mpfr.h"
#endif
#endif
#include "caml/mlvalues.h"

#ifdef __cplusplus
extern "C" {
#endif

struct gmp_randstate__t {
  gmp_randstate_t mp;
};
typedef __gmp_randstate_struct* gmp_randstate_ptr;

value camlidl_mpz_ptr_c2ml(mpz_ptr* mpz);
void camlidl_mpz_ptr_ml2c(value val, mpz_ptr* mpz);
void camlidl_mpz_ml2c(value val, __mpz_struct* mpz);

value camlidl_mpq_ptr_c2ml(mpq_ptr* mpq);
void camlidl_mpq_ptr_ml2c(value val, mpq_ptr* mpq);
void camlidl_mpq_ml2c(value val, __mpq_struct* mpq);

value camlidl_mpf_ptr_c2ml(mpf_ptr* mpf);
void camlidl_mpf_ptr_ml2c(value val, mpf_ptr* mpf);
void camlidl_mpf_ml2c(value val, __mpf_struct* mpf);

#if defined(HAS_MPFR)
#if HAS_MPFR!=0
value camlidl_mpfr_ptr_c2ml(mpfr_ptr* mpf);
void camlidl_mpfr_ptr_ml2c(value val, mpfr_ptr* mpf);
void camlidl_mpfr_ml2c(value val, __mpfr_struct* mpf);
static inline
value camlidl_mpfr_rnd_t_c2ml(mpfr_rnd_t* rnd)
{ assert(*rnd>=0
#ifdef GMP_RND_MAX
&& *rnd<GMP_RND_MAX
#endif
); return Val_int(*rnd); }
static inline
void camlidl_mpfr_rnd_t_ml2c(value val, mpfr_rnd_t* rnd)
{ *rnd = Int_val(val); assert(*rnd>=0
#ifdef GMP_RND_MAX
&& *rnd<GMP_RND_MAX
#endif
); }
#endif
#endif

value camlidl_gmp_randstate_ptr_c2ml(gmp_randstate_ptr* gmp_randstate);
void camlidl_gmp_randstate_ptr_ml2c(value val, gmp_randstate_ptr* gmp_randstate);


int mpz_fits_int_p (mpz_t OP);
int mpf_fits_int_p (mpf_t OP);

#ifdef __cplusplus
}
#endif

#endif
