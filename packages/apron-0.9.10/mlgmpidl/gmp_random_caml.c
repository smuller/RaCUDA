/* File generated from gmp_random.idl */

/* This file is part of the MLGmpIDL interface, released under LGPL license.
   Please read the COPYING file packaged in the distribution  */

#include <stddef.h>
#include <string.h>
#include <caml/mlvalues.h>
#include <caml/memory.h>
#include <caml/alloc.h>
#include <caml/fail.h>
#include <caml/callback.h>
#ifdef Custom_tag
#include <caml/custom.h>
#include <caml/bigarray.h>
#endif
#include <caml/camlidlruntime.h>


#include "caml/custom.h"
#include "gmp_caml.h"

extern void camlidl_mpz_ptr_ml2c(value, mpz_ptr *);
#define camlidl_ml2c_mpz_mpz_ptr(v,c,ctx) camlidl_mpz_ptr_ml2c(v,c)

extern value camlidl_mpz_ptr_c2ml(mpz_ptr *);
#define camlidl_c2ml_mpz_mpz_ptr(c,ctx) camlidl_mpz_ptr_c2ml(c)


extern void camlidl_mpq_ptr_ml2c(value, mpq_ptr *);
#define camlidl_ml2c_mpq_mpq_ptr(v,c,ctx) camlidl_mpq_ptr_ml2c(v,c)

extern value camlidl_mpq_ptr_c2ml(mpq_ptr *);
#define camlidl_c2ml_mpq_mpq_ptr(c,ctx) camlidl_mpq_ptr_c2ml(c)


extern void camlidl_mpf_ptr_ml2c(value, mpf_ptr *);
#define camlidl_ml2c_mpf_mpf_ptr(v,c,ctx) camlidl_mpf_ptr_ml2c(v,c)

extern value camlidl_mpf_ptr_c2ml(mpf_ptr *);
#define camlidl_c2ml_mpf_mpf_ptr(c,ctx) camlidl_mpf_ptr_c2ml(c)


extern void camlidl_mpfr_ptr_ml2c(value, mpfr_ptr *);
#define camlidl_ml2c_mpfr_mpfr_ptr(v,c,ctx) camlidl_mpfr_ptr_ml2c(v,c)

extern value camlidl_mpfr_ptr_c2ml(mpfr_ptr *);
#define camlidl_c2ml_mpfr_mpfr_ptr(c,ctx) camlidl_mpfr_ptr_c2ml(c)


extern void camlidl_ml2c_mpfr_struct_mpfr_rnd_t(value, mpfr_rnd_t *, camlidl_ctx _ctx);
extern value camlidl_c2ml_mpfr_struct_mpfr_rnd_t(mpfr_rnd_t *, camlidl_ctx _ctx);

extern void camlidl_mpfr_rnd_t_ml2c(value, mpfr_rnd_t *);
#define camlidl_ml2c_mpfr_mpfr_rnd_t(v,c,ctx) camlidl_mpfr_rnd_t_ml2c(v,c)

extern value camlidl_mpfr_rnd_t_c2ml(mpfr_rnd_t *);
#define camlidl_c2ml_mpfr_mpfr_rnd_t(c,ctx) camlidl_mpfr_rnd_t_c2ml(c)


#define camlidl_ml2c_gmp_random_gmp_randstate_ptr(v,c,ctx) camlidl_gmp_randstate_ptr_ml2c(v,c)

#define camlidl_c2ml_gmp_random_gmp_randstate_ptr(c,ctx) camlidl_gmp_randstate_ptr_c2ml(c)

value camlidl_gmp_random_gmp_randinit_default(value _unit)
{
  gmp_randstate_t OUTOUTOUT; gmp_randstate_ptr OUTTOUTTOUTT = OUTOUTOUT;; /*out*/
  value _vres;

  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  gmp_randinit_default(OUTOUTOUT);
  _vres = camlidl_c2ml_gmp_random_gmp_randstate_ptr(&OUTTOUTTOUTT, _ctx);
  camlidl_free(_ctx);
  return _vres;
}

value camlidl_gmp_random_gmp_randinit_lc_2exp(
	value _v_A,
	value _v_C,
	value _v_M2EXP)
{
  gmp_randstate_t OUTOUTOUT; gmp_randstate_ptr OUTTOUTTOUTT = OUTOUTOUT;; /*out*/
  mpz_ptr A; /*in*/
  unsigned long C; /*in*/
  unsigned long M2EXP; /*in*/
  value _vres;

  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  camlidl_ml2c_mpz_mpz_ptr(_v_A, &A, _ctx);
  C = Long_val(_v_C);
  M2EXP = Long_val(_v_M2EXP);
  gmp_randinit_lc_2exp(OUTOUTOUT, A, C, M2EXP);
  _vres = camlidl_c2ml_gmp_random_gmp_randstate_ptr(&OUTTOUTTOUTT, _ctx);
  camlidl_free(_ctx);
  return _vres;
}

value camlidl_gmp_random_gmp_randinit_lc_2exp_size(
	value _v_SIZE)
{
  gmp_randstate_t OUTOUTOUT; gmp_randstate_ptr OUTTOUTTOUTT = OUTOUTOUT;; /*out*/
  unsigned long SIZE; /*in*/
  value _vres;

  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  SIZE = Long_val(_v_SIZE);
  /* begin user-supplied calling sequence */
  {
    int n = gmp_randinit_lc_2exp_size(OUTOUTOUT,SIZE);
    if (n==0) caml_invalid_argument("Argument not supported");
  }
  /* end user-supplied calling sequence */
  _vres = camlidl_c2ml_gmp_random_gmp_randstate_ptr(&OUTTOUTTOUTT, _ctx);
  camlidl_free(_ctx);
  return _vres;
}

value camlidl_gmp_random_gmp_randseed(
	value _v_STATE,
	value _v_SEED)
{
  gmp_randstate_ptr STATE; /*in*/
  mpz_ptr SEED; /*in*/
  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  camlidl_ml2c_gmp_random_gmp_randstate_ptr(_v_STATE, &STATE, _ctx);
  camlidl_ml2c_mpz_mpz_ptr(_v_SEED, &SEED, _ctx);
  gmp_randseed(STATE, SEED);
  camlidl_free(_ctx);
  return Val_unit;
}

value camlidl_gmp_random_gmp_randseed_ui(
	value _v_STATE,
	value _v_SEED)
{
  gmp_randstate_ptr STATE; /*in*/
  unsigned long SEED; /*in*/
  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  camlidl_ml2c_gmp_random_gmp_randstate_ptr(_v_STATE, &STATE, _ctx);
  SEED = Long_val(_v_SEED);
  gmp_randseed_ui(STATE, SEED);
  camlidl_free(_ctx);
  return Val_unit;
}

value camlidl_gmp_random_mpz_urandomb(
	value _v_ROP,
	value _v_STATE,
	value _v_N)
{
  mpz_ptr ROP; /*in*/
  gmp_randstate_ptr STATE; /*in*/
  unsigned long N; /*in*/
  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  camlidl_ml2c_mpz_mpz_ptr(_v_ROP, &ROP, _ctx);
  camlidl_ml2c_gmp_random_gmp_randstate_ptr(_v_STATE, &STATE, _ctx);
  N = Long_val(_v_N);
  mpz_urandomb(ROP, STATE, N);
  camlidl_free(_ctx);
  return Val_unit;
}

value camlidl_gmp_random_mpz_urandomm(
	value _v_ROP,
	value _v_STATE,
	value _v_N)
{
  mpz_ptr ROP; /*in*/
  gmp_randstate_ptr STATE; /*in*/
  mpz_ptr N; /*in*/
  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  camlidl_ml2c_mpz_mpz_ptr(_v_ROP, &ROP, _ctx);
  camlidl_ml2c_gmp_random_gmp_randstate_ptr(_v_STATE, &STATE, _ctx);
  camlidl_ml2c_mpz_mpz_ptr(_v_N, &N, _ctx);
  mpz_urandomm(ROP, STATE, N);
  camlidl_free(_ctx);
  return Val_unit;
}

value camlidl_gmp_random_mpz_rrandomb(
	value _v_ROP,
	value _v_STATE,
	value _v_N)
{
  mpz_ptr ROP; /*in*/
  gmp_randstate_ptr STATE; /*in*/
  unsigned long N; /*in*/
  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  camlidl_ml2c_mpz_mpz_ptr(_v_ROP, &ROP, _ctx);
  camlidl_ml2c_gmp_random_gmp_randstate_ptr(_v_STATE, &STATE, _ctx);
  N = Long_val(_v_N);
  mpz_rrandomb(ROP, STATE, N);
  camlidl_free(_ctx);
  return Val_unit;
}

value camlidl_gmp_random_mpf_urandomb(
	value _v_ROP,
	value _v_STATE,
	value _v_N)
{
  mpf_ptr ROP; /*in*/
  gmp_randstate_ptr STATE; /*in*/
  unsigned long N; /*in*/
  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  camlidl_ml2c_mpf_mpf_ptr(_v_ROP, &ROP, _ctx);
  camlidl_ml2c_gmp_random_gmp_randstate_ptr(_v_STATE, &STATE, _ctx);
  N = Long_val(_v_N);
  mpf_urandomb(ROP, STATE, N);
  camlidl_free(_ctx);
  return Val_unit;
}

value camlidl_gmp_random_mpfr_urandomb(
	value _v_ROP,
	value _v_STATE)
{
  mpfr_ptr ROP; /*in*/
  gmp_randstate_ptr STATE; /*in*/
  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  camlidl_ml2c_mpfr_mpfr_ptr(_v_ROP, &ROP, _ctx);
  camlidl_ml2c_gmp_random_gmp_randstate_ptr(_v_STATE, &STATE, _ctx);
  mpfr_urandomb(ROP, STATE);
  camlidl_free(_ctx);
  return Val_unit;
}

#if 0
/* Apparently this function is no longer provided by mpfr */
value camlidl_gmp_random_mpfr_random(
	value _v_ROP)
{
  mpfr_ptr ROP; /*in*/
  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  camlidl_ml2c_mpfr_mpfr_ptr(_v_ROP, &ROP, _ctx);
  mpfr_random(ROP);
  camlidl_free(_ctx);
  return Val_unit;
}
#endif
