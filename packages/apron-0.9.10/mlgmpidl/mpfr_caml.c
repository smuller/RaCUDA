/* File generated from mpfr.idl */

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


#define camlidl_ml2c_mpfr_mpfr_ptr(v,c,ctx) camlidl_mpfr_ptr_ml2c(v,c)

#define camlidl_c2ml_mpfr_mpfr_ptr(c,ctx) camlidl_mpfr_ptr_c2ml(c)

extern void camlidl_ml2c_mpfr_struct_mpfr_rnd_t(value, mpfr_rnd_t *, camlidl_ctx _ctx);
extern value camlidl_c2ml_mpfr_struct_mpfr_rnd_t(mpfr_rnd_t *, camlidl_ctx _ctx);

#define camlidl_ml2c_mpfr_mpfr_rnd_t(v,c,ctx) camlidl_mpfr_rnd_t_ml2c(v,c)

#define camlidl_c2ml_mpfr_mpfr_rnd_t(c,ctx) camlidl_mpfr_rnd_t_c2ml(c)

value camlidl_mpfr_mpfr_set_default_rounding_mode(
	value _v_RND)
{
  mpfr_rnd_t RND; /*in*/
  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  camlidl_ml2c_mpfr_mpfr_rnd_t(_v_RND, &RND, _ctx);
  mpfr_set_default_rounding_mode(RND);
  camlidl_free(_ctx);
  return Val_unit;
}

value camlidl_mpfr_mpfr_get_default_rounding_mode(value _unit)
{
  mpfr_rnd_t _res;
  value _vres;

  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  _res = mpfr_get_default_rounding_mode();
  _vres = camlidl_c2ml_mpfr_mpfr_rnd_t(&_res, _ctx);
  camlidl_free(_ctx);
  return _vres;
}

value camlidl_mpfr_mpfr_round_prec(
	value _v_X,
	value _v_RND,
	value _v_PREC)
{
  mpfr_ptr X; /*in*/
  mpfr_rnd_t RND; /*in*/
  unsigned long PREC; /*in*/
  int _res;
  value _vres;

  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  camlidl_ml2c_mpfr_mpfr_ptr(_v_X, &X, _ctx);
  camlidl_ml2c_mpfr_mpfr_rnd_t(_v_RND, &RND, _ctx);
  PREC = Long_val(_v_PREC);
  _res = mpfr_round_prec(X, RND, PREC);
  _vres = Val_int(_res);
  camlidl_free(_ctx);
  return _vres;
}

value camlidl_mpfr_mpfr_get_emin(value _unit)
{
  long _res;
  value _vres;

  _res = mpfr_get_emin();
  _vres = Val_long(_res);
  return _vres;
}

value camlidl_mpfr_mpfr_get_emax(value _unit)
{
  long _res;
  value _vres;

  _res = mpfr_get_emax();
  _vres = Val_long(_res);
  return _vres;
}

value camlidl_mpfr_mpfr_set_emin(
	value _v_EXP)
{
  long EXP; /*in*/
  EXP = Long_val(_v_EXP);
  /* begin user-supplied calling sequence */
{
int n = mpfr_set_emin(EXP);
if (n){ caml_invalid_argument(""); }
  }
  /* end user-supplied calling sequence */
  return Val_unit;
}

value camlidl_mpfr_mpfr_set_emax(
	value _v_EXP)
{
  long EXP; /*in*/
  EXP = Long_val(_v_EXP);
  /* begin user-supplied calling sequence */
{
int n = mpfr_set_emax(EXP);
if (n){ caml_invalid_argument(""); }
  }
  /* end user-supplied calling sequence */
  return Val_unit;
}

value camlidl_mpfr_mpfr_check_range(
	value _v_X,
	value _v_T,
	value _v_RND)
{
  mpfr_ptr X; /*in*/
  int T; /*in*/
  mpfr_rnd_t RND; /*in*/
  int _res;
  value _vres;

  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  camlidl_ml2c_mpfr_mpfr_ptr(_v_X, &X, _ctx);
  T = Int_val(_v_T);
  camlidl_ml2c_mpfr_mpfr_rnd_t(_v_RND, &RND, _ctx);
  _res = mpfr_check_range(X, T, RND);
  _vres = Val_int(_res);
  camlidl_free(_ctx);
  return _vres;
}

value camlidl_mpfr_mpfr_clear_underflow(value _unit)
{
  mpfr_clear_underflow();
  return Val_unit;
}

value camlidl_mpfr_mpfr_clear_overflow(value _unit)
{
  mpfr_clear_overflow();
  return Val_unit;
}

value camlidl_mpfr_mpfr_clear_nanflag(value _unit)
{
  mpfr_clear_nanflag();
  return Val_unit;
}

value camlidl_mpfr_mpfr_clear_inexflag(value _unit)
{
  mpfr_clear_inexflag();
  return Val_unit;
}

value camlidl_mpfr_mpfr_clear_flags(value _unit)
{
  mpfr_clear_flags();
  return Val_unit;
}

value camlidl_mpfr_mpfr_underflow_p(value _unit)
{
  int _res;
  value _vres;

  _res = mpfr_underflow_p();
  _vres = Val_int(_res);
  return _vres;
}

value camlidl_mpfr_mpfr_overflow_p(value _unit)
{
  int _res;
  value _vres;

  _res = mpfr_overflow_p();
  _vres = Val_int(_res);
  return _vres;
}

value camlidl_mpfr_mpfr_nanflag_p(value _unit)
{
  int _res;
  value _vres;

  _res = mpfr_nanflag_p();
  _vres = Val_int(_res);
  return _vres;
}

value camlidl_mpfr_mpfr_inexflag_p(value _unit)
{
  int _res;
  value _vres;

  _res = mpfr_inexflag_p();
  _vres = Val_int(_res);
  return _vres;
}

value camlidl_mpfr_mpfr_set_default_prec(
	value _v_PREC)
{
  unsigned long PREC; /*in*/
  PREC = Long_val(_v_PREC);
  mpfr_set_default_prec(PREC);
  return Val_unit;
}

value camlidl_mpfr_mpfr_get_default_prec(value _unit)
{
  unsigned long _res;
  value _vres;

  _res = mpfr_get_default_prec();
  _vres = Val_long(_res);
  return _vres;
}

value camlidl_mpfr_mpfr_init(value _unit)
{
  mpfr_t OUTOUTOUT; mpfr_ptr OUTTOUTTOUTT = OUTOUTOUT;; /*out*/
  value _vres;

  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  mpfr_init(OUTOUTOUT);
  _vres = camlidl_c2ml_mpfr_mpfr_ptr(&OUTTOUTTOUTT, _ctx);
  camlidl_free(_ctx);
  return _vres;
}

value camlidl_mpfr_mpfr_init2(
	value _v_PREC)
{
  mpfr_t OUTOUTOUT; mpfr_ptr OUTTOUTTOUTT = OUTOUTOUT;; /*out*/
  unsigned long PREC; /*in*/
  value _vres;

  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  PREC = Long_val(_v_PREC);
  mpfr_init2(OUTOUTOUT, PREC);
  _vres = camlidl_c2ml_mpfr_mpfr_ptr(&OUTTOUTTOUTT, _ctx);
  camlidl_free(_ctx);
  return _vres;
}

value camlidl_mpfr_mpfr_get_prec(
	value _v_OP)
{
  mpfr_ptr OP; /*in*/
  unsigned long _res;
  value _vres;

  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  camlidl_ml2c_mpfr_mpfr_ptr(_v_OP, &OP, _ctx);
  _res = mpfr_get_prec(OP);
  _vres = Val_long(_res);
  camlidl_free(_ctx);
  return _vres;
}

value camlidl_mpfr_mpfr_set_prec(
	value _v_ROP,
	value _v_PREC)
{
  mpfr_ptr ROP; /*in*/
  unsigned long PREC; /*in*/
  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  camlidl_ml2c_mpfr_mpfr_ptr(_v_ROP, &ROP, _ctx);
  PREC = Long_val(_v_PREC);
  mpfr_set_prec(ROP, PREC);
  camlidl_free(_ctx);
  return Val_unit;
}

value camlidl_mpfr_mpfr_set_prec_raw(
	value _v_ROP,
	value _v_PREC)
{
  mpfr_ptr ROP; /*in*/
  unsigned long PREC; /*in*/
  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  camlidl_ml2c_mpfr_mpfr_ptr(_v_ROP, &ROP, _ctx);
  PREC = Long_val(_v_PREC);
  mpfr_set_prec_raw(ROP, PREC);
  camlidl_free(_ctx);
  return Val_unit;
}

value camlidl_mpfr_mpfr_set(
	value _v_ROP,
	value _v_OP,
	value _v_RND)
{
  mpfr_ptr ROP; /*in*/
  mpfr_ptr OP; /*in*/
  mpfr_rnd_t RND; /*in*/
  int _res;
  value _vres;

  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  camlidl_ml2c_mpfr_mpfr_ptr(_v_ROP, &ROP, _ctx);
  camlidl_ml2c_mpfr_mpfr_ptr(_v_OP, &OP, _ctx);
  camlidl_ml2c_mpfr_mpfr_rnd_t(_v_RND, &RND, _ctx);
  _res = mpfr_set(ROP, OP, RND);
  _vres = Val_int(_res);
  camlidl_free(_ctx);
  return _vres;
}

value camlidl_mpfr_mpfr_set_si(
	value _v_ROP,
	value _v_OP,
	value _v_RND)
{
  mpfr_ptr ROP; /*in*/
  long OP; /*in*/
  mpfr_rnd_t RND; /*in*/
  int _res;
  value _vres;

  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  camlidl_ml2c_mpfr_mpfr_ptr(_v_ROP, &ROP, _ctx);
  OP = Long_val(_v_OP);
  camlidl_ml2c_mpfr_mpfr_rnd_t(_v_RND, &RND, _ctx);
  _res = mpfr_set_si(ROP, OP, RND);
  _vres = Val_int(_res);
  camlidl_free(_ctx);
  return _vres;
}

value camlidl_mpfr_mpfr_set_d(
	value _v_ROP,
	value _v_OP,
	value _v_RND)
{
  mpfr_ptr ROP; /*in*/
  double OP; /*in*/
  mpfr_rnd_t RND; /*in*/
  int _res;
  value _vres;

  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  camlidl_ml2c_mpfr_mpfr_ptr(_v_ROP, &ROP, _ctx);
  OP = Double_val(_v_OP);
  camlidl_ml2c_mpfr_mpfr_rnd_t(_v_RND, &RND, _ctx);
  _res = mpfr_set_d(ROP, OP, RND);
  _vres = Val_int(_res);
  camlidl_free(_ctx);
  return _vres;
}

value camlidl_mpfr_mpfr_set_z(
	value _v_ROP,
	value _v_OP,
	value _v_RND)
{
  mpfr_ptr ROP; /*in*/
  mpz_ptr OP; /*in*/
  mpfr_rnd_t RND; /*in*/
  int _res;
  value _vres;

  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  camlidl_ml2c_mpfr_mpfr_ptr(_v_ROP, &ROP, _ctx);
  camlidl_ml2c_mpz_mpz_ptr(_v_OP, &OP, _ctx);
  camlidl_ml2c_mpfr_mpfr_rnd_t(_v_RND, &RND, _ctx);
  _res = mpfr_set_z(ROP, OP, RND);
  _vres = Val_int(_res);
  camlidl_free(_ctx);
  return _vres;
}

value camlidl_mpfr_mpfr_set_q(
	value _v_ROP,
	value _v_OP,
	value _v_RND)
{
  mpfr_ptr ROP; /*in*/
  mpq_ptr OP; /*in*/
  mpfr_rnd_t RND; /*in*/
  int _res;
  value _vres;

  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  camlidl_ml2c_mpfr_mpfr_ptr(_v_ROP, &ROP, _ctx);
  camlidl_ml2c_mpq_mpq_ptr(_v_OP, &OP, _ctx);
  camlidl_ml2c_mpfr_mpfr_rnd_t(_v_RND, &RND, _ctx);
  _res = mpfr_set_q(ROP, OP, RND);
  _vres = Val_int(_res);
  camlidl_free(_ctx);
  return _vres;
}

value camlidl_mpfr_mpfr__set_str(
	value _v_ROP,
	value _v_STR,
	value _v_BASE,
	value _v_RND)
{
  mpfr_ptr ROP; /*in*/
  char *STR; /*in*/
  int BASE; /*in*/
  mpfr_rnd_t RND; /*in*/
  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  camlidl_ml2c_mpfr_mpfr_ptr(_v_ROP, &ROP, _ctx);
  STR = String_val(_v_STR);
  BASE = Int_val(_v_BASE);
  camlidl_ml2c_mpfr_mpfr_rnd_t(_v_RND, &RND, _ctx);
  /* begin user-supplied calling sequence */
{
  int n = mpfr_set_str(ROP,STR,BASE,RND);
  if (n){ mpfr_clear(ROP); caml_invalid_argument(""); }
  }
  /* end user-supplied calling sequence */
  camlidl_free(_ctx);
  return Val_unit;
}

value camlidl_mpfr_mpfr_set_f(
	value _v_X,
	value _v_Y,
	value _v_RND)
{
  mpfr_ptr X; /*in*/
  mpf_ptr Y; /*in*/
  mpfr_rnd_t RND; /*in*/
  int _res;
  value _vres;

  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  camlidl_ml2c_mpfr_mpfr_ptr(_v_X, &X, _ctx);
  camlidl_ml2c_mpf_mpf_ptr(_v_Y, &Y, _ctx);
  camlidl_ml2c_mpfr_mpfr_rnd_t(_v_RND, &RND, _ctx);
  _res = mpfr_set_f(X, Y, RND);
  _vres = Val_int(_res);
  camlidl_free(_ctx);
  return _vres;
}

value camlidl_mpfr_mpfr_set_si_2exp(
	value _v_ROP,
	value _v_OP,
	value _v_EXPNT,
	value _v_RND)
{
  mpfr_ptr ROP; /*in*/
  long OP; /*in*/
  long EXPNT; /*in*/
  mpfr_rnd_t RND; /*in*/
  int _res;
  value _vres;

  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  camlidl_ml2c_mpfr_mpfr_ptr(_v_ROP, &ROP, _ctx);
  OP = Long_val(_v_OP);
  EXPNT = Long_val(_v_EXPNT);
  camlidl_ml2c_mpfr_mpfr_rnd_t(_v_RND, &RND, _ctx);
  _res = mpfr_set_si_2exp(ROP, OP, EXPNT, RND);
  _vres = Val_int(_res);
  camlidl_free(_ctx);
  return _vres;
}

value camlidl_mpfr_mpfr_set_inf(
	value _v_X,
	value _v_SIGN)
{
  mpfr_ptr X; /*in*/
  int SIGN; /*in*/
  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  camlidl_ml2c_mpfr_mpfr_ptr(_v_X, &X, _ctx);
  SIGN = Int_val(_v_SIGN);
  mpfr_set_inf(X, SIGN);
  camlidl_free(_ctx);
  return Val_unit;
}

value camlidl_mpfr_mpfr_set_nan(
	value _v_X)
{
  mpfr_ptr X; /*in*/
  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  camlidl_ml2c_mpfr_mpfr_ptr(_v_X, &X, _ctx);
  mpfr_set_nan(X);
  camlidl_free(_ctx);
  return Val_unit;
}

value camlidl_mpfr_mpfr_swap(
	value _v_ROP1,
	value _v_ROP2)
{
  mpfr_ptr ROP1; /*in*/
  mpfr_ptr ROP2; /*in*/
  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  camlidl_ml2c_mpfr_mpfr_ptr(_v_ROP1, &ROP1, _ctx);
  camlidl_ml2c_mpfr_mpfr_ptr(_v_ROP2, &ROP2, _ctx);
  mpfr_swap(ROP1, ROP2);
  camlidl_free(_ctx);
  return Val_unit;
}

value camlidl_mpfr_mpfr_init_set(
	value _v_OP,
	value _v_RND)
{
  mpfr_t OUTOUTOUT; mpfr_ptr OUTTOUTTOUTT = OUTOUTOUT;; /*out*/
  mpfr_ptr OP; /*in*/
  mpfr_rnd_t RND; /*in*/
  int _res;
  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  value _vresult;
  value _vres[2] = { 0, 0, };

  camlidl_ml2c_mpfr_mpfr_ptr(_v_OP, &OP, _ctx);
  camlidl_ml2c_mpfr_mpfr_rnd_t(_v_RND, &RND, _ctx);
  _res = mpfr_init_set(OUTOUTOUT, OP, RND);
  Begin_roots_block(_vres, 2)
    _vres[0] = Val_int(_res);
    _vres[1] = camlidl_c2ml_mpfr_mpfr_ptr(&OUTTOUTTOUTT, _ctx);
    _vresult = camlidl_alloc_small(2, 0);
    Field(_vresult, 0) = _vres[0];
    Field(_vresult, 1) = _vres[1];
  End_roots()
  camlidl_free(_ctx);
  return _vresult;
}

value camlidl_mpfr_mpfr_init_set_si(
	value _v_OP,
	value _v_RND)
{
  mpfr_t OUTOUTOUT; mpfr_ptr OUTTOUTTOUTT = OUTOUTOUT;; /*out*/
  long OP; /*in*/
  mpfr_rnd_t RND; /*in*/
  int _res;
  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  value _vresult;
  value _vres[2] = { 0, 0, };

  OP = Long_val(_v_OP);
  camlidl_ml2c_mpfr_mpfr_rnd_t(_v_RND, &RND, _ctx);
  _res = mpfr_init_set_si(OUTOUTOUT, OP, RND);
  Begin_roots_block(_vres, 2)
    _vres[0] = Val_int(_res);
    _vres[1] = camlidl_c2ml_mpfr_mpfr_ptr(&OUTTOUTTOUTT, _ctx);
    _vresult = camlidl_alloc_small(2, 0);
    Field(_vresult, 0) = _vres[0];
    Field(_vresult, 1) = _vres[1];
  End_roots()
  camlidl_free(_ctx);
  return _vresult;
}

value camlidl_mpfr_mpfr_init_set_d(
	value _v_OP,
	value _v_RND)
{
  mpfr_t OUTOUTOUT; mpfr_ptr OUTTOUTTOUTT = OUTOUTOUT;; /*out*/
  double OP; /*in*/
  mpfr_rnd_t RND; /*in*/
  int _res;
  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  value _vresult;
  value _vres[2] = { 0, 0, };

  OP = Double_val(_v_OP);
  camlidl_ml2c_mpfr_mpfr_rnd_t(_v_RND, &RND, _ctx);
  _res = mpfr_init_set_d(OUTOUTOUT, OP, RND);
  Begin_roots_block(_vres, 2)
    _vres[0] = Val_int(_res);
    _vres[1] = camlidl_c2ml_mpfr_mpfr_ptr(&OUTTOUTTOUTT, _ctx);
    _vresult = camlidl_alloc_small(2, 0);
    Field(_vresult, 0) = _vres[0];
    Field(_vresult, 1) = _vres[1];
  End_roots()
  camlidl_free(_ctx);
  return _vresult;
}

value camlidl_mpfr_mpfr_init_set_f(
	value _v_OP,
	value _v_RND)
{
  mpfr_t OUTOUTOUT; mpfr_ptr OUTTOUTTOUTT = OUTOUTOUT;; /*out*/
  mpf_ptr OP; /*in*/
  mpfr_rnd_t RND; /*in*/
  int _res;
  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  value _vresult;
  value _vres[2] = { 0, 0, };

  camlidl_ml2c_mpf_mpf_ptr(_v_OP, &OP, _ctx);
  camlidl_ml2c_mpfr_mpfr_rnd_t(_v_RND, &RND, _ctx);
  _res = mpfr_init_set_f(OUTOUTOUT, OP, RND);
  Begin_roots_block(_vres, 2)
    _vres[0] = Val_int(_res);
    _vres[1] = camlidl_c2ml_mpfr_mpfr_ptr(&OUTTOUTTOUTT, _ctx);
    _vresult = camlidl_alloc_small(2, 0);
    Field(_vresult, 0) = _vres[0];
    Field(_vresult, 1) = _vres[1];
  End_roots()
  camlidl_free(_ctx);
  return _vresult;
}

value camlidl_mpfr_mpfr_init_set_z(
	value _v_OP,
	value _v_RND)
{
  mpfr_t OUTOUTOUT; mpfr_ptr OUTTOUTTOUTT = OUTOUTOUT;; /*out*/
  mpz_ptr OP; /*in*/
  mpfr_rnd_t RND; /*in*/
  int _res;
  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  value _vresult;
  value _vres[2] = { 0, 0, };

  camlidl_ml2c_mpz_mpz_ptr(_v_OP, &OP, _ctx);
  camlidl_ml2c_mpfr_mpfr_rnd_t(_v_RND, &RND, _ctx);
  _res = mpfr_init_set_z(OUTOUTOUT, OP, RND);
  Begin_roots_block(_vres, 2)
    _vres[0] = Val_int(_res);
    _vres[1] = camlidl_c2ml_mpfr_mpfr_ptr(&OUTTOUTTOUTT, _ctx);
    _vresult = camlidl_alloc_small(2, 0);
    Field(_vresult, 0) = _vres[0];
    Field(_vresult, 1) = _vres[1];
  End_roots()
  camlidl_free(_ctx);
  return _vresult;
}

value camlidl_mpfr_mpfr_init_set_q(
	value _v_OP,
	value _v_RND)
{
  mpfr_t OUTOUTOUT; mpfr_ptr OUTTOUTTOUTT = OUTOUTOUT;; /*out*/
  mpq_ptr OP; /*in*/
  mpfr_rnd_t RND; /*in*/
  int _res;
  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  value _vresult;
  value _vres[2] = { 0, 0, };

  camlidl_ml2c_mpq_mpq_ptr(_v_OP, &OP, _ctx);
  camlidl_ml2c_mpfr_mpfr_rnd_t(_v_RND, &RND, _ctx);
  _res = mpfr_init_set_q(OUTOUTOUT, OP, RND);
  Begin_roots_block(_vres, 2)
    _vres[0] = Val_int(_res);
    _vres[1] = camlidl_c2ml_mpfr_mpfr_ptr(&OUTTOUTTOUTT, _ctx);
    _vresult = camlidl_alloc_small(2, 0);
    Field(_vresult, 0) = _vres[0];
    Field(_vresult, 1) = _vres[1];
  End_roots()
  camlidl_free(_ctx);
  return _vresult;
}

value camlidl_mpfr_mpfr__init_set_str(
	value _v_STR,
	value _v_BASE,
	value _v_RND)
{
  mpfr_t OUTOUTOUT; mpfr_ptr OUTTOUTTOUTT = OUTOUTOUT;; /*out*/
  char *STR; /*in*/
  int BASE; /*in*/
  mpfr_rnd_t RND; /*in*/
  value _vres;

  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  STR = String_val(_v_STR);
  BASE = Int_val(_v_BASE);
  camlidl_ml2c_mpfr_mpfr_rnd_t(_v_RND, &RND, _ctx);
  /* begin user-supplied calling sequence */
  {
  int n = mpfr_init_set_str(OUTOUTOUT,STR,BASE,RND);
  if (n){ mpfr_clear(OUTOUTOUT); caml_invalid_argument(""); }
  }
  /* end user-supplied calling sequence */
  _vres = camlidl_c2ml_mpfr_mpfr_ptr(&OUTTOUTTOUTT, _ctx);
  camlidl_free(_ctx);
  return _vres;
}

value camlidl_mpfr_mpfr_get_d(
	value _v_OP,
	value _v_RND)
{
  mpfr_ptr OP; /*in*/
  mpfr_rnd_t RND; /*in*/
  double _res;
  value _vres;

  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  camlidl_ml2c_mpfr_mpfr_ptr(_v_OP, &OP, _ctx);
  camlidl_ml2c_mpfr_mpfr_rnd_t(_v_RND, &RND, _ctx);
  _res = mpfr_get_d(OP, RND);
  _vres = copy_double(_res);
  camlidl_free(_ctx);
  return _vres;
}

value camlidl_mpfr_mpfr_get_d1(
	value _v_OP)
{
  mpfr_ptr OP; /*in*/
  double _res;
  value _vres;

  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  camlidl_ml2c_mpfr_mpfr_ptr(_v_OP, &OP, _ctx);
  _res = mpfr_get_d1(OP);
  _vres = copy_double(_res);
  camlidl_free(_ctx);
  return _vres;
}

value camlidl_mpfr_mpfr_get_z_exp(
	value _v_Z,
	value _v_OP)
{
  mpz_ptr Z; /*in*/
  mpfr_ptr OP; /*in*/
  int _res;
  value _vres;

  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  camlidl_ml2c_mpz_mpz_ptr(_v_Z, &Z, _ctx);
  camlidl_ml2c_mpfr_mpfr_ptr(_v_OP, &OP, _ctx);
  _res = mpfr_get_z_exp(Z, OP);
  _vres = Val_int(_res);
  camlidl_free(_ctx);
  return _vres;
}

value camlidl_mpfr_mpfr_get_z(
	value _v_Z,
	value _v_OP,
	value _v_RND)
{
  mpz_ptr Z; /*in*/
  mpfr_ptr OP; /*in*/
  mpfr_rnd_t RND; /*in*/
  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  camlidl_ml2c_mpz_mpz_ptr(_v_Z, &Z, _ctx);
  camlidl_ml2c_mpfr_mpfr_ptr(_v_OP, &OP, _ctx);
  camlidl_ml2c_mpfr_mpfr_rnd_t(_v_RND, &RND, _ctx);
  mpfr_get_z(Z, OP, RND);
  camlidl_free(_ctx);
  return Val_unit;
}

value camlidl_mpfr_mpfr__get_str(
	value _v_BASE,
	value _v_N_DIGITS,
	value _v_OP,
	value _v_RND)
{
  long *EXPPTR; /*out*/
  int BASE; /*in*/
  int N_DIGITS; /*in*/
  mpfr_ptr OP; /*in*/
  mpfr_rnd_t RND; /*in*/
  char *_res;
  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  long _c1;
  value _vresult;
  value _vres[2] = { 0, 0, };

  BASE = Int_val(_v_BASE);
  N_DIGITS = Int_val(_v_N_DIGITS);
  camlidl_ml2c_mpfr_mpfr_ptr(_v_OP, &OP, _ctx);
  camlidl_ml2c_mpfr_mpfr_rnd_t(_v_RND, &RND, _ctx);
  EXPPTR = &_c1;
  /* begin user-supplied calling sequence */
_res = mpfr_get_str(NULL,EXPPTR,BASE,N_DIGITS,OP,RND);
  /* end user-supplied calling sequence */
  Begin_roots_block(_vres, 2)
    _vres[0] = copy_string(_res);
    _vres[1] = Val_long(*EXPPTR);
    _vresult = camlidl_alloc_small(2, 0);
    Field(_vresult, 0) = _vres[0];
    Field(_vresult, 1) = _vres[1];
  End_roots()
  camlidl_free(_ctx);
  /* begin user-supplied deallocation sequence */
free(_res);
  /* end user-supplied deallocation sequence */
  return _vresult;
}

value camlidl_mpfr_mpfr_add(
	value _v_ROP,
	value _v_OP1,
	value _v_OP2,
	value _v_RND)
{
  mpfr_ptr ROP; /*in*/
  mpfr_ptr OP1; /*in*/
  mpfr_ptr OP2; /*in*/
  mpfr_rnd_t RND; /*in*/
  int _res;
  value _vres;

  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  camlidl_ml2c_mpfr_mpfr_ptr(_v_ROP, &ROP, _ctx);
  camlidl_ml2c_mpfr_mpfr_ptr(_v_OP1, &OP1, _ctx);
  camlidl_ml2c_mpfr_mpfr_ptr(_v_OP2, &OP2, _ctx);
  camlidl_ml2c_mpfr_mpfr_rnd_t(_v_RND, &RND, _ctx);
  _res = mpfr_add(ROP, OP1, OP2, RND);
  _vres = Val_int(_res);
  camlidl_free(_ctx);
  return _vres;
}

value camlidl_mpfr_mpfr_add_ui(
	value _v_ROP,
	value _v_OP1,
	value _v_OP2,
	value _v_RND)
{
  mpfr_ptr ROP; /*in*/
  mpfr_ptr OP1; /*in*/
  unsigned long OP2; /*in*/
  mpfr_rnd_t RND; /*in*/
  int _res;
  value _vres;

  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  camlidl_ml2c_mpfr_mpfr_ptr(_v_ROP, &ROP, _ctx);
  camlidl_ml2c_mpfr_mpfr_ptr(_v_OP1, &OP1, _ctx);
  OP2 = Long_val(_v_OP2);
  camlidl_ml2c_mpfr_mpfr_rnd_t(_v_RND, &RND, _ctx);
  _res = mpfr_add_ui(ROP, OP1, OP2, RND);
  _vres = Val_int(_res);
  camlidl_free(_ctx);
  return _vres;
}

value camlidl_mpfr_mpfr_add_z(
	value _v_ROP,
	value _v_OP1,
	value _v_OP2,
	value _v_RND)
{
  mpfr_ptr ROP; /*in*/
  mpfr_ptr OP1; /*in*/
  mpz_ptr OP2; /*in*/
  mpfr_rnd_t RND; /*in*/
  int _res;
  value _vres;

  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  camlidl_ml2c_mpfr_mpfr_ptr(_v_ROP, &ROP, _ctx);
  camlidl_ml2c_mpfr_mpfr_ptr(_v_OP1, &OP1, _ctx);
  camlidl_ml2c_mpz_mpz_ptr(_v_OP2, &OP2, _ctx);
  camlidl_ml2c_mpfr_mpfr_rnd_t(_v_RND, &RND, _ctx);
  _res = mpfr_add_z(ROP, OP1, OP2, RND);
  _vres = Val_int(_res);
  camlidl_free(_ctx);
  return _vres;
}

value camlidl_mpfr_mpfr_add_q(
	value _v_ROP,
	value _v_OP1,
	value _v_OP2,
	value _v_RND)
{
  mpfr_ptr ROP; /*in*/
  mpfr_ptr OP1; /*in*/
  mpq_ptr OP2; /*in*/
  mpfr_rnd_t RND; /*in*/
  int _res;
  value _vres;

  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  camlidl_ml2c_mpfr_mpfr_ptr(_v_ROP, &ROP, _ctx);
  camlidl_ml2c_mpfr_mpfr_ptr(_v_OP1, &OP1, _ctx);
  camlidl_ml2c_mpq_mpq_ptr(_v_OP2, &OP2, _ctx);
  camlidl_ml2c_mpfr_mpfr_rnd_t(_v_RND, &RND, _ctx);
  _res = mpfr_add_q(ROP, OP1, OP2, RND);
  _vres = Val_int(_res);
  camlidl_free(_ctx);
  return _vres;
}

value camlidl_mpfr_mpfr_sub(
	value _v_ROP,
	value _v_OP1,
	value _v_OP2,
	value _v_RND)
{
  mpfr_ptr ROP; /*in*/
  mpfr_ptr OP1; /*in*/
  mpfr_ptr OP2; /*in*/
  mpfr_rnd_t RND; /*in*/
  int _res;
  value _vres;

  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  camlidl_ml2c_mpfr_mpfr_ptr(_v_ROP, &ROP, _ctx);
  camlidl_ml2c_mpfr_mpfr_ptr(_v_OP1, &OP1, _ctx);
  camlidl_ml2c_mpfr_mpfr_ptr(_v_OP2, &OP2, _ctx);
  camlidl_ml2c_mpfr_mpfr_rnd_t(_v_RND, &RND, _ctx);
  _res = mpfr_sub(ROP, OP1, OP2, RND);
  _vres = Val_int(_res);
  camlidl_free(_ctx);
  return _vres;
}

value camlidl_mpfr_mpfr_ui_sub(
	value _v_ROP,
	value _v_OP1,
	value _v_OP2,
	value _v_RND)
{
  mpfr_ptr ROP; /*in*/
  unsigned long OP1; /*in*/
  mpfr_ptr OP2; /*in*/
  mpfr_rnd_t RND; /*in*/
  int _res;
  value _vres;

  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  camlidl_ml2c_mpfr_mpfr_ptr(_v_ROP, &ROP, _ctx);
  OP1 = Long_val(_v_OP1);
  camlidl_ml2c_mpfr_mpfr_ptr(_v_OP2, &OP2, _ctx);
  camlidl_ml2c_mpfr_mpfr_rnd_t(_v_RND, &RND, _ctx);
  _res = mpfr_ui_sub(ROP, OP1, OP2, RND);
  _vres = Val_int(_res);
  camlidl_free(_ctx);
  return _vres;
}

value camlidl_mpfr_mpfr_sub_ui(
	value _v_ROP,
	value _v_OP1,
	value _v_OP2,
	value _v_RND)
{
  mpfr_ptr ROP; /*in*/
  mpfr_ptr OP1; /*in*/
  unsigned long OP2; /*in*/
  mpfr_rnd_t RND; /*in*/
  int _res;
  value _vres;

  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  camlidl_ml2c_mpfr_mpfr_ptr(_v_ROP, &ROP, _ctx);
  camlidl_ml2c_mpfr_mpfr_ptr(_v_OP1, &OP1, _ctx);
  OP2 = Long_val(_v_OP2);
  camlidl_ml2c_mpfr_mpfr_rnd_t(_v_RND, &RND, _ctx);
  _res = mpfr_sub_ui(ROP, OP1, OP2, RND);
  _vres = Val_int(_res);
  camlidl_free(_ctx);
  return _vres;
}

value camlidl_mpfr_mpfr_sub_z(
	value _v_ROP,
	value _v_OP1,
	value _v_OP2,
	value _v_RND)
{
  mpfr_ptr ROP; /*in*/
  mpfr_ptr OP1; /*in*/
  mpz_ptr OP2; /*in*/
  mpfr_rnd_t RND; /*in*/
  int _res;
  value _vres;

  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  camlidl_ml2c_mpfr_mpfr_ptr(_v_ROP, &ROP, _ctx);
  camlidl_ml2c_mpfr_mpfr_ptr(_v_OP1, &OP1, _ctx);
  camlidl_ml2c_mpz_mpz_ptr(_v_OP2, &OP2, _ctx);
  camlidl_ml2c_mpfr_mpfr_rnd_t(_v_RND, &RND, _ctx);
  _res = mpfr_sub_z(ROP, OP1, OP2, RND);
  _vres = Val_int(_res);
  camlidl_free(_ctx);
  return _vres;
}

value camlidl_mpfr_mpfr_sub_q(
	value _v_ROP,
	value _v_OP1,
	value _v_OP2,
	value _v_RND)
{
  mpfr_ptr ROP; /*in*/
  mpfr_ptr OP1; /*in*/
  mpq_ptr OP2; /*in*/
  mpfr_rnd_t RND; /*in*/
  int _res;
  value _vres;

  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  camlidl_ml2c_mpfr_mpfr_ptr(_v_ROP, &ROP, _ctx);
  camlidl_ml2c_mpfr_mpfr_ptr(_v_OP1, &OP1, _ctx);
  camlidl_ml2c_mpq_mpq_ptr(_v_OP2, &OP2, _ctx);
  camlidl_ml2c_mpfr_mpfr_rnd_t(_v_RND, &RND, _ctx);
  _res = mpfr_sub_q(ROP, OP1, OP2, RND);
  _vres = Val_int(_res);
  camlidl_free(_ctx);
  return _vres;
}

value camlidl_mpfr_mpfr_mul(
	value _v_ROP,
	value _v_OP1,
	value _v_OP2,
	value _v_RND)
{
  mpfr_ptr ROP; /*in*/
  mpfr_ptr OP1; /*in*/
  mpfr_ptr OP2; /*in*/
  mpfr_rnd_t RND; /*in*/
  int _res;
  value _vres;

  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  camlidl_ml2c_mpfr_mpfr_ptr(_v_ROP, &ROP, _ctx);
  camlidl_ml2c_mpfr_mpfr_ptr(_v_OP1, &OP1, _ctx);
  camlidl_ml2c_mpfr_mpfr_ptr(_v_OP2, &OP2, _ctx);
  camlidl_ml2c_mpfr_mpfr_rnd_t(_v_RND, &RND, _ctx);
  _res = mpfr_mul(ROP, OP1, OP2, RND);
  _vres = Val_int(_res);
  camlidl_free(_ctx);
  return _vres;
}

value camlidl_mpfr_mpfr_mul_ui(
	value _v_ROP,
	value _v_OP1,
	value _v_OP2,
	value _v_RND)
{
  mpfr_ptr ROP; /*in*/
  mpfr_ptr OP1; /*in*/
  unsigned long OP2; /*in*/
  mpfr_rnd_t RND; /*in*/
  int _res;
  value _vres;

  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  camlidl_ml2c_mpfr_mpfr_ptr(_v_ROP, &ROP, _ctx);
  camlidl_ml2c_mpfr_mpfr_ptr(_v_OP1, &OP1, _ctx);
  OP2 = Long_val(_v_OP2);
  camlidl_ml2c_mpfr_mpfr_rnd_t(_v_RND, &RND, _ctx);
  _res = mpfr_mul_ui(ROP, OP1, OP2, RND);
  _vres = Val_int(_res);
  camlidl_free(_ctx);
  return _vres;
}

value camlidl_mpfr_mpfr_mul_z(
	value _v_ROP,
	value _v_OP1,
	value _v_OP2,
	value _v_RND)
{
  mpfr_ptr ROP; /*in*/
  mpfr_ptr OP1; /*in*/
  mpz_ptr OP2; /*in*/
  mpfr_rnd_t RND; /*in*/
  int _res;
  value _vres;

  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  camlidl_ml2c_mpfr_mpfr_ptr(_v_ROP, &ROP, _ctx);
  camlidl_ml2c_mpfr_mpfr_ptr(_v_OP1, &OP1, _ctx);
  camlidl_ml2c_mpz_mpz_ptr(_v_OP2, &OP2, _ctx);
  camlidl_ml2c_mpfr_mpfr_rnd_t(_v_RND, &RND, _ctx);
  _res = mpfr_mul_z(ROP, OP1, OP2, RND);
  _vres = Val_int(_res);
  camlidl_free(_ctx);
  return _vres;
}

value camlidl_mpfr_mpfr_mul_q(
	value _v_ROP,
	value _v_OP1,
	value _v_OP2,
	value _v_RND)
{
  mpfr_ptr ROP; /*in*/
  mpfr_ptr OP1; /*in*/
  mpq_ptr OP2; /*in*/
  mpfr_rnd_t RND; /*in*/
  int _res;
  value _vres;

  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  camlidl_ml2c_mpfr_mpfr_ptr(_v_ROP, &ROP, _ctx);
  camlidl_ml2c_mpfr_mpfr_ptr(_v_OP1, &OP1, _ctx);
  camlidl_ml2c_mpq_mpq_ptr(_v_OP2, &OP2, _ctx);
  camlidl_ml2c_mpfr_mpfr_rnd_t(_v_RND, &RND, _ctx);
  _res = mpfr_mul_q(ROP, OP1, OP2, RND);
  _vres = Val_int(_res);
  camlidl_free(_ctx);
  return _vres;
}

value camlidl_mpfr_mpfr_mul_2ui(
	value _v_ROP,
	value _v_OP1,
	value _v_OP2,
	value _v_RND)
{
  mpfr_ptr ROP; /*in*/
  mpfr_ptr OP1; /*in*/
  unsigned long OP2; /*in*/
  mpfr_rnd_t RND; /*in*/
  int _res;
  value _vres;

  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  camlidl_ml2c_mpfr_mpfr_ptr(_v_ROP, &ROP, _ctx);
  camlidl_ml2c_mpfr_mpfr_ptr(_v_OP1, &OP1, _ctx);
  OP2 = Long_val(_v_OP2);
  camlidl_ml2c_mpfr_mpfr_rnd_t(_v_RND, &RND, _ctx);
  _res = mpfr_mul_2ui(ROP, OP1, OP2, RND);
  _vres = Val_int(_res);
  camlidl_free(_ctx);
  return _vres;
}

value camlidl_mpfr_mpfr_mul_2si(
	value _v_ROP,
	value _v_OP1,
	value _v_OP2,
	value _v_RND)
{
  mpfr_ptr ROP; /*in*/
  mpfr_ptr OP1; /*in*/
  long OP2; /*in*/
  mpfr_rnd_t RND; /*in*/
  int _res;
  value _vres;

  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  camlidl_ml2c_mpfr_mpfr_ptr(_v_ROP, &ROP, _ctx);
  camlidl_ml2c_mpfr_mpfr_ptr(_v_OP1, &OP1, _ctx);
  OP2 = Long_val(_v_OP2);
  camlidl_ml2c_mpfr_mpfr_rnd_t(_v_RND, &RND, _ctx);
  _res = mpfr_mul_2si(ROP, OP1, OP2, RND);
  _vres = Val_int(_res);
  camlidl_free(_ctx);
  return _vres;
}

value camlidl_mpfr_mpfr_mul_2exp(
	value _v_ROP,
	value _v_OP1,
	value _v_OP2,
	value _v_RND)
{
  mpfr_ptr ROP; /*in*/
  mpfr_ptr OP1; /*in*/
  unsigned long OP2; /*in*/
  mpfr_rnd_t RND; /*in*/
  int _res;
  value _vres;

  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  camlidl_ml2c_mpfr_mpfr_ptr(_v_ROP, &ROP, _ctx);
  camlidl_ml2c_mpfr_mpfr_ptr(_v_OP1, &OP1, _ctx);
  OP2 = Long_val(_v_OP2);
  camlidl_ml2c_mpfr_mpfr_rnd_t(_v_RND, &RND, _ctx);
  _res = mpfr_mul_2exp(ROP, OP1, OP2, RND);
  _vres = Val_int(_res);
  camlidl_free(_ctx);
  return _vres;
}

value camlidl_mpfr_mpfr_div(
	value _v_ROP,
	value _v_OP1,
	value _v_OP2,
	value _v_RND)
{
  mpfr_ptr ROP; /*in*/
  mpfr_ptr OP1; /*in*/
  mpfr_ptr OP2; /*in*/
  mpfr_rnd_t RND; /*in*/
  int _res;
  value _vres;

  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  camlidl_ml2c_mpfr_mpfr_ptr(_v_ROP, &ROP, _ctx);
  camlidl_ml2c_mpfr_mpfr_ptr(_v_OP1, &OP1, _ctx);
  camlidl_ml2c_mpfr_mpfr_ptr(_v_OP2, &OP2, _ctx);
  camlidl_ml2c_mpfr_mpfr_rnd_t(_v_RND, &RND, _ctx);
  _res = mpfr_div(ROP, OP1, OP2, RND);
  _vres = Val_int(_res);
  camlidl_free(_ctx);
  return _vres;
}

value camlidl_mpfr_mpfr_ui_div(
	value _v_ROP,
	value _v_OP1,
	value _v_OP2,
	value _v_RND)
{
  mpfr_ptr ROP; /*in*/
  unsigned long OP1; /*in*/
  mpfr_ptr OP2; /*in*/
  mpfr_rnd_t RND; /*in*/
  int _res;
  value _vres;

  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  camlidl_ml2c_mpfr_mpfr_ptr(_v_ROP, &ROP, _ctx);
  OP1 = Long_val(_v_OP1);
  camlidl_ml2c_mpfr_mpfr_ptr(_v_OP2, &OP2, _ctx);
  camlidl_ml2c_mpfr_mpfr_rnd_t(_v_RND, &RND, _ctx);
  _res = mpfr_ui_div(ROP, OP1, OP2, RND);
  _vres = Val_int(_res);
  camlidl_free(_ctx);
  return _vres;
}

value camlidl_mpfr_mpfr_div_ui(
	value _v_ROP,
	value _v_OP1,
	value _v_OP2,
	value _v_RND)
{
  mpfr_ptr ROP; /*in*/
  mpfr_ptr OP1; /*in*/
  unsigned long OP2; /*in*/
  mpfr_rnd_t RND; /*in*/
  int _res;
  value _vres;

  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  camlidl_ml2c_mpfr_mpfr_ptr(_v_ROP, &ROP, _ctx);
  camlidl_ml2c_mpfr_mpfr_ptr(_v_OP1, &OP1, _ctx);
  OP2 = Long_val(_v_OP2);
  camlidl_ml2c_mpfr_mpfr_rnd_t(_v_RND, &RND, _ctx);
  _res = mpfr_div_ui(ROP, OP1, OP2, RND);
  _vres = Val_int(_res);
  camlidl_free(_ctx);
  return _vres;
}

value camlidl_mpfr_mpfr_div_z(
	value _v_ROP,
	value _v_OP1,
	value _v_OP2,
	value _v_RND)
{
  mpfr_ptr ROP; /*in*/
  mpfr_ptr OP1; /*in*/
  mpz_ptr OP2; /*in*/
  mpfr_rnd_t RND; /*in*/
  int _res;
  value _vres;

  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  camlidl_ml2c_mpfr_mpfr_ptr(_v_ROP, &ROP, _ctx);
  camlidl_ml2c_mpfr_mpfr_ptr(_v_OP1, &OP1, _ctx);
  camlidl_ml2c_mpz_mpz_ptr(_v_OP2, &OP2, _ctx);
  camlidl_ml2c_mpfr_mpfr_rnd_t(_v_RND, &RND, _ctx);
  _res = mpfr_div_z(ROP, OP1, OP2, RND);
  _vres = Val_int(_res);
  camlidl_free(_ctx);
  return _vres;
}

value camlidl_mpfr_mpfr_div_q(
	value _v_ROP,
	value _v_OP1,
	value _v_OP2,
	value _v_RND)
{
  mpfr_ptr ROP; /*in*/
  mpfr_ptr OP1; /*in*/
  mpq_ptr OP2; /*in*/
  mpfr_rnd_t RND; /*in*/
  int _res;
  value _vres;

  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  camlidl_ml2c_mpfr_mpfr_ptr(_v_ROP, &ROP, _ctx);
  camlidl_ml2c_mpfr_mpfr_ptr(_v_OP1, &OP1, _ctx);
  camlidl_ml2c_mpq_mpq_ptr(_v_OP2, &OP2, _ctx);
  camlidl_ml2c_mpfr_mpfr_rnd_t(_v_RND, &RND, _ctx);
  _res = mpfr_div_q(ROP, OP1, OP2, RND);
  _vres = Val_int(_res);
  camlidl_free(_ctx);
  return _vres;
}

value camlidl_mpfr_mpfr_div_2ui(
	value _v_ROP,
	value _v_OP1,
	value _v_OP2,
	value _v_RND)
{
  mpfr_ptr ROP; /*in*/
  mpfr_ptr OP1; /*in*/
  unsigned long OP2; /*in*/
  mpfr_rnd_t RND; /*in*/
  int _res;
  value _vres;

  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  camlidl_ml2c_mpfr_mpfr_ptr(_v_ROP, &ROP, _ctx);
  camlidl_ml2c_mpfr_mpfr_ptr(_v_OP1, &OP1, _ctx);
  OP2 = Long_val(_v_OP2);
  camlidl_ml2c_mpfr_mpfr_rnd_t(_v_RND, &RND, _ctx);
  _res = mpfr_div_2ui(ROP, OP1, OP2, RND);
  _vres = Val_int(_res);
  camlidl_free(_ctx);
  return _vres;
}

value camlidl_mpfr_mpfr_div_2si(
	value _v_ROP,
	value _v_OP1,
	value _v_OP2,
	value _v_RND)
{
  mpfr_ptr ROP; /*in*/
  mpfr_ptr OP1; /*in*/
  long OP2; /*in*/
  mpfr_rnd_t RND; /*in*/
  int _res;
  value _vres;

  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  camlidl_ml2c_mpfr_mpfr_ptr(_v_ROP, &ROP, _ctx);
  camlidl_ml2c_mpfr_mpfr_ptr(_v_OP1, &OP1, _ctx);
  OP2 = Long_val(_v_OP2);
  camlidl_ml2c_mpfr_mpfr_rnd_t(_v_RND, &RND, _ctx);
  _res = mpfr_div_2si(ROP, OP1, OP2, RND);
  _vres = Val_int(_res);
  camlidl_free(_ctx);
  return _vres;
}

value camlidl_mpfr_mpfr_sqrt(
	value _v_ROP,
	value _v_OP,
	value _v_RND)
{
  mpfr_ptr ROP; /*in*/
  mpfr_ptr OP; /*in*/
  mpfr_rnd_t RND; /*in*/
  int _res;
  value _vres;

  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  camlidl_ml2c_mpfr_mpfr_ptr(_v_ROP, &ROP, _ctx);
  camlidl_ml2c_mpfr_mpfr_ptr(_v_OP, &OP, _ctx);
  camlidl_ml2c_mpfr_mpfr_rnd_t(_v_RND, &RND, _ctx);
  _res = mpfr_sqrt(ROP, OP, RND);
  _vres = Val_int(_res);
  camlidl_free(_ctx);
  return _vres;
}

value camlidl_mpfr_mpfr_sqrt_ui(
	value _v_ROP,
	value _v_OP,
	value _v_RND)
{
  mpfr_ptr ROP; /*in*/
  unsigned long OP; /*in*/
  mpfr_rnd_t RND; /*in*/
  int _res;
  value _vres;

  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  camlidl_ml2c_mpfr_mpfr_ptr(_v_ROP, &ROP, _ctx);
  OP = Long_val(_v_OP);
  camlidl_ml2c_mpfr_mpfr_rnd_t(_v_RND, &RND, _ctx);
  _res = mpfr_sqrt_ui(ROP, OP, RND);
  _vres = Val_int(_res);
  camlidl_free(_ctx);
  return _vres;
}

value camlidl_mpfr_mpfr_pow_ui(
	value _v_ROP,
	value _v_OP1,
	value _v_OP2,
	value _v_RND)
{
  mpfr_ptr ROP; /*in*/
  mpfr_ptr OP1; /*in*/
  unsigned long OP2; /*in*/
  mpfr_rnd_t RND; /*in*/
  int _res;
  value _vres;

  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  camlidl_ml2c_mpfr_mpfr_ptr(_v_ROP, &ROP, _ctx);
  camlidl_ml2c_mpfr_mpfr_ptr(_v_OP1, &OP1, _ctx);
  OP2 = Long_val(_v_OP2);
  camlidl_ml2c_mpfr_mpfr_rnd_t(_v_RND, &RND, _ctx);
  _res = mpfr_pow_ui(ROP, OP1, OP2, RND);
  _vres = Val_int(_res);
  camlidl_free(_ctx);
  return _vres;
}

value camlidl_mpfr_mpfr_pow_si(
	value _v_ROP,
	value _v_OP1,
	value _v_OP2,
	value _v_RND)
{
  mpfr_ptr ROP; /*in*/
  mpfr_ptr OP1; /*in*/
  long OP2; /*in*/
  mpfr_rnd_t RND; /*in*/
  int _res;
  value _vres;

  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  camlidl_ml2c_mpfr_mpfr_ptr(_v_ROP, &ROP, _ctx);
  camlidl_ml2c_mpfr_mpfr_ptr(_v_OP1, &OP1, _ctx);
  OP2 = Long_val(_v_OP2);
  camlidl_ml2c_mpfr_mpfr_rnd_t(_v_RND, &RND, _ctx);
  _res = mpfr_pow_si(ROP, OP1, OP2, RND);
  _vres = Val_int(_res);
  camlidl_free(_ctx);
  return _vres;
}

value camlidl_mpfr_mpfr_ui_pow_ui(
	value _v_ROP,
	value _v_OP1,
	value _v_OP2,
	value _v_RND)
{
  mpfr_ptr ROP; /*in*/
  unsigned long OP1; /*in*/
  unsigned long OP2; /*in*/
  mpfr_rnd_t RND; /*in*/
  int _res;
  value _vres;

  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  camlidl_ml2c_mpfr_mpfr_ptr(_v_ROP, &ROP, _ctx);
  OP1 = Long_val(_v_OP1);
  OP2 = Long_val(_v_OP2);
  camlidl_ml2c_mpfr_mpfr_rnd_t(_v_RND, &RND, _ctx);
  _res = mpfr_ui_pow_ui(ROP, OP1, OP2, RND);
  _vres = Val_int(_res);
  camlidl_free(_ctx);
  return _vres;
}

value camlidl_mpfr_mpfr_ui_pow(
	value _v_ROP,
	value _v_OP1,
	value _v_OP2,
	value _v_RND)
{
  mpfr_ptr ROP; /*in*/
  unsigned long OP1; /*in*/
  mpfr_ptr OP2; /*in*/
  mpfr_rnd_t RND; /*in*/
  int _res;
  value _vres;

  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  camlidl_ml2c_mpfr_mpfr_ptr(_v_ROP, &ROP, _ctx);
  OP1 = Long_val(_v_OP1);
  camlidl_ml2c_mpfr_mpfr_ptr(_v_OP2, &OP2, _ctx);
  camlidl_ml2c_mpfr_mpfr_rnd_t(_v_RND, &RND, _ctx);
  _res = mpfr_ui_pow(ROP, OP1, OP2, RND);
  _vres = Val_int(_res);
  camlidl_free(_ctx);
  return _vres;
}

value camlidl_mpfr_mpfr_pow(
	value _v_ROP,
	value _v_OP1,
	value _v_OP2,
	value _v_RND)
{
  mpfr_ptr ROP; /*in*/
  mpfr_ptr OP1; /*in*/
  mpfr_ptr OP2; /*in*/
  mpfr_rnd_t RND; /*in*/
  int _res;
  value _vres;

  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  camlidl_ml2c_mpfr_mpfr_ptr(_v_ROP, &ROP, _ctx);
  camlidl_ml2c_mpfr_mpfr_ptr(_v_OP1, &OP1, _ctx);
  camlidl_ml2c_mpfr_mpfr_ptr(_v_OP2, &OP2, _ctx);
  camlidl_ml2c_mpfr_mpfr_rnd_t(_v_RND, &RND, _ctx);
  _res = mpfr_pow(ROP, OP1, OP2, RND);
  _vres = Val_int(_res);
  camlidl_free(_ctx);
  return _vres;
}

value camlidl_mpfr_mpfr_neg(
	value _v_ROP,
	value _v_OP,
	value _v_RND)
{
  mpfr_ptr ROP; /*in*/
  mpfr_ptr OP; /*in*/
  mpfr_rnd_t RND; /*in*/
  int _res;
  value _vres;

  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  camlidl_ml2c_mpfr_mpfr_ptr(_v_ROP, &ROP, _ctx);
  camlidl_ml2c_mpfr_mpfr_ptr(_v_OP, &OP, _ctx);
  camlidl_ml2c_mpfr_mpfr_rnd_t(_v_RND, &RND, _ctx);
  _res = mpfr_neg(ROP, OP, RND);
  _vres = Val_int(_res);
  camlidl_free(_ctx);
  return _vres;
}

value camlidl_mpfr_mpfr_abs(
	value _v_ROP,
	value _v_OP,
	value _v_RND)
{
  mpfr_ptr ROP; /*in*/
  mpfr_ptr OP; /*in*/
  mpfr_rnd_t RND; /*in*/
  int _res;
  value _vres;

  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  camlidl_ml2c_mpfr_mpfr_ptr(_v_ROP, &ROP, _ctx);
  camlidl_ml2c_mpfr_mpfr_ptr(_v_OP, &OP, _ctx);
  camlidl_ml2c_mpfr_mpfr_rnd_t(_v_RND, &RND, _ctx);
  _res = mpfr_abs(ROP, OP, RND);
  _vres = Val_int(_res);
  camlidl_free(_ctx);
  return _vres;
}

value camlidl_mpfr_mpfr_cmp(
	value _v_OP1,
	value _v_OP2)
{
  mpfr_ptr OP1; /*in*/
  mpfr_ptr OP2; /*in*/
  int _res;
  value _vres;

  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  camlidl_ml2c_mpfr_mpfr_ptr(_v_OP1, &OP1, _ctx);
  camlidl_ml2c_mpfr_mpfr_ptr(_v_OP2, &OP2, _ctx);
  _res = mpfr_cmp(OP1, OP2);
  _vres = Val_int(_res);
  camlidl_free(_ctx);
  return _vres;
}

value camlidl_mpfr_mpfr_cmp_si(
	value _v_OP1,
	value _v_OP2)
{
  mpfr_ptr OP1; /*in*/
  long OP2; /*in*/
  int _res;
  value _vres;

  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  camlidl_ml2c_mpfr_mpfr_ptr(_v_OP1, &OP1, _ctx);
  OP2 = Long_val(_v_OP2);
  _res = mpfr_cmp_si(OP1, OP2);
  _vres = Val_int(_res);
  camlidl_free(_ctx);
  return _vres;
}

value camlidl_mpfr_mpfr_cmp_si_2exp(
	value _v_OP1,
	value _v_OP2,
	value _v_E)
{
  mpfr_ptr OP1; /*in*/
  long OP2; /*in*/
  int E; /*in*/
  int _res;
  value _vres;

  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  camlidl_ml2c_mpfr_mpfr_ptr(_v_OP1, &OP1, _ctx);
  OP2 = Long_val(_v_OP2);
  E = Int_val(_v_E);
  _res = mpfr_cmp_si_2exp(OP1, OP2, E);
  _vres = Val_int(_res);
  camlidl_free(_ctx);
  return _vres;
}

value camlidl_mpfr_mpfr_sgn(
	value _v_OP)
{
  mpfr_ptr OP; /*in*/
  int _res;
  value _vres;

  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  camlidl_ml2c_mpfr_mpfr_ptr(_v_OP, &OP, _ctx);
  _res = mpfr_sgn(OP);
  _vres = Val_int(_res);
  camlidl_free(_ctx);
  return _vres;
}

value camlidl_mpfr_mpfr__equal(
	value _v_OP1,
	value _v_OP2,
	value _v_OP3)
{
  mpfr_ptr OP1; /*in*/
  mpfr_ptr OP2; /*in*/
  unsigned long OP3; /*in*/
  int _res;
  value _vres;

  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  camlidl_ml2c_mpfr_mpfr_ptr(_v_OP1, &OP1, _ctx);
  camlidl_ml2c_mpfr_mpfr_ptr(_v_OP2, &OP2, _ctx);
  OP3 = Long_val(_v_OP3);
  /* begin user-supplied calling sequence */
_res=mpfr_eq(OP1,OP2,OP3);
  /* end user-supplied calling sequence */
  _vres = Val_int(_res);
  camlidl_free(_ctx);
  return _vres;
}

value camlidl_mpfr_mpfr_nan_p(
	value _v_OP)
{
  mpfr_ptr OP; /*in*/
  int _res;
  value _vres;

  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  camlidl_ml2c_mpfr_mpfr_ptr(_v_OP, &OP, _ctx);
  _res = mpfr_nan_p(OP);
  _vres = Val_int(_res);
  camlidl_free(_ctx);
  return _vres;
}

value camlidl_mpfr_mpfr_inf_p(
	value _v_OP)
{
  mpfr_ptr OP; /*in*/
  int _res;
  value _vres;

  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  camlidl_ml2c_mpfr_mpfr_ptr(_v_OP, &OP, _ctx);
  _res = mpfr_inf_p(OP);
  _vres = Val_int(_res);
  camlidl_free(_ctx);
  return _vres;
}

value camlidl_mpfr_mpfr_number_p(
	value _v_OP)
{
  mpfr_ptr OP; /*in*/
  int _res;
  value _vres;

  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  camlidl_ml2c_mpfr_mpfr_ptr(_v_OP, &OP, _ctx);
  _res = mpfr_number_p(OP);
  _vres = Val_int(_res);
  camlidl_free(_ctx);
  return _vres;
}

value camlidl_mpfr_mpfr_reldiff(
	value _v_ROP,
	value _v_OP1,
	value _v_OP2,
	value _v_RND)
{
  mpfr_ptr ROP; /*in*/
  mpfr_ptr OP1; /*in*/
  mpfr_ptr OP2; /*in*/
  mpfr_rnd_t RND; /*in*/
  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  camlidl_ml2c_mpfr_mpfr_ptr(_v_ROP, &ROP, _ctx);
  camlidl_ml2c_mpfr_mpfr_ptr(_v_OP1, &OP1, _ctx);
  camlidl_ml2c_mpfr_mpfr_ptr(_v_OP2, &OP2, _ctx);
  camlidl_ml2c_mpfr_mpfr_rnd_t(_v_RND, &RND, _ctx);
  mpfr_reldiff(ROP, OP1, OP2, RND);
  camlidl_free(_ctx);
  return Val_unit;
}

value camlidl_mpfr_mpfr_log(
	value _v_ROP,
	value _v_OP,
	value _v_RND)
{
  mpfr_ptr ROP; /*in*/
  mpfr_ptr OP; /*in*/
  mpfr_rnd_t RND; /*in*/
  int _res;
  value _vres;

  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  camlidl_ml2c_mpfr_mpfr_ptr(_v_ROP, &ROP, _ctx);
  camlidl_ml2c_mpfr_mpfr_ptr(_v_OP, &OP, _ctx);
  camlidl_ml2c_mpfr_mpfr_rnd_t(_v_RND, &RND, _ctx);
  _res = mpfr_log(ROP, OP, RND);
  _vres = Val_int(_res);
  camlidl_free(_ctx);
  return _vres;
}

value camlidl_mpfr_mpfr_log2(
	value _v_ROP,
	value _v_OP,
	value _v_RND)
{
  mpfr_ptr ROP; /*in*/
  mpfr_ptr OP; /*in*/
  mpfr_rnd_t RND; /*in*/
  int _res;
  value _vres;

  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  camlidl_ml2c_mpfr_mpfr_ptr(_v_ROP, &ROP, _ctx);
  camlidl_ml2c_mpfr_mpfr_ptr(_v_OP, &OP, _ctx);
  camlidl_ml2c_mpfr_mpfr_rnd_t(_v_RND, &RND, _ctx);
  _res = mpfr_log2(ROP, OP, RND);
  _vres = Val_int(_res);
  camlidl_free(_ctx);
  return _vres;
}

value camlidl_mpfr_mpfr_log10(
	value _v_ROP,
	value _v_OP,
	value _v_RND)
{
  mpfr_ptr ROP; /*in*/
  mpfr_ptr OP; /*in*/
  mpfr_rnd_t RND; /*in*/
  int _res;
  value _vres;

  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  camlidl_ml2c_mpfr_mpfr_ptr(_v_ROP, &ROP, _ctx);
  camlidl_ml2c_mpfr_mpfr_ptr(_v_OP, &OP, _ctx);
  camlidl_ml2c_mpfr_mpfr_rnd_t(_v_RND, &RND, _ctx);
  _res = mpfr_log10(ROP, OP, RND);
  _vres = Val_int(_res);
  camlidl_free(_ctx);
  return _vres;
}

value camlidl_mpfr_mpfr_exp(
	value _v_ROP,
	value _v_OP,
	value _v_RND)
{
  mpfr_ptr ROP; /*in*/
  mpfr_ptr OP; /*in*/
  mpfr_rnd_t RND; /*in*/
  int _res;
  value _vres;

  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  camlidl_ml2c_mpfr_mpfr_ptr(_v_ROP, &ROP, _ctx);
  camlidl_ml2c_mpfr_mpfr_ptr(_v_OP, &OP, _ctx);
  camlidl_ml2c_mpfr_mpfr_rnd_t(_v_RND, &RND, _ctx);
  _res = mpfr_exp(ROP, OP, RND);
  _vres = Val_int(_res);
  camlidl_free(_ctx);
  return _vres;
}

value camlidl_mpfr_mpfr_exp2(
	value _v_ROP,
	value _v_OP,
	value _v_RND)
{
  mpfr_ptr ROP; /*in*/
  mpfr_ptr OP; /*in*/
  mpfr_rnd_t RND; /*in*/
  int _res;
  value _vres;

  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  camlidl_ml2c_mpfr_mpfr_ptr(_v_ROP, &ROP, _ctx);
  camlidl_ml2c_mpfr_mpfr_ptr(_v_OP, &OP, _ctx);
  camlidl_ml2c_mpfr_mpfr_rnd_t(_v_RND, &RND, _ctx);
  _res = mpfr_exp2(ROP, OP, RND);
  _vres = Val_int(_res);
  camlidl_free(_ctx);
  return _vres;
}

value camlidl_mpfr_mpfr_exp10(
	value _v_ROP,
	value _v_OP,
	value _v_RND)
{
  mpfr_ptr ROP; /*in*/
  mpfr_ptr OP; /*in*/
  mpfr_rnd_t RND; /*in*/
  int _res;
  value _vres;

  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  camlidl_ml2c_mpfr_mpfr_ptr(_v_ROP, &ROP, _ctx);
  camlidl_ml2c_mpfr_mpfr_ptr(_v_OP, &OP, _ctx);
  camlidl_ml2c_mpfr_mpfr_rnd_t(_v_RND, &RND, _ctx);
  _res = mpfr_exp10(ROP, OP, RND);
  _vres = Val_int(_res);
  camlidl_free(_ctx);
  return _vres;
}

value camlidl_mpfr_mpfr_cos(
	value _v_COP,
	value _v_OP,
	value _v_RND)
{
  mpfr_ptr COP; /*in*/
  mpfr_ptr OP; /*in*/
  mpfr_rnd_t RND; /*in*/
  int _res;
  value _vres;

  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  camlidl_ml2c_mpfr_mpfr_ptr(_v_COP, &COP, _ctx);
  camlidl_ml2c_mpfr_mpfr_ptr(_v_OP, &OP, _ctx);
  camlidl_ml2c_mpfr_mpfr_rnd_t(_v_RND, &RND, _ctx);
  _res = mpfr_cos(COP, OP, RND);
  _vres = Val_int(_res);
  camlidl_free(_ctx);
  return _vres;
}

value camlidl_mpfr_mpfr_sin(
	value _v_SOP,
	value _v_OP,
	value _v_RND)
{
  mpfr_ptr SOP; /*in*/
  mpfr_ptr OP; /*in*/
  mpfr_rnd_t RND; /*in*/
  int _res;
  value _vres;

  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  camlidl_ml2c_mpfr_mpfr_ptr(_v_SOP, &SOP, _ctx);
  camlidl_ml2c_mpfr_mpfr_ptr(_v_OP, &OP, _ctx);
  camlidl_ml2c_mpfr_mpfr_rnd_t(_v_RND, &RND, _ctx);
  _res = mpfr_sin(SOP, OP, RND);
  _vres = Val_int(_res);
  camlidl_free(_ctx);
  return _vres;
}

value camlidl_mpfr_mpfr_tan(
	value _v_TOP,
	value _v_OP,
	value _v_RND)
{
  mpfr_ptr TOP; /*in*/
  mpfr_ptr OP; /*in*/
  mpfr_rnd_t RND; /*in*/
  int _res;
  value _vres;

  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  camlidl_ml2c_mpfr_mpfr_ptr(_v_TOP, &TOP, _ctx);
  camlidl_ml2c_mpfr_mpfr_ptr(_v_OP, &OP, _ctx);
  camlidl_ml2c_mpfr_mpfr_rnd_t(_v_RND, &RND, _ctx);
  _res = mpfr_tan(TOP, OP, RND);
  _vres = Val_int(_res);
  camlidl_free(_ctx);
  return _vres;
}

value camlidl_mpfr_mpfr_sec(
	value _v_COP,
	value _v_OP,
	value _v_RND)
{
  mpfr_ptr COP; /*in*/
  mpfr_ptr OP; /*in*/
  mpfr_rnd_t RND; /*in*/
  int _res;
  value _vres;

  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  camlidl_ml2c_mpfr_mpfr_ptr(_v_COP, &COP, _ctx);
  camlidl_ml2c_mpfr_mpfr_ptr(_v_OP, &OP, _ctx);
  camlidl_ml2c_mpfr_mpfr_rnd_t(_v_RND, &RND, _ctx);
  _res = mpfr_sec(COP, OP, RND);
  _vres = Val_int(_res);
  camlidl_free(_ctx);
  return _vres;
}

value camlidl_mpfr_mpfr_csc(
	value _v_SOP,
	value _v_OP,
	value _v_RND)
{
  mpfr_ptr SOP; /*in*/
  mpfr_ptr OP; /*in*/
  mpfr_rnd_t RND; /*in*/
  int _res;
  value _vres;

  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  camlidl_ml2c_mpfr_mpfr_ptr(_v_SOP, &SOP, _ctx);
  camlidl_ml2c_mpfr_mpfr_ptr(_v_OP, &OP, _ctx);
  camlidl_ml2c_mpfr_mpfr_rnd_t(_v_RND, &RND, _ctx);
  _res = mpfr_csc(SOP, OP, RND);
  _vres = Val_int(_res);
  camlidl_free(_ctx);
  return _vres;
}

value camlidl_mpfr_mpfr_cot(
	value _v_TOP,
	value _v_OP,
	value _v_RND)
{
  mpfr_ptr TOP; /*in*/
  mpfr_ptr OP; /*in*/
  mpfr_rnd_t RND; /*in*/
  int _res;
  value _vres;

  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  camlidl_ml2c_mpfr_mpfr_ptr(_v_TOP, &TOP, _ctx);
  camlidl_ml2c_mpfr_mpfr_ptr(_v_OP, &OP, _ctx);
  camlidl_ml2c_mpfr_mpfr_rnd_t(_v_RND, &RND, _ctx);
  _res = mpfr_cot(TOP, OP, RND);
  _vres = Val_int(_res);
  camlidl_free(_ctx);
  return _vres;
}

value camlidl_mpfr_mpfr_sin_cos(
	value _v_SOP,
	value _v_COP,
	value _v_OP,
	value _v_RND)
{
  mpfr_ptr SOP; /*in*/
  mpfr_ptr COP; /*in*/
  mpfr_ptr OP; /*in*/
  mpfr_rnd_t RND; /*in*/
  int _res;
  value _vres;

  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  camlidl_ml2c_mpfr_mpfr_ptr(_v_SOP, &SOP, _ctx);
  camlidl_ml2c_mpfr_mpfr_ptr(_v_COP, &COP, _ctx);
  camlidl_ml2c_mpfr_mpfr_ptr(_v_OP, &OP, _ctx);
  camlidl_ml2c_mpfr_mpfr_rnd_t(_v_RND, &RND, _ctx);
  _res = mpfr_sin_cos(SOP, COP, OP, RND);
  _vres = Val_int(_res);
  camlidl_free(_ctx);
  return _vres;
}

value camlidl_mpfr_mpfr_acos(
	value _v_ROP,
	value _v_OP,
	value _v_RND)
{
  mpfr_ptr ROP; /*in*/
  mpfr_ptr OP; /*in*/
  mpfr_rnd_t RND; /*in*/
  int _res;
  value _vres;

  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  camlidl_ml2c_mpfr_mpfr_ptr(_v_ROP, &ROP, _ctx);
  camlidl_ml2c_mpfr_mpfr_ptr(_v_OP, &OP, _ctx);
  camlidl_ml2c_mpfr_mpfr_rnd_t(_v_RND, &RND, _ctx);
  _res = mpfr_acos(ROP, OP, RND);
  _vres = Val_int(_res);
  camlidl_free(_ctx);
  return _vres;
}

value camlidl_mpfr_mpfr_asin(
	value _v_ROP,
	value _v_OP,
	value _v_RND)
{
  mpfr_ptr ROP; /*in*/
  mpfr_ptr OP; /*in*/
  mpfr_rnd_t RND; /*in*/
  int _res;
  value _vres;

  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  camlidl_ml2c_mpfr_mpfr_ptr(_v_ROP, &ROP, _ctx);
  camlidl_ml2c_mpfr_mpfr_ptr(_v_OP, &OP, _ctx);
  camlidl_ml2c_mpfr_mpfr_rnd_t(_v_RND, &RND, _ctx);
  _res = mpfr_asin(ROP, OP, RND);
  _vres = Val_int(_res);
  camlidl_free(_ctx);
  return _vres;
}

value camlidl_mpfr_mpfr_atan(
	value _v_ROP,
	value _v_OP,
	value _v_RND)
{
  mpfr_ptr ROP; /*in*/
  mpfr_ptr OP; /*in*/
  mpfr_rnd_t RND; /*in*/
  int _res;
  value _vres;

  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  camlidl_ml2c_mpfr_mpfr_ptr(_v_ROP, &ROP, _ctx);
  camlidl_ml2c_mpfr_mpfr_ptr(_v_OP, &OP, _ctx);
  camlidl_ml2c_mpfr_mpfr_rnd_t(_v_RND, &RND, _ctx);
  _res = mpfr_atan(ROP, OP, RND);
  _vres = Val_int(_res);
  camlidl_free(_ctx);
  return _vres;
}

value camlidl_mpfr_mpfr_atan2(
	value _v_ROP,
	value _v_Y,
	value _v_X,
	value _v_RND)
{
  mpfr_ptr ROP; /*in*/
  mpfr_ptr Y; /*in*/
  mpfr_ptr X; /*in*/
  mpfr_rnd_t RND; /*in*/
  int _res;
  value _vres;

  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  camlidl_ml2c_mpfr_mpfr_ptr(_v_ROP, &ROP, _ctx);
  camlidl_ml2c_mpfr_mpfr_ptr(_v_Y, &Y, _ctx);
  camlidl_ml2c_mpfr_mpfr_ptr(_v_X, &X, _ctx);
  camlidl_ml2c_mpfr_mpfr_rnd_t(_v_RND, &RND, _ctx);
  _res = mpfr_atan2(ROP, Y, X, RND);
  _vres = Val_int(_res);
  camlidl_free(_ctx);
  return _vres;
}

value camlidl_mpfr_mpfr_cosh(
	value _v_COP,
	value _v_OP,
	value _v_RND)
{
  mpfr_ptr COP; /*in*/
  mpfr_ptr OP; /*in*/
  mpfr_rnd_t RND; /*in*/
  int _res;
  value _vres;

  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  camlidl_ml2c_mpfr_mpfr_ptr(_v_COP, &COP, _ctx);
  camlidl_ml2c_mpfr_mpfr_ptr(_v_OP, &OP, _ctx);
  camlidl_ml2c_mpfr_mpfr_rnd_t(_v_RND, &RND, _ctx);
  _res = mpfr_cosh(COP, OP, RND);
  _vres = Val_int(_res);
  camlidl_free(_ctx);
  return _vres;
}

value camlidl_mpfr_mpfr_sinh(
	value _v_SOP,
	value _v_OP,
	value _v_RND)
{
  mpfr_ptr SOP; /*in*/
  mpfr_ptr OP; /*in*/
  mpfr_rnd_t RND; /*in*/
  int _res;
  value _vres;

  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  camlidl_ml2c_mpfr_mpfr_ptr(_v_SOP, &SOP, _ctx);
  camlidl_ml2c_mpfr_mpfr_ptr(_v_OP, &OP, _ctx);
  camlidl_ml2c_mpfr_mpfr_rnd_t(_v_RND, &RND, _ctx);
  _res = mpfr_sinh(SOP, OP, RND);
  _vres = Val_int(_res);
  camlidl_free(_ctx);
  return _vres;
}

value camlidl_mpfr_mpfr_tanh(
	value _v_TOP,
	value _v_OP,
	value _v_RND)
{
  mpfr_ptr TOP; /*in*/
  mpfr_ptr OP; /*in*/
  mpfr_rnd_t RND; /*in*/
  int _res;
  value _vres;

  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  camlidl_ml2c_mpfr_mpfr_ptr(_v_TOP, &TOP, _ctx);
  camlidl_ml2c_mpfr_mpfr_ptr(_v_OP, &OP, _ctx);
  camlidl_ml2c_mpfr_mpfr_rnd_t(_v_RND, &RND, _ctx);
  _res = mpfr_tanh(TOP, OP, RND);
  _vres = Val_int(_res);
  camlidl_free(_ctx);
  return _vres;
}

value camlidl_mpfr_mpfr_sech(
	value _v_COP,
	value _v_OP,
	value _v_RND)
{
  mpfr_ptr COP; /*in*/
  mpfr_ptr OP; /*in*/
  mpfr_rnd_t RND; /*in*/
  int _res;
  value _vres;

  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  camlidl_ml2c_mpfr_mpfr_ptr(_v_COP, &COP, _ctx);
  camlidl_ml2c_mpfr_mpfr_ptr(_v_OP, &OP, _ctx);
  camlidl_ml2c_mpfr_mpfr_rnd_t(_v_RND, &RND, _ctx);
  _res = mpfr_sech(COP, OP, RND);
  _vres = Val_int(_res);
  camlidl_free(_ctx);
  return _vres;
}

value camlidl_mpfr_mpfr_csch(
	value _v_SOP,
	value _v_OP,
	value _v_RND)
{
  mpfr_ptr SOP; /*in*/
  mpfr_ptr OP; /*in*/
  mpfr_rnd_t RND; /*in*/
  int _res;
  value _vres;

  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  camlidl_ml2c_mpfr_mpfr_ptr(_v_SOP, &SOP, _ctx);
  camlidl_ml2c_mpfr_mpfr_ptr(_v_OP, &OP, _ctx);
  camlidl_ml2c_mpfr_mpfr_rnd_t(_v_RND, &RND, _ctx);
  _res = mpfr_csch(SOP, OP, RND);
  _vres = Val_int(_res);
  camlidl_free(_ctx);
  return _vres;
}

value camlidl_mpfr_mpfr_coth(
	value _v_TOP,
	value _v_OP,
	value _v_RND)
{
  mpfr_ptr TOP; /*in*/
  mpfr_ptr OP; /*in*/
  mpfr_rnd_t RND; /*in*/
  int _res;
  value _vres;

  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  camlidl_ml2c_mpfr_mpfr_ptr(_v_TOP, &TOP, _ctx);
  camlidl_ml2c_mpfr_mpfr_ptr(_v_OP, &OP, _ctx);
  camlidl_ml2c_mpfr_mpfr_rnd_t(_v_RND, &RND, _ctx);
  _res = mpfr_coth(TOP, OP, RND);
  _vres = Val_int(_res);
  camlidl_free(_ctx);
  return _vres;
}

value camlidl_mpfr_mpfr_acosh(
	value _v_ROP,
	value _v_OP,
	value _v_RND)
{
  mpfr_ptr ROP; /*in*/
  mpfr_ptr OP; /*in*/
  mpfr_rnd_t RND; /*in*/
  int _res;
  value _vres;

  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  camlidl_ml2c_mpfr_mpfr_ptr(_v_ROP, &ROP, _ctx);
  camlidl_ml2c_mpfr_mpfr_ptr(_v_OP, &OP, _ctx);
  camlidl_ml2c_mpfr_mpfr_rnd_t(_v_RND, &RND, _ctx);
  _res = mpfr_acosh(ROP, OP, RND);
  _vres = Val_int(_res);
  camlidl_free(_ctx);
  return _vres;
}

value camlidl_mpfr_mpfr_asinh(
	value _v_ROP,
	value _v_OP,
	value _v_RND)
{
  mpfr_ptr ROP; /*in*/
  mpfr_ptr OP; /*in*/
  mpfr_rnd_t RND; /*in*/
  int _res;
  value _vres;

  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  camlidl_ml2c_mpfr_mpfr_ptr(_v_ROP, &ROP, _ctx);
  camlidl_ml2c_mpfr_mpfr_ptr(_v_OP, &OP, _ctx);
  camlidl_ml2c_mpfr_mpfr_rnd_t(_v_RND, &RND, _ctx);
  _res = mpfr_asinh(ROP, OP, RND);
  _vres = Val_int(_res);
  camlidl_free(_ctx);
  return _vres;
}

value camlidl_mpfr_mpfr_atanh(
	value _v_ROP,
	value _v_OP,
	value _v_RND)
{
  mpfr_ptr ROP; /*in*/
  mpfr_ptr OP; /*in*/
  mpfr_rnd_t RND; /*in*/
  int _res;
  value _vres;

  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  camlidl_ml2c_mpfr_mpfr_ptr(_v_ROP, &ROP, _ctx);
  camlidl_ml2c_mpfr_mpfr_ptr(_v_OP, &OP, _ctx);
  camlidl_ml2c_mpfr_mpfr_rnd_t(_v_RND, &RND, _ctx);
  _res = mpfr_atanh(ROP, OP, RND);
  _vres = Val_int(_res);
  camlidl_free(_ctx);
  return _vres;
}

value camlidl_mpfr_mpfr_fac_ui(
	value _v_ROP,
	value _v_OP,
	value _v_RND)
{
  mpfr_ptr ROP; /*in*/
  unsigned long OP; /*in*/
  mpfr_rnd_t RND; /*in*/
  int _res;
  value _vres;

  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  camlidl_ml2c_mpfr_mpfr_ptr(_v_ROP, &ROP, _ctx);
  OP = Long_val(_v_OP);
  camlidl_ml2c_mpfr_mpfr_rnd_t(_v_RND, &RND, _ctx);
  _res = mpfr_fac_ui(ROP, OP, RND);
  _vres = Val_int(_res);
  camlidl_free(_ctx);
  return _vres;
}

value camlidl_mpfr_mpfr_log1p(
	value _v_ROP,
	value _v_OP,
	value _v_RND)
{
  mpfr_ptr ROP; /*in*/
  mpfr_ptr OP; /*in*/
  mpfr_rnd_t RND; /*in*/
  int _res;
  value _vres;

  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  camlidl_ml2c_mpfr_mpfr_ptr(_v_ROP, &ROP, _ctx);
  camlidl_ml2c_mpfr_mpfr_ptr(_v_OP, &OP, _ctx);
  camlidl_ml2c_mpfr_mpfr_rnd_t(_v_RND, &RND, _ctx);
  _res = mpfr_log1p(ROP, OP, RND);
  _vres = Val_int(_res);
  camlidl_free(_ctx);
  return _vres;
}

value camlidl_mpfr_mpfr_expm1(
	value _v_ROP,
	value _v_OP,
	value _v_RND)
{
  mpfr_ptr ROP; /*in*/
  mpfr_ptr OP; /*in*/
  mpfr_rnd_t RND; /*in*/
  int _res;
  value _vres;

  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  camlidl_ml2c_mpfr_mpfr_ptr(_v_ROP, &ROP, _ctx);
  camlidl_ml2c_mpfr_mpfr_ptr(_v_OP, &OP, _ctx);
  camlidl_ml2c_mpfr_mpfr_rnd_t(_v_RND, &RND, _ctx);
  _res = mpfr_expm1(ROP, OP, RND);
  _vres = Val_int(_res);
  camlidl_free(_ctx);
  return _vres;
}

value camlidl_mpfr_mpfr_eint(
	value _v_Y,
	value _v_X,
	value _v_RND)
{
  mpfr_ptr Y; /*in*/
  mpfr_ptr X; /*in*/
  mpfr_rnd_t RND; /*in*/
  int _res;
  value _vres;

  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  camlidl_ml2c_mpfr_mpfr_ptr(_v_Y, &Y, _ctx);
  camlidl_ml2c_mpfr_mpfr_ptr(_v_X, &X, _ctx);
  camlidl_ml2c_mpfr_mpfr_rnd_t(_v_RND, &RND, _ctx);
  _res = mpfr_eint(Y, X, RND);
  _vres = Val_int(_res);
  camlidl_free(_ctx);
  return _vres;
}

value camlidl_mpfr_mpfr_gamma(
	value _v_ROP,
	value _v_OP,
	value _v_RND)
{
  mpfr_ptr ROP; /*in*/
  mpfr_ptr OP; /*in*/
  mpfr_rnd_t RND; /*in*/
  int _res;
  value _vres;

  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  camlidl_ml2c_mpfr_mpfr_ptr(_v_ROP, &ROP, _ctx);
  camlidl_ml2c_mpfr_mpfr_ptr(_v_OP, &OP, _ctx);
  camlidl_ml2c_mpfr_mpfr_rnd_t(_v_RND, &RND, _ctx);
  _res = mpfr_gamma(ROP, OP, RND);
  _vres = Val_int(_res);
  camlidl_free(_ctx);
  return _vres;
}

value camlidl_mpfr_mpfr_lngamma(
	value _v_ROP,
	value _v_OP,
	value _v_RND)
{
  mpfr_ptr ROP; /*in*/
  mpfr_ptr OP; /*in*/
  mpfr_rnd_t RND; /*in*/
  int _res;
  value _vres;

  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  camlidl_ml2c_mpfr_mpfr_ptr(_v_ROP, &ROP, _ctx);
  camlidl_ml2c_mpfr_mpfr_ptr(_v_OP, &OP, _ctx);
  camlidl_ml2c_mpfr_mpfr_rnd_t(_v_RND, &RND, _ctx);
  _res = mpfr_lngamma(ROP, OP, RND);
  _vres = Val_int(_res);
  camlidl_free(_ctx);
  return _vres;
}

value camlidl_mpfr_mpfr_zeta(
	value _v_ROP,
	value _v_OP,
	value _v_RND)
{
  mpfr_ptr ROP; /*in*/
  mpfr_ptr OP; /*in*/
  mpfr_rnd_t RND; /*in*/
  int _res;
  value _vres;

  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  camlidl_ml2c_mpfr_mpfr_ptr(_v_ROP, &ROP, _ctx);
  camlidl_ml2c_mpfr_mpfr_ptr(_v_OP, &OP, _ctx);
  camlidl_ml2c_mpfr_mpfr_rnd_t(_v_RND, &RND, _ctx);
  _res = mpfr_zeta(ROP, OP, RND);
  _vres = Val_int(_res);
  camlidl_free(_ctx);
  return _vres;
}

value camlidl_mpfr_mpfr_erf(
	value _v_ROP,
	value _v_OP,
	value _v_RND)
{
  mpfr_ptr ROP; /*in*/
  mpfr_ptr OP; /*in*/
  mpfr_rnd_t RND; /*in*/
  int _res;
  value _vres;

  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  camlidl_ml2c_mpfr_mpfr_ptr(_v_ROP, &ROP, _ctx);
  camlidl_ml2c_mpfr_mpfr_ptr(_v_OP, &OP, _ctx);
  camlidl_ml2c_mpfr_mpfr_rnd_t(_v_RND, &RND, _ctx);
  _res = mpfr_erf(ROP, OP, RND);
  _vres = Val_int(_res);
  camlidl_free(_ctx);
  return _vres;
}

value camlidl_mpfr_mpfr_erfc(
	value _v_ROP,
	value _v_OP,
	value _v_RND)
{
  mpfr_ptr ROP; /*in*/
  mpfr_ptr OP; /*in*/
  mpfr_rnd_t RND; /*in*/
  int _res;
  value _vres;

  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  camlidl_ml2c_mpfr_mpfr_ptr(_v_ROP, &ROP, _ctx);
  camlidl_ml2c_mpfr_mpfr_ptr(_v_OP, &OP, _ctx);
  camlidl_ml2c_mpfr_mpfr_rnd_t(_v_RND, &RND, _ctx);
  _res = mpfr_erfc(ROP, OP, RND);
  _vres = Val_int(_res);
  camlidl_free(_ctx);
  return _vres;
}

value camlidl_mpfr_mpfr_fma(
	value _v_ROP,
	value _v_OPX,
	value _v_OPY,
	value _v_OPZ,
	value _v_RND)
{
  mpfr_ptr ROP; /*in*/
  mpfr_ptr OPX; /*in*/
  mpfr_ptr OPY; /*in*/
  mpfr_ptr OPZ; /*in*/
  mpfr_rnd_t RND; /*in*/
  int _res;
  value _vres;

  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  camlidl_ml2c_mpfr_mpfr_ptr(_v_ROP, &ROP, _ctx);
  camlidl_ml2c_mpfr_mpfr_ptr(_v_OPX, &OPX, _ctx);
  camlidl_ml2c_mpfr_mpfr_ptr(_v_OPY, &OPY, _ctx);
  camlidl_ml2c_mpfr_mpfr_ptr(_v_OPZ, &OPZ, _ctx);
  camlidl_ml2c_mpfr_mpfr_rnd_t(_v_RND, &RND, _ctx);
  _res = mpfr_fma(ROP, OPX, OPY, OPZ, RND);
  _vres = Val_int(_res);
  camlidl_free(_ctx);
  return _vres;
}

value camlidl_mpfr_mpfr_agm(
	value _v_ROP,
	value _v_OP1,
	value _v_OP2,
	value _v_RND)
{
  mpfr_ptr ROP; /*in*/
  mpfr_ptr OP1; /*in*/
  mpfr_ptr OP2; /*in*/
  mpfr_rnd_t RND; /*in*/
  int _res;
  value _vres;

  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  camlidl_ml2c_mpfr_mpfr_ptr(_v_ROP, &ROP, _ctx);
  camlidl_ml2c_mpfr_mpfr_ptr(_v_OP1, &OP1, _ctx);
  camlidl_ml2c_mpfr_mpfr_ptr(_v_OP2, &OP2, _ctx);
  camlidl_ml2c_mpfr_mpfr_rnd_t(_v_RND, &RND, _ctx);
  _res = mpfr_agm(ROP, OP1, OP2, RND);
  _vres = Val_int(_res);
  camlidl_free(_ctx);
  return _vres;
}

value camlidl_mpfr_mpfr_hypot(
	value _v_ROP,
	value _v_X,
	value _v_Y,
	value _v_RND)
{
  mpfr_ptr ROP; /*in*/
  mpfr_ptr X; /*in*/
  mpfr_ptr Y; /*in*/
  mpfr_rnd_t RND; /*in*/
  int _res;
  value _vres;

  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  camlidl_ml2c_mpfr_mpfr_ptr(_v_ROP, &ROP, _ctx);
  camlidl_ml2c_mpfr_mpfr_ptr(_v_X, &X, _ctx);
  camlidl_ml2c_mpfr_mpfr_ptr(_v_Y, &Y, _ctx);
  camlidl_ml2c_mpfr_mpfr_rnd_t(_v_RND, &RND, _ctx);
  _res = mpfr_hypot(ROP, X, Y, RND);
  _vres = Val_int(_res);
  camlidl_free(_ctx);
  return _vres;
}

value camlidl_mpfr_mpfr_const_log2(
	value _v_ROP,
	value _v_RND)
{
  mpfr_ptr ROP; /*in*/
  mpfr_rnd_t RND; /*in*/
  int _res;
  value _vres;

  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  camlidl_ml2c_mpfr_mpfr_ptr(_v_ROP, &ROP, _ctx);
  camlidl_ml2c_mpfr_mpfr_rnd_t(_v_RND, &RND, _ctx);
  _res = mpfr_const_log2(ROP, RND);
  _vres = Val_int(_res);
  camlidl_free(_ctx);
  return _vres;
}

value camlidl_mpfr_mpfr_const_pi(
	value _v_ROP,
	value _v_RND)
{
  mpfr_ptr ROP; /*in*/
  mpfr_rnd_t RND; /*in*/
  int _res;
  value _vres;

  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  camlidl_ml2c_mpfr_mpfr_ptr(_v_ROP, &ROP, _ctx);
  camlidl_ml2c_mpfr_mpfr_rnd_t(_v_RND, &RND, _ctx);
  _res = mpfr_const_pi(ROP, RND);
  _vres = Val_int(_res);
  camlidl_free(_ctx);
  return _vres;
}

value camlidl_mpfr_mpfr_const_euler(
	value _v_ROP,
	value _v_RND)
{
  mpfr_ptr ROP; /*in*/
  mpfr_rnd_t RND; /*in*/
  int _res;
  value _vres;

  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  camlidl_ml2c_mpfr_mpfr_ptr(_v_ROP, &ROP, _ctx);
  camlidl_ml2c_mpfr_mpfr_rnd_t(_v_RND, &RND, _ctx);
  _res = mpfr_const_euler(ROP, RND);
  _vres = Val_int(_res);
  camlidl_free(_ctx);
  return _vres;
}

value camlidl_mpfr_mpfr_const_catalan(
	value _v_ROP,
	value _v_RND)
{
  mpfr_ptr ROP; /*in*/
  mpfr_rnd_t RND; /*in*/
  int _res;
  value _vres;

  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  camlidl_ml2c_mpfr_mpfr_ptr(_v_ROP, &ROP, _ctx);
  camlidl_ml2c_mpfr_mpfr_rnd_t(_v_RND, &RND, _ctx);
  _res = mpfr_const_catalan(ROP, RND);
  _vres = Val_int(_res);
  camlidl_free(_ctx);
  return _vres;
}

value camlidl_mpfr_mpfr_rint(
	value _v_ROP,
	value _v_OP,
	value _v_RND)
{
  mpfr_ptr ROP; /*in*/
  mpfr_ptr OP; /*in*/
  mpfr_rnd_t RND; /*in*/
  int _res;
  value _vres;

  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  camlidl_ml2c_mpfr_mpfr_ptr(_v_ROP, &ROP, _ctx);
  camlidl_ml2c_mpfr_mpfr_ptr(_v_OP, &OP, _ctx);
  camlidl_ml2c_mpfr_mpfr_rnd_t(_v_RND, &RND, _ctx);
  _res = mpfr_rint(ROP, OP, RND);
  _vres = Val_int(_res);
  camlidl_free(_ctx);
  return _vres;
}

value camlidl_mpfr_mpfr_ceil(
	value _v_ROP,
	value _v_OP)
{
  mpfr_ptr ROP; /*in*/
  mpfr_ptr OP; /*in*/
  int _res;
  value _vres;

  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  camlidl_ml2c_mpfr_mpfr_ptr(_v_ROP, &ROP, _ctx);
  camlidl_ml2c_mpfr_mpfr_ptr(_v_OP, &OP, _ctx);
  _res = mpfr_ceil(ROP, OP);
  _vres = Val_int(_res);
  camlidl_free(_ctx);
  return _vres;
}

value camlidl_mpfr_mpfr_floor(
	value _v_ROP,
	value _v_OP)
{
  mpfr_ptr ROP; /*in*/
  mpfr_ptr OP; /*in*/
  int _res;
  value _vres;

  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  camlidl_ml2c_mpfr_mpfr_ptr(_v_ROP, &ROP, _ctx);
  camlidl_ml2c_mpfr_mpfr_ptr(_v_OP, &OP, _ctx);
  _res = mpfr_floor(ROP, OP);
  _vres = Val_int(_res);
  camlidl_free(_ctx);
  return _vres;
}

value camlidl_mpfr_mpfr_round(
	value _v_ROP,
	value _v_OP)
{
  mpfr_ptr ROP; /*in*/
  mpfr_ptr OP; /*in*/
  int _res;
  value _vres;

  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  camlidl_ml2c_mpfr_mpfr_ptr(_v_ROP, &ROP, _ctx);
  camlidl_ml2c_mpfr_mpfr_ptr(_v_OP, &OP, _ctx);
  _res = mpfr_round(ROP, OP);
  _vres = Val_int(_res);
  camlidl_free(_ctx);
  return _vres;
}

value camlidl_mpfr_mpfr_trunc(
	value _v_ROP,
	value _v_OP)
{
  mpfr_ptr ROP; /*in*/
  mpfr_ptr OP; /*in*/
  int _res;
  value _vres;

  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  camlidl_ml2c_mpfr_mpfr_ptr(_v_ROP, &ROP, _ctx);
  camlidl_ml2c_mpfr_mpfr_ptr(_v_OP, &OP, _ctx);
  _res = mpfr_trunc(ROP, OP);
  _vres = Val_int(_res);
  camlidl_free(_ctx);
  return _vres;
}

value camlidl_mpfr_mpfr_integer_p(
	value _v_OP)
{
  mpfr_ptr OP; /*in*/
  int _res;
  value _vres;

  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  camlidl_ml2c_mpfr_mpfr_ptr(_v_OP, &OP, _ctx);
  _res = mpfr_integer_p(OP);
  _vres = Val_int(_res);
  camlidl_free(_ctx);
  return _vres;
}

value camlidl_mpfr_mpfr_nexttoward(
	value _v_X,
	value _v_Y)
{
  mpfr_ptr X; /*in*/
  mpfr_ptr Y; /*in*/
  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  camlidl_ml2c_mpfr_mpfr_ptr(_v_X, &X, _ctx);
  camlidl_ml2c_mpfr_mpfr_ptr(_v_Y, &Y, _ctx);
  mpfr_nexttoward(X, Y);
  camlidl_free(_ctx);
  return Val_unit;
}

value camlidl_mpfr_mpfr_nextabove(
	value _v_X)
{
  mpfr_ptr X; /*in*/
  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  camlidl_ml2c_mpfr_mpfr_ptr(_v_X, &X, _ctx);
  mpfr_nextabove(X);
  camlidl_free(_ctx);
  return Val_unit;
}

value camlidl_mpfr_mpfr_nextbelow(
	value _v_X)
{
  mpfr_ptr X; /*in*/
  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  camlidl_ml2c_mpfr_mpfr_ptr(_v_X, &X, _ctx);
  mpfr_nextbelow(X);
  camlidl_free(_ctx);
  return Val_unit;
}

value camlidl_mpfr_mpfr_min(
	value _v_ROP,
	value _v_OP1,
	value _v_OP2,
	value _v_RND)
{
  mpfr_ptr ROP; /*in*/
  mpfr_ptr OP1; /*in*/
  mpfr_ptr OP2; /*in*/
  mpfr_rnd_t RND; /*in*/
  int _res;
  value _vres;

  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  camlidl_ml2c_mpfr_mpfr_ptr(_v_ROP, &ROP, _ctx);
  camlidl_ml2c_mpfr_mpfr_ptr(_v_OP1, &OP1, _ctx);
  camlidl_ml2c_mpfr_mpfr_ptr(_v_OP2, &OP2, _ctx);
  camlidl_ml2c_mpfr_mpfr_rnd_t(_v_RND, &RND, _ctx);
  _res = mpfr_min(ROP, OP1, OP2, RND);
  _vres = Val_int(_res);
  camlidl_free(_ctx);
  return _vres;
}

value camlidl_mpfr_mpfr_max(
	value _v_ROP,
	value _v_OP1,
	value _v_OP2,
	value _v_RND)
{
  mpfr_ptr ROP; /*in*/
  mpfr_ptr OP1; /*in*/
  mpfr_ptr OP2; /*in*/
  mpfr_rnd_t RND; /*in*/
  int _res;
  value _vres;

  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  camlidl_ml2c_mpfr_mpfr_ptr(_v_ROP, &ROP, _ctx);
  camlidl_ml2c_mpfr_mpfr_ptr(_v_OP1, &OP1, _ctx);
  camlidl_ml2c_mpfr_mpfr_ptr(_v_OP2, &OP2, _ctx);
  camlidl_ml2c_mpfr_mpfr_rnd_t(_v_RND, &RND, _ctx);
  _res = mpfr_max(ROP, OP1, OP2, RND);
  _vres = Val_int(_res);
  camlidl_free(_ctx);
  return _vres;
}

value camlidl_mpfr_mpfr_get_exp(
	value _v_X)
{
  mpfr_ptr X; /*in*/
  int _res;
  value _vres;

  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  camlidl_ml2c_mpfr_mpfr_ptr(_v_X, &X, _ctx);
  _res = mpfr_get_exp(X);
  _vres = Val_int(_res);
  camlidl_free(_ctx);
  return _vres;
}

value camlidl_mpfr_mpfr_set_exp(
	value _v_X,
	value _v_E)
{
  mpfr_ptr X; /*in*/
  long E; /*in*/
  int _res;
  value _vres;

  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  camlidl_ml2c_mpfr_mpfr_ptr(_v_X, &X, _ctx);
  E = Long_val(_v_E);
  _res = mpfr_set_exp(X, E);
  _vres = Val_int(_res);
  camlidl_free(_ctx);
  return _vres;
}

