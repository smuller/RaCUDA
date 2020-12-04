/* File generated from mpf.idl */

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


#define camlidl_ml2c_mpf_mpf_ptr(v,c,ctx) camlidl_mpf_ptr_ml2c(v,c)

#define camlidl_c2ml_mpf_mpf_ptr(c,ctx) camlidl_mpf_ptr_c2ml(c)

value camlidl_mpf_mpf_set_default_prec(
	value _v_PREC)
{
  unsigned long PREC; /*in*/
  PREC = Long_val(_v_PREC);
  mpf_set_default_prec(PREC);
  return Val_unit;
}

value camlidl_mpf_mpf_get_default_prec(value _unit)
{
  unsigned long _res;
  value _vres;

  _res = mpf_get_default_prec();
  _vres = Val_long(_res);
  return _vres;
}

value camlidl_mpf_mpf_init(value _unit)
{
  mpf_t OUTOUTOUT; mpf_ptr OUTTOUTTOUTT = OUTOUTOUT;; /*out*/
  value _vres;

  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  mpf_init(OUTOUTOUT);
  _vres = camlidl_c2ml_mpf_mpf_ptr(&OUTTOUTTOUTT, _ctx);
  camlidl_free(_ctx);
  return _vres;
}

value camlidl_mpf_mpf_init2(
	value _v_PREC)
{
  mpf_t OUTOUTOUT; mpf_ptr OUTTOUTTOUTT = OUTOUTOUT;; /*out*/
  unsigned long PREC; /*in*/
  value _vres;

  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  PREC = Long_val(_v_PREC);
  mpf_init2(OUTOUTOUT, PREC);
  _vres = camlidl_c2ml_mpf_mpf_ptr(&OUTTOUTTOUTT, _ctx);
  camlidl_free(_ctx);
  return _vres;
}

value camlidl_mpf_mpf_get_prec(
	value _v_OP)
{
  mpf_ptr OP; /*in*/
  unsigned long _res;
  value _vres;

  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  camlidl_ml2c_mpf_mpf_ptr(_v_OP, &OP, _ctx);
  _res = mpf_get_prec(OP);
  _vres = Val_long(_res);
  camlidl_free(_ctx);
  return _vres;
}

value camlidl_mpf_mpf_set_prec(
	value _v_ROP,
	value _v_PREC)
{
  mpf_ptr ROP; /*in*/
  unsigned long PREC; /*in*/
  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  camlidl_ml2c_mpf_mpf_ptr(_v_ROP, &ROP, _ctx);
  PREC = Long_val(_v_PREC);
  mpf_set_prec(ROP, PREC);
  camlidl_free(_ctx);
  return Val_unit;
}

value camlidl_mpf_mpf_set_prec_raw(
	value _v_ROP,
	value _v_PREC)
{
  mpf_ptr ROP; /*in*/
  unsigned long PREC; /*in*/
  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  camlidl_ml2c_mpf_mpf_ptr(_v_ROP, &ROP, _ctx);
  PREC = Long_val(_v_PREC);
  mpf_set_prec_raw(ROP, PREC);
  camlidl_free(_ctx);
  return Val_unit;
}

value camlidl_mpf_mpf_set(
	value _v_ROP,
	value _v_OP)
{
  mpf_ptr ROP; /*in*/
  mpf_ptr OP; /*in*/
  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  camlidl_ml2c_mpf_mpf_ptr(_v_ROP, &ROP, _ctx);
  camlidl_ml2c_mpf_mpf_ptr(_v_OP, &OP, _ctx);
  mpf_set(ROP, OP);
  camlidl_free(_ctx);
  return Val_unit;
}

value camlidl_mpf_mpf_set_si(
	value _v_ROP,
	value _v_OP)
{
  mpf_ptr ROP; /*in*/
  long OP; /*in*/
  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  camlidl_ml2c_mpf_mpf_ptr(_v_ROP, &ROP, _ctx);
  OP = Long_val(_v_OP);
  mpf_set_si(ROP, OP);
  camlidl_free(_ctx);
  return Val_unit;
}

value camlidl_mpf_mpf_set_d(
	value _v_ROP,
	value _v_OP)
{
  mpf_ptr ROP; /*in*/
  double OP; /*in*/
  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  camlidl_ml2c_mpf_mpf_ptr(_v_ROP, &ROP, _ctx);
  OP = Double_val(_v_OP);
  mpf_set_d(ROP, OP);
  camlidl_free(_ctx);
  return Val_unit;
}

value camlidl_mpf_mpf_set_z(
	value _v_ROP,
	value _v_OP)
{
  mpf_ptr ROP; /*in*/
  mpz_ptr OP; /*in*/
  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  camlidl_ml2c_mpf_mpf_ptr(_v_ROP, &ROP, _ctx);
  camlidl_ml2c_mpz_mpz_ptr(_v_OP, &OP, _ctx);
  mpf_set_z(ROP, OP);
  camlidl_free(_ctx);
  return Val_unit;
}

value camlidl_mpf_mpf_set_q(
	value _v_ROP,
	value _v_OP)
{
  mpf_ptr ROP; /*in*/
  mpq_ptr OP; /*in*/
  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  camlidl_ml2c_mpf_mpf_ptr(_v_ROP, &ROP, _ctx);
  camlidl_ml2c_mpq_mpq_ptr(_v_OP, &OP, _ctx);
  mpf_set_q(ROP, OP);
  camlidl_free(_ctx);
  return Val_unit;
}

value camlidl_mpf_mpf__set_str(
	value _v_ROP,
	value _v_STR,
	value _v_BASE)
{
  mpf_ptr ROP; /*in*/
  char *STR; /*in*/
  int BASE; /*in*/
  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  camlidl_ml2c_mpf_mpf_ptr(_v_ROP, &ROP, _ctx);
  STR = String_val(_v_STR);
  BASE = Int_val(_v_BASE);
  /* begin user-supplied calling sequence */
{
  int n = mpf_set_str(ROP,STR,BASE);
  if (n){ mpf_clear(ROP); caml_invalid_argument(""); }
  }
  /* end user-supplied calling sequence */
  camlidl_free(_ctx);
  return Val_unit;
}

value camlidl_mpf_mpf_swap(
	value _v_ROP1,
	value _v_ROP2)
{
  mpf_ptr ROP1; /*in*/
  mpf_ptr ROP2; /*in*/
  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  camlidl_ml2c_mpf_mpf_ptr(_v_ROP1, &ROP1, _ctx);
  camlidl_ml2c_mpf_mpf_ptr(_v_ROP2, &ROP2, _ctx);
  mpf_swap(ROP1, ROP2);
  camlidl_free(_ctx);
  return Val_unit;
}

value camlidl_mpf_mpf_init_set(
	value _v_OP)
{
  mpf_t OUTOUTOUT; mpf_ptr OUTTOUTTOUTT = OUTOUTOUT;; /*out*/
  mpf_ptr OP; /*in*/
  value _vres;

  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  camlidl_ml2c_mpf_mpf_ptr(_v_OP, &OP, _ctx);
  mpf_init_set(OUTOUTOUT, OP);
  _vres = camlidl_c2ml_mpf_mpf_ptr(&OUTTOUTTOUTT, _ctx);
  camlidl_free(_ctx);
  return _vres;
}

value camlidl_mpf_mpf_init_set_si(
	value _v_OP)
{
  mpf_t OUTOUTOUT; mpf_ptr OUTTOUTTOUTT = OUTOUTOUT;; /*out*/
  long OP; /*in*/
  value _vres;

  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  OP = Long_val(_v_OP);
  mpf_init_set_si(OUTOUTOUT, OP);
  _vres = camlidl_c2ml_mpf_mpf_ptr(&OUTTOUTTOUTT, _ctx);
  camlidl_free(_ctx);
  return _vres;
}

value camlidl_mpf_mpf_init_set_d(
	value _v_OP)
{
  mpf_t OUTOUTOUT; mpf_ptr OUTTOUTTOUTT = OUTOUTOUT;; /*out*/
  double OP; /*in*/
  value _vres;

  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  OP = Double_val(_v_OP);
  mpf_init_set_d(OUTOUTOUT, OP);
  _vres = camlidl_c2ml_mpf_mpf_ptr(&OUTTOUTTOUTT, _ctx);
  camlidl_free(_ctx);
  return _vres;
}

value camlidl_mpf_mpf__init_set_str(
	value _v_STR,
	value _v_BASE)
{
  mpf_t OUTOUTOUT; mpf_ptr OUTTOUTTOUTT = OUTOUTOUT;; /*out*/
  char *STR; /*in*/
  int BASE; /*in*/
  value _vres;

  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  STR = String_val(_v_STR);
  BASE = Int_val(_v_BASE);
  /* begin user-supplied calling sequence */
  {
  int n = mpf_init_set_str(OUTOUTOUT,STR,BASE);
  if (n){ mpf_clear(OUTOUTOUT); caml_invalid_argument(""); }
  }
  /* end user-supplied calling sequence */
  _vres = camlidl_c2ml_mpf_mpf_ptr(&OUTTOUTTOUTT, _ctx);
  camlidl_free(_ctx);
  return _vres;
}

value camlidl_mpf_mpf_get_d(
	value _v_OP)
{
  mpf_ptr OP; /*in*/
  double _res;
  value _vres;

  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  camlidl_ml2c_mpf_mpf_ptr(_v_OP, &OP, _ctx);
  _res = mpf_get_d(OP);
  _vres = copy_double(_res);
  camlidl_free(_ctx);
  return _vres;
}

value camlidl_mpf_mpf_get_d_2exp(
	value _v_OP)
{
  long *EXP; /*out*/
  mpf_ptr OP; /*in*/
  double _res;
  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  long _c1;
  value _vresult;
  value _vres[2] = { 0, 0, };

  camlidl_ml2c_mpf_mpf_ptr(_v_OP, &OP, _ctx);
  EXP = &_c1;
  _res = mpf_get_d_2exp(EXP, OP);
  Begin_roots_block(_vres, 2)
    _vres[0] = copy_double(_res);
    _vres[1] = Val_long(*EXP);
    _vresult = camlidl_alloc_small(2, 0);
    Field(_vresult, 0) = _vres[0];
    Field(_vresult, 1) = _vres[1];
  End_roots()
  camlidl_free(_ctx);
  return _vresult;
}

value camlidl_mpf_mpf_get_si(
	value _v_OP)
{
  mpf_ptr OP; /*in*/
  long _res;
  value _vres;

  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  camlidl_ml2c_mpf_mpf_ptr(_v_OP, &OP, _ctx);
  _res = mpf_get_si(OP);
  _vres = copy_nativeint(_res);
  camlidl_free(_ctx);
  return _vres;
}

value camlidl_mpf_mpf_get_int(
	value _v_OP)
{
  mpf_ptr OP; /*in*/
  long _res;
  value _vres;

  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  camlidl_ml2c_mpf_mpf_ptr(_v_OP, &OP, _ctx);
  /* begin user-supplied calling sequence */
_res = mpf_get_si(OP);
  /* end user-supplied calling sequence */
  _vres = Val_long(_res);
  camlidl_free(_ctx);
  return _vres;
}

value camlidl_mpf_mpf_get_z(
	value _v_ROP,
	value _v_OP)
{
  mpz_ptr ROP; /*in*/
  mpf_ptr OP; /*in*/
  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  camlidl_ml2c_mpz_mpz_ptr(_v_ROP, &ROP, _ctx);
  camlidl_ml2c_mpf_mpf_ptr(_v_OP, &OP, _ctx);
  /* begin user-supplied calling sequence */
mpz_set_f(ROP,OP);
  /* end user-supplied calling sequence */
  camlidl_free(_ctx);
  return Val_unit;
}

value camlidl_mpf_mpf_get_q(
	value _v_ROP,
	value _v_OP)
{
  mpq_ptr ROP; /*in*/
  mpf_ptr OP; /*in*/
  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  camlidl_ml2c_mpq_mpq_ptr(_v_ROP, &ROP, _ctx);
  camlidl_ml2c_mpf_mpf_ptr(_v_OP, &OP, _ctx);
  /* begin user-supplied calling sequence */
mpq_set_f(ROP,OP);
  /* end user-supplied calling sequence */
  camlidl_free(_ctx);
  return Val_unit;
}

value camlidl_mpf_mpf__get_str(
	value _v_BASE,
	value _v_N_DIGITS,
	value _v_OP)
{
  long *EXPPTR; /*out*/
  int BASE; /*in*/
  int N_DIGITS; /*in*/
  mpf_ptr OP; /*in*/
  char *_res;
  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  long _c1;
  value _vresult;
  value _vres[2] = { 0, 0, };

  BASE = Int_val(_v_BASE);
  N_DIGITS = Int_val(_v_N_DIGITS);
  camlidl_ml2c_mpf_mpf_ptr(_v_OP, &OP, _ctx);
  EXPPTR = &_c1;
  /* begin user-supplied calling sequence */
_res = mpf_get_str(NULL,EXPPTR,BASE,N_DIGITS,OP);
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

value camlidl_mpf_mpf_add(
	value _v_ROP,
	value _v_OP1,
	value _v_OP2)
{
  mpf_ptr ROP; /*in*/
  mpf_ptr OP1; /*in*/
  mpf_ptr OP2; /*in*/
  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  camlidl_ml2c_mpf_mpf_ptr(_v_ROP, &ROP, _ctx);
  camlidl_ml2c_mpf_mpf_ptr(_v_OP1, &OP1, _ctx);
  camlidl_ml2c_mpf_mpf_ptr(_v_OP2, &OP2, _ctx);
  mpf_add(ROP, OP1, OP2);
  camlidl_free(_ctx);
  return Val_unit;
}

value camlidl_mpf_mpf_add_ui(
	value _v_ROP,
	value _v_OP1,
	value _v_OP2)
{
  mpf_ptr ROP; /*in*/
  mpf_ptr OP1; /*in*/
  unsigned long OP2; /*in*/
  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  camlidl_ml2c_mpf_mpf_ptr(_v_ROP, &ROP, _ctx);
  camlidl_ml2c_mpf_mpf_ptr(_v_OP1, &OP1, _ctx);
  OP2 = Long_val(_v_OP2);
  mpf_add_ui(ROP, OP1, OP2);
  camlidl_free(_ctx);
  return Val_unit;
}

value camlidl_mpf_mpf_sub(
	value _v_ROP,
	value _v_OP1,
	value _v_OP2)
{
  mpf_ptr ROP; /*in*/
  mpf_ptr OP1; /*in*/
  mpf_ptr OP2; /*in*/
  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  camlidl_ml2c_mpf_mpf_ptr(_v_ROP, &ROP, _ctx);
  camlidl_ml2c_mpf_mpf_ptr(_v_OP1, &OP1, _ctx);
  camlidl_ml2c_mpf_mpf_ptr(_v_OP2, &OP2, _ctx);
  mpf_sub(ROP, OP1, OP2);
  camlidl_free(_ctx);
  return Val_unit;
}

value camlidl_mpf_mpf_ui_sub(
	value _v_ROP,
	value _v_OP1,
	value _v_OP2)
{
  mpf_ptr ROP; /*in*/
  unsigned long OP1; /*in*/
  mpf_ptr OP2; /*in*/
  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  camlidl_ml2c_mpf_mpf_ptr(_v_ROP, &ROP, _ctx);
  OP1 = Long_val(_v_OP1);
  camlidl_ml2c_mpf_mpf_ptr(_v_OP2, &OP2, _ctx);
  mpf_ui_sub(ROP, OP1, OP2);
  camlidl_free(_ctx);
  return Val_unit;
}

value camlidl_mpf_mpf_sub_ui(
	value _v_ROP,
	value _v_OP1,
	value _v_OP2)
{
  mpf_ptr ROP; /*in*/
  mpf_ptr OP1; /*in*/
  unsigned long OP2; /*in*/
  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  camlidl_ml2c_mpf_mpf_ptr(_v_ROP, &ROP, _ctx);
  camlidl_ml2c_mpf_mpf_ptr(_v_OP1, &OP1, _ctx);
  OP2 = Long_val(_v_OP2);
  mpf_sub_ui(ROP, OP1, OP2);
  camlidl_free(_ctx);
  return Val_unit;
}

value camlidl_mpf_mpf_mul(
	value _v_ROP,
	value _v_OP1,
	value _v_OP2)
{
  mpf_ptr ROP; /*in*/
  mpf_ptr OP1; /*in*/
  mpf_ptr OP2; /*in*/
  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  camlidl_ml2c_mpf_mpf_ptr(_v_ROP, &ROP, _ctx);
  camlidl_ml2c_mpf_mpf_ptr(_v_OP1, &OP1, _ctx);
  camlidl_ml2c_mpf_mpf_ptr(_v_OP2, &OP2, _ctx);
  mpf_mul(ROP, OP1, OP2);
  camlidl_free(_ctx);
  return Val_unit;
}

value camlidl_mpf_mpf_mul_ui(
	value _v_ROP,
	value _v_OP1,
	value _v_OP2)
{
  mpf_ptr ROP; /*in*/
  mpf_ptr OP1; /*in*/
  unsigned long OP2; /*in*/
  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  camlidl_ml2c_mpf_mpf_ptr(_v_ROP, &ROP, _ctx);
  camlidl_ml2c_mpf_mpf_ptr(_v_OP1, &OP1, _ctx);
  OP2 = Long_val(_v_OP2);
  mpf_mul_ui(ROP, OP1, OP2);
  camlidl_free(_ctx);
  return Val_unit;
}

value camlidl_mpf_mpf_mul_2exp(
	value _v_ROP,
	value _v_OP1,
	value _v_OP2)
{
  mpf_ptr ROP; /*in*/
  mpf_ptr OP1; /*in*/
  unsigned long OP2; /*in*/
  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  camlidl_ml2c_mpf_mpf_ptr(_v_ROP, &ROP, _ctx);
  camlidl_ml2c_mpf_mpf_ptr(_v_OP1, &OP1, _ctx);
  OP2 = Long_val(_v_OP2);
  mpf_mul_2exp(ROP, OP1, OP2);
  camlidl_free(_ctx);
  return Val_unit;
}

value camlidl_mpf_mpf_div(
	value _v_ROP,
	value _v_OP1,
	value _v_OP2)
{
  mpf_ptr ROP; /*in*/
  mpf_ptr OP1; /*in*/
  mpf_ptr OP2; /*in*/
  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  camlidl_ml2c_mpf_mpf_ptr(_v_ROP, &ROP, _ctx);
  camlidl_ml2c_mpf_mpf_ptr(_v_OP1, &OP1, _ctx);
  camlidl_ml2c_mpf_mpf_ptr(_v_OP2, &OP2, _ctx);
  mpf_div(ROP, OP1, OP2);
  camlidl_free(_ctx);
  return Val_unit;
}

value camlidl_mpf_mpf_ui_div(
	value _v_ROP,
	value _v_OP1,
	value _v_OP2)
{
  mpf_ptr ROP; /*in*/
  unsigned long OP1; /*in*/
  mpf_ptr OP2; /*in*/
  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  camlidl_ml2c_mpf_mpf_ptr(_v_ROP, &ROP, _ctx);
  OP1 = Long_val(_v_OP1);
  camlidl_ml2c_mpf_mpf_ptr(_v_OP2, &OP2, _ctx);
  mpf_ui_div(ROP, OP1, OP2);
  camlidl_free(_ctx);
  return Val_unit;
}

value camlidl_mpf_mpf_div_ui(
	value _v_ROP,
	value _v_OP1,
	value _v_OP2)
{
  mpf_ptr ROP; /*in*/
  mpf_ptr OP1; /*in*/
  unsigned long OP2; /*in*/
  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  camlidl_ml2c_mpf_mpf_ptr(_v_ROP, &ROP, _ctx);
  camlidl_ml2c_mpf_mpf_ptr(_v_OP1, &OP1, _ctx);
  OP2 = Long_val(_v_OP2);
  mpf_div_ui(ROP, OP1, OP2);
  camlidl_free(_ctx);
  return Val_unit;
}

value camlidl_mpf_mpf_div_2exp(
	value _v_ROP,
	value _v_OP1,
	value _v_OP2)
{
  mpf_ptr ROP; /*in*/
  mpf_ptr OP1; /*in*/
  unsigned long OP2; /*in*/
  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  camlidl_ml2c_mpf_mpf_ptr(_v_ROP, &ROP, _ctx);
  camlidl_ml2c_mpf_mpf_ptr(_v_OP1, &OP1, _ctx);
  OP2 = Long_val(_v_OP2);
  mpf_div_2exp(ROP, OP1, OP2);
  camlidl_free(_ctx);
  return Val_unit;
}

value camlidl_mpf_mpf_sqrt(
	value _v_ROP,
	value _v_OP)
{
  mpf_ptr ROP; /*in*/
  mpf_ptr OP; /*in*/
  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  camlidl_ml2c_mpf_mpf_ptr(_v_ROP, &ROP, _ctx);
  camlidl_ml2c_mpf_mpf_ptr(_v_OP, &OP, _ctx);
  mpf_sqrt(ROP, OP);
  camlidl_free(_ctx);
  return Val_unit;
}

value camlidl_mpf_mpf_pow_ui(
	value _v_ROP,
	value _v_OP1,
	value _v_OP2)
{
  mpf_ptr ROP; /*in*/
  mpf_ptr OP1; /*in*/
  unsigned long OP2; /*in*/
  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  camlidl_ml2c_mpf_mpf_ptr(_v_ROP, &ROP, _ctx);
  camlidl_ml2c_mpf_mpf_ptr(_v_OP1, &OP1, _ctx);
  OP2 = Long_val(_v_OP2);
  mpf_pow_ui(ROP, OP1, OP2);
  camlidl_free(_ctx);
  return Val_unit;
}

value camlidl_mpf_mpf_neg(
	value _v_ROP,
	value _v_OP)
{
  mpf_ptr ROP; /*in*/
  mpf_ptr OP; /*in*/
  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  camlidl_ml2c_mpf_mpf_ptr(_v_ROP, &ROP, _ctx);
  camlidl_ml2c_mpf_mpf_ptr(_v_OP, &OP, _ctx);
  mpf_neg(ROP, OP);
  camlidl_free(_ctx);
  return Val_unit;
}

value camlidl_mpf_mpf_abs(
	value _v_ROP,
	value _v_OP)
{
  mpf_ptr ROP; /*in*/
  mpf_ptr OP; /*in*/
  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  camlidl_ml2c_mpf_mpf_ptr(_v_ROP, &ROP, _ctx);
  camlidl_ml2c_mpf_mpf_ptr(_v_OP, &OP, _ctx);
  mpf_abs(ROP, OP);
  camlidl_free(_ctx);
  return Val_unit;
}

value camlidl_mpf_mpf_cmp(
	value _v_OP1,
	value _v_OP2)
{
  mpf_ptr OP1; /*in*/
  mpf_ptr OP2; /*in*/
  int _res;
  value _vres;

  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  camlidl_ml2c_mpf_mpf_ptr(_v_OP1, &OP1, _ctx);
  camlidl_ml2c_mpf_mpf_ptr(_v_OP2, &OP2, _ctx);
  _res = mpf_cmp(OP1, OP2);
  _vres = Val_int(_res);
  camlidl_free(_ctx);
  return _vres;
}

value camlidl_mpf_mpf_cmp_d(
	value _v_OP1,
	value _v_OP2)
{
  mpf_ptr OP1; /*in*/
  double OP2; /*in*/
  int _res;
  value _vres;

  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  camlidl_ml2c_mpf_mpf_ptr(_v_OP1, &OP1, _ctx);
  OP2 = Double_val(_v_OP2);
  _res = mpf_cmp_d(OP1, OP2);
  _vres = Val_int(_res);
  camlidl_free(_ctx);
  return _vres;
}

value camlidl_mpf_mpf_cmp_si(
	value _v_OP1,
	value _v_OP2)
{
  mpf_ptr OP1; /*in*/
  long OP2; /*in*/
  int _res;
  value _vres;

  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  camlidl_ml2c_mpf_mpf_ptr(_v_OP1, &OP1, _ctx);
  OP2 = Long_val(_v_OP2);
  _res = mpf_cmp_si(OP1, OP2);
  _vres = Val_int(_res);
  camlidl_free(_ctx);
  return _vres;
}

value camlidl_mpf_mpf_sgn(
	value _v_OP)
{
  mpf_ptr OP; /*in*/
  int _res;
  value _vres;

  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  camlidl_ml2c_mpf_mpf_ptr(_v_OP, &OP, _ctx);
  _res = mpf_sgn(OP);
  _vres = Val_int(_res);
  camlidl_free(_ctx);
  return _vres;
}

value camlidl_mpf_mpf__equal(
	value _v_OP1,
	value _v_OP2,
	value _v_OP3)
{
  mpf_ptr OP1; /*in*/
  mpf_ptr OP2; /*in*/
  unsigned long OP3; /*in*/
  int _res;
  value _vres;

  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  camlidl_ml2c_mpf_mpf_ptr(_v_OP1, &OP1, _ctx);
  camlidl_ml2c_mpf_mpf_ptr(_v_OP2, &OP2, _ctx);
  OP3 = Long_val(_v_OP3);
  /* begin user-supplied calling sequence */
_res=mpf_eq(OP1,OP2,OP3);
  /* end user-supplied calling sequence */
  _vres = Val_int(_res);
  camlidl_free(_ctx);
  return _vres;
}

value camlidl_mpf_mpf_reldiff(
	value _v_ROP,
	value _v_OP1,
	value _v_OP2)
{
  mpf_ptr ROP; /*in*/
  mpf_ptr OP1; /*in*/
  mpf_ptr OP2; /*in*/
  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  camlidl_ml2c_mpf_mpf_ptr(_v_ROP, &ROP, _ctx);
  camlidl_ml2c_mpf_mpf_ptr(_v_OP1, &OP1, _ctx);
  camlidl_ml2c_mpf_mpf_ptr(_v_OP2, &OP2, _ctx);
  mpf_reldiff(ROP, OP1, OP2);
  camlidl_free(_ctx);
  return Val_unit;
}

value camlidl_mpf_mpf_ceil(
	value _v_ROP,
	value _v_OP)
{
  mpf_ptr ROP; /*in*/
  mpf_ptr OP; /*in*/
  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  camlidl_ml2c_mpf_mpf_ptr(_v_ROP, &ROP, _ctx);
  camlidl_ml2c_mpf_mpf_ptr(_v_OP, &OP, _ctx);
  mpf_ceil(ROP, OP);
  camlidl_free(_ctx);
  return Val_unit;
}

value camlidl_mpf_mpf_floor(
	value _v_ROP,
	value _v_OP)
{
  mpf_ptr ROP; /*in*/
  mpf_ptr OP; /*in*/
  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  camlidl_ml2c_mpf_mpf_ptr(_v_ROP, &ROP, _ctx);
  camlidl_ml2c_mpf_mpf_ptr(_v_OP, &OP, _ctx);
  mpf_floor(ROP, OP);
  camlidl_free(_ctx);
  return Val_unit;
}

value camlidl_mpf_mpf_trunc(
	value _v_ROP,
	value _v_OP)
{
  mpf_ptr ROP; /*in*/
  mpf_ptr OP; /*in*/
  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  camlidl_ml2c_mpf_mpf_ptr(_v_ROP, &ROP, _ctx);
  camlidl_ml2c_mpf_mpf_ptr(_v_OP, &OP, _ctx);
  mpf_trunc(ROP, OP);
  camlidl_free(_ctx);
  return Val_unit;
}

value camlidl_mpf_mpf_integer_p(
	value _v_OP)
{
  mpf_ptr OP; /*in*/
  int _res;
  value _vres;

  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  camlidl_ml2c_mpf_mpf_ptr(_v_OP, &OP, _ctx);
  _res = mpf_integer_p(OP);
  _vres = Val_int(_res);
  camlidl_free(_ctx);
  return _vres;
}

value camlidl_mpf_mpf_fits_int_p(
	value _v_OP)
{
  mpf_ptr OP; /*in*/
  int _res;
  value _vres;

  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  camlidl_ml2c_mpf_mpf_ptr(_v_OP, &OP, _ctx);
  _res = mpf_fits_int_p(OP);
  _vres = Val_int(_res);
  camlidl_free(_ctx);
  return _vres;
}

value camlidl_mpf_mpf_fits_ulong_p(
	value _v_OP)
{
  mpf_ptr OP; /*in*/
  int _res;
  value _vres;

  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  camlidl_ml2c_mpf_mpf_ptr(_v_OP, &OP, _ctx);
  _res = mpf_fits_ulong_p(OP);
  _vres = Val_int(_res);
  camlidl_free(_ctx);
  return _vres;
}

value camlidl_mpf_mpf_fits_slong_p(
	value _v_OP)
{
  mpf_ptr OP; /*in*/
  int _res;
  value _vres;

  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  camlidl_ml2c_mpf_mpf_ptr(_v_OP, &OP, _ctx);
  _res = mpf_fits_slong_p(OP);
  _vres = Val_int(_res);
  camlidl_free(_ctx);
  return _vres;
}

value camlidl_mpf_mpf_fits_uint_p(
	value _v_OP)
{
  mpf_ptr OP; /*in*/
  int _res;
  value _vres;

  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  camlidl_ml2c_mpf_mpf_ptr(_v_OP, &OP, _ctx);
  _res = mpf_fits_uint_p(OP);
  _vres = Val_int(_res);
  camlidl_free(_ctx);
  return _vres;
}

value camlidl_mpf_mpf_fits_sint_p(
	value _v_OP)
{
  mpf_ptr OP; /*in*/
  int _res;
  value _vres;

  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  camlidl_ml2c_mpf_mpf_ptr(_v_OP, &OP, _ctx);
  _res = mpf_fits_sint_p(OP);
  _vres = Val_int(_res);
  camlidl_free(_ctx);
  return _vres;
}

value camlidl_mpf_mpf_fits_ushort_p(
	value _v_OP)
{
  mpf_ptr OP; /*in*/
  int _res;
  value _vres;

  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  camlidl_ml2c_mpf_mpf_ptr(_v_OP, &OP, _ctx);
  _res = mpf_fits_ushort_p(OP);
  _vres = Val_int(_res);
  camlidl_free(_ctx);
  return _vres;
}

value camlidl_mpf_mpf_fits_sshort_p(
	value _v_OP)
{
  mpf_ptr OP; /*in*/
  int _res;
  value _vres;

  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  camlidl_ml2c_mpf_mpf_ptr(_v_OP, &OP, _ctx);
  _res = mpf_fits_sshort_p(OP);
  _vres = Val_int(_res);
  camlidl_free(_ctx);
  return _vres;
}

