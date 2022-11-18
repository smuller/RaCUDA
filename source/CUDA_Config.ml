let memreads_func = "__memreads"
let bankconflicts_func = "__conflicts"
let tid_var = "__tid"
let tidx_var = "__tidx"
let tidy_var = "__tidy"
let tidz_var = "__tidz"
let bidx_var = "__bidx"
let bidy_var = "__bidy"
let bidz_var = "__bidz"
let bid_var = "__bid"
let bdimx_var = "__bdimx"
let bdimy_var = "__bdimy"
let bdimz_var = "__bdimz"
              
let warpsize_var = "__warp"

let div_cost = "__div_cost"

let cuda_vars = [tid_var;
                 tidx_var;
                 tidy_var;
                 tidz_var;
                 bidx_var;
                 bidy_var;
                 bidz_var;
                 bdimx_var;
                 bdimy_var;
                 bdimz_var;
                 bid_var;
                 warpsize_var]
