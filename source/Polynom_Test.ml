open Polynom

let _ =
  let zero = Poly.zero () in
  let x15 = Poly.of_monom (Monom.of_factor (Factor.Var "x") 15) 1. in
  let z = Poly.of_monom (Monom.of_factor (Factor.Var "z") 1) 1. in
  let maxz = Poly.of_monom (Monom.of_factor (Factor.Max z) 3) (-0.5) in
  let ym1 =
    Poly.add (Poly.of_monom (Monom.of_factor (Factor.Var "y") 1) 1.) maxz in
  begin
    Format.printf "::@.%a@." Poly.print_ascii zero;
    Format.printf "::@.%a@." Poly.print_ascii x15;
    Format.printf "::@.%a@." Poly.print_ascii ym1;
    Format.printf "::@.%a@." Poly.print_ascii (poly_subst "z" (Poly.const 42.) maxz);
    Format.printf "::@.%a@." Poly.print_ascii (poly_subst "x" ym1 x15);
  end
