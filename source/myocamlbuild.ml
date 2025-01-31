open Ocamlbuild_plugin
open Command

let debug = false
let debug_build = true

let build_path = Sys.getcwd ()
let clp_inc_dir = Sys.getenv "INCDIRS"
let clp_lib_dir = Sys.getenv "LIBDIRS"
let coin_utils_inc_dir = (Sys.getenv "CUTILSDIR")

let cxx = try Sys.getenv "CXX" with Not_found -> "g++"

(* Clp specific flags. *)
let clp_stubs = "Clp_Stubs.o"
let flag_clp_include = [A "-ccopt"; A ("-O2" ^ " -Wall" ^ " -I" ^ clp_inc_dir ^ " -I" ^ coin_utils_inc_dir)]
let flag_clp_link =
  [A clp_stubs] @
  [A "-cclib"; A ("-L" ^ clp_lib_dir ^ " -lClp" ^ " -lCoinUtils")]
let flag_clp_byt = A "-custom" :: flag_clp_link

let () =
  dispatch begin function
             | After_rules ->
                ocaml_lib ~extern:true ~dir: "/usr0/home/smuller/absynth/source/frontc" "frontc";
    (* Make OCaml files with flag use_clp depend on Clp_Stubs.o. *)
    dep ["compile"; "ocaml"; "use_clp"] [clp_stubs];

    if debug_build then begin
      flag ["compile"] (S [A "-ccopt"; A "-g"; A "-g"]);
      flag ["link"] (S [A "-ccopt"; A "-g"; A "-g"]);
    end;

    flag ["compile"; "c"; "use_clp"] (S flag_clp_include);
    flag ["link"; "ocaml"; "native"; "use_clp"] (S flag_clp_link);
    flag ["link"; "ocaml"; "byte"; "use_clp"] (S flag_clp_byt);

    rule "Create configuration file" ~prod:"Config.ml"
      begin fun _env _build ->
        let out = Printf.sprintf "let build_path = %S" build_path in
        Cmd (S [A "echo"; A out; Sh "> "; Px "Config.ml"])
      end;

  | _ -> ()
  end
