open Std

(** {1 OCaml commandline parsing} *)

type ocaml = {
  include_dirs         : string list;
  no_std_include       : bool;
  unsafe               : bool;
  classic              : bool;
  principal            : bool;
  real_paths           : [ `Real | `Short | `Opened ];
  threads              : [ `None | `Threads | `Vmthreads ];
  recursive_types      : bool;
  strict_sequence      : bool;
  applicative_functors : bool;
  unsafe_string        : bool;
  nopervasives         : bool;
  strict_formats       : bool;
  open_modules         : string list;
  ppx                  : string list;
  pp                   : string;
}

let dump_ocaml x = `Assoc [
    "include_dirs"         , `List (List.map Json.string x.include_dirs);
    "no_std_include"       , `Bool x.no_std_include;
    "unsafe"               , `Bool x.unsafe;
    "classic"              , `Bool x.classic;
    "principal"            , `Bool x.principal;
    "real_paths"           , `String (
      match x.real_paths with
        `Real -> "real" | `Short -> "short" | `Opened -> "opened"
    );
    "recursive_types"      , `Bool x.recursive_types;
    "strict_sequence"      , `Bool x.strict_sequence;
    "applicative_functors" , `Bool x.applicative_functors;
    "unsafe_string"        , `Bool x.unsafe_string;
    "nopervasives"         , `Bool x.nopervasives;
    "strict_formats"       , `Bool x.strict_formats;
    "open_modules"         , `List (List.map Json.string x.open_modules);
    "ppx"                  , `List (List.map Json.string x.ppx);
    "pp"                   , `String x.pp;
  ]

(** {1 Findlib configuration} *)

type findlib = {
  conf : string option;
  path : string list;
}

let dump_findlib x = `Assoc [
    "conf", Json.option Json.string x.conf;
    "path", `List (List.map Json.string x.path);
  ]

let findlib_flags = []

(** {1 Merlin high-level settings} *)

type merlin = {
  build_path  : string list;
  source_path : string list;
  cmi_path    : string list;
  cmt_path    : string list;
  packages    : string list;
  flags       : string list list;
  extensions  : string list;
  suffixes    : (string * string) list;
  stdlib      : string option;
  reader      : string list;
}

let dump_merlin x = `Assoc [
    "build_path"  , `List (List.map Json.string x.build_path);
    "source_path" , `List (List.map Json.string x.source_path);
    "cmi_path"    , `List (List.map Json.string x.cmi_path);
    "cmt_path"    , `List (List.map Json.string x.cmt_path);
    "packages"    , `List (List.map Json.string x.packages);
    "flags"       , `List (List.map (fun l -> `List (List.map Json.string l))
                             x.flags);
    "extensions"  , `List (List.map Json.string x.extensions);
    "suffixes"    , `List (
      List.map (fun (impl,intf) -> `Assoc [
          "impl", `String impl;
          "intf", `String intf;
        ]) x.suffixes
    );
    "stdlib"      , Json.option Json.string x.stdlib;
    "reader"      , `List (List.map Json.string x.reader);
  ]

let merlin_flags = []

type t = {
  filename  : string;
  directory : string;
  ocaml     : ocaml;
  findlib   : findlib;
  merlin    : merlin;
}

let ocaml_ignored_flags = [
  "-a"; "-absname"; "-alias-deps"; "-annot"; "-app-funct"; "-bin-annot";
  "-c"; "-compact"; "-compat-32"; "-config"; "-custom"; "-dalloc";
  "-dclambda"; "-dcmm"; "-dcombine"; "-dcse"; "-dflambda";
  "-dflambda-no-invariants"; "-dflambda-verbose"; "-dinstr"; "-dinterf";
  "-dlambda"; "-dlinear"; "-dlive"; "-dparsetree"; "-dprefer";
  "-drawclambda"; "-drawflambda"; "-drawlambda"; "-dreload"; "-dscheduling";
  "-dsel"; "-dsource"; "-dspill"; "-dsplit"; "-dstartup"; "-dtimings";
  "-dtypedtree"; "-dtypes"; "-dump-pass"; "-fno-PIC"; "-fPIC"; "-g"; "-i";
  "-inlining-report"; "-keep-docs"; "-keep-docs"; "-keep-locs"; "-linkall";
  "-make_runtime"; "-make-runtime"; "-modern"; "-no-alias-deps"; "-noassert";
  "-noautolink"; "-no-check-prims"; "-nodynlink"; "-no-float-const-prop";
  "-no-keep-locs"; "-no-principal"; "-no-rectypes"; "-no-strict-formats";
  "-no-strict-sequence"; "-no-unbox-free-vars-of-clos";
  "-no-unbox-specialised-args"; "-O2"; "-O3"; "-Oclassic"; "-opaque";
  "-output-complete-obj"; "-output-obj"; "-p"; "-pack";
  "-remove-unused-arguments"; "-S"; "-shared"; "-unbox-closures"; "-v";
  "-verbose"; "-where";
]

let ocaml_ignored_parametrized_flags = [
  "-cc"; "-cclib"; "-ccopt"; "-color"; "-dflambda-let"; "-dllib"; "-dllpath";
  "-for-pack"; "-impl"; "-inline-alloc-cost"; "-inline-branch-cost";
  "-inline-branch-factor"; "-inline-call-cost"; "-inline-indirect-cost";
  "-inline-lifting-benefit"; "-inline-max-depth"; "-inline-max-unroll";
  "-inline"; "-inline-prim-cost"; "-inline-toplevel"; "-intf";
  "-intf_suffix"; "-intf-suffix"; "-o"; "-rounds"; "-runtime-variant";
  "-unbox-closures-factor"; "-use-prims"; "-use_runtime"; "-use-runtime";
]

let ocaml_flags = [
  (
    "-I",
    Marg.param "directory" (fun dir ocaml ->
        {ocaml with include_dirs = dir :: ocaml.include_dirs}),
    "<dir> Add <dir> to the list of include directories"
  );
  (
    "-nostdlib",
    Marg.unit (fun ocaml -> {ocaml with no_std_include = true}),
    " Do not add default directory to the list of include directories"
  );
  (
    "-unsafe",
    Marg.unit (fun ocaml -> {ocaml with unsafe = true}),
    " Do not compile bounds checking on array and string access"
  );
  (
    "-labels",
    Marg.unit (fun ocaml -> {ocaml with classic = false}),
    " Use commuting label mode"
  );
  (
    "-nolabels",
    Marg.unit (fun ocaml -> {ocaml with classic = true}),
    " Ignore non-optional labels in types"
  );
  (
    "-principal",
    Marg.unit (fun ocaml -> {ocaml with principal = true}),
    " Check principality of type inference"
  );
  (
    "-real-paths",
    Marg.unit (fun ocaml -> {ocaml with real_paths = `Real}),
    " Display real paths in types rather than short ones"
  );
  (
    "-short-paths",
    Marg.unit (fun ocaml -> {ocaml with real_paths = `Short}),
    " Shorten paths in types"
  );
  (
    "-opened-paths",
    Marg.unit (fun ocaml -> {ocaml with real_paths = `Opened}),
    " Remove opened prefix from displayed types"
  );
  (
    "-rectypes",
    Marg.unit (fun ocaml -> {ocaml with recursive_types = true}),
    " Allow arbitrary recursive types"
  );
  (
    "-strict-sequence",
    Marg.unit (fun ocaml -> {ocaml with strict_sequence = true}),
    " Left-hand part of a sequence must have type unit"
  );
  (
    "-no-app-funct",
    Marg.unit (fun ocaml -> {ocaml with applicative_functors = false}),
    " Deactivate applicative functors"
  );
  (
    "-thread",
    Marg.unit (fun ocaml -> {ocaml with threads = `Threads}),
    " Add support for system threads library"
  );
  (
    "-vmthread",
    Marg.unit (fun ocaml -> {ocaml with threads = `None}),
    " Add support for VM-scheduled threads library"
  );
  (
    "-unsafe-string",
    Marg.unit (fun ocaml -> {ocaml with unsafe_string = true}),
    " Make strings mutable (default)"
  );
  (
    "-safe-string",
    Marg.unit (fun ocaml -> {ocaml with unsafe_string = false}),
    " Make strings immutable"
  );
  (
    "-nopervasives",
    Marg.unit (fun ocaml -> {ocaml with nopervasives = true}),
    " Don't open Pervasives module (advanced)"
  );
  (
    "-strict-formats",
    Marg.unit (fun ocaml -> {ocaml with strict_formats = true}),
    " Reject invalid formats accepted by legacy implementations"
  );
  (
    "-open",
    Marg.param "module" (fun md ocaml ->
        {ocaml with open_modules = md :: ocaml.open_modules}),
    "<module>  Opens the module <module> before typing"
  );
  (
    "-ppx",
    Marg.param "command" (fun command ocaml ->
        {ocaml with ppx = command :: ocaml.ppx}),
    "<command> Pipe abstract syntax trees through preprocessor <command>"
  );
  (
    "-pp",
    Marg.param "command" (fun pp ocaml -> {ocaml with pp}),
    "<command> Pipe sources through preprocessor <command>"
  )
]

(** {1 Main configuration} *)

let initial = {
  filename = "<buffer>";
  directory = Sys.getcwd ();

  ocaml = {
    include_dirs         = [];
    no_std_include       = false;
    unsafe               = false;
    classic              = false;
    principal            = false;
    real_paths           = `Real;
    threads              = `None;
    recursive_types      = false;
    strict_sequence      = false;
    applicative_functors = true;
    unsafe_string        = true;
    nopervasives         = false;
    strict_formats       = false;
    open_modules         = [];
    ppx                  = [];
    pp                   = ""
  };
  findlib = {
    conf = None;
    path = [];
  };
  merlin = {
    build_path  = [];
    source_path = [];
    cmi_path    = [];
    cmt_path    = [];
    packages    = [];
    flags       = [];
    extensions  = [];
    suffixes    = [];
    stdlib      = None;
    reader      = [];
  };
}

let dump x = `Assoc [
    "ocaml"   , dump_ocaml x.ocaml;
    "findlib" , dump_findlib x.findlib;
    "merlin"  , dump_merlin x.merlin;
  ]

let normalize _trace t = t

let is_normalized t = `Yes

let arguments_table =
  let table = Hashtbl.create 67 in
  List.iter (fun name -> Hashtbl.add table name Marg.unit_ignore)
    ocaml_ignored_flags;
  List.iter (fun name -> Hashtbl.add table name Marg.param_ignore)
    ocaml_ignored_parametrized_flags;
  let add prj upd (name,flag,_doc) =
    assert (not (Hashtbl.mem table name));
    Hashtbl.add table name (Marg.lens prj upd flag)
  in
  List.iter
    (add (fun x -> x.ocaml) (fun x ocaml -> {x with ocaml}))
    ocaml_flags;
  List.iter
    (add (fun x -> x.findlib) (fun x findlib -> {x with findlib}))
    findlib_flags;
  List.iter
    (add (fun x -> x.merlin) (fun x merlin -> {x with merlin}))
    merlin_flags;
  table

let try_parse_argument ~warning args ocaml =
  match args with
  | [] -> None
  | arg :: args ->
    match Hashtbl.find arguments_table arg with
    | exception Not_found -> None
    | action -> match action args ocaml with
      | result -> Some result
      | exception (Failure msg) ->
        warning ("flag " ^ arg ^ " " ^ msg);
        Some (args, ocaml)
      | exception exn ->
        warning ("flag " ^ arg ^ ": error, " ^ Printexc.to_string exn);
        Some (args, ocaml)

let parse_arguments ~warning =
  let rec normal_parsing args ocaml =
    match try_parse_argument ~warning args ocaml with
    | Some (args, ocaml) -> normal_parsing args ocaml
    | None -> match args with
      | _ :: args -> resume_parsing args ocaml
      | [] -> ocaml
  and resume_parsing args ocaml =
    match args with
    | arg :: args when not (Hashtbl.mem arguments_table arg) ->
      normal_parsing args ocaml
    | args -> normal_parsing args ocaml
  in
  normal_parsing

let document_arguments oc =
  let print_doc flags =
    List.iter (fun (name,_flag,doc) -> Printf.fprintf oc "  %s\t%s\n" name doc)
      flags
  in
  output_string oc "Flags affecting Merlin:\n";
  print_doc merlin_flags;
  output_string oc "Flags affecting OCaml frontend:\n";
  print_doc ocaml_flags;
  output_string oc "Flags affecting Findlib behavior:\n";
  print_doc findlib_flags;
  output_string oc "Accepted but ineffective compiler flags:\n";
  List.iter (Printf.fprintf oc "  %s\n") ocaml_ignored_flags;
  List.iter (Printf.fprintf oc "  %s _\n") ocaml_ignored_parametrized_flags