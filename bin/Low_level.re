module Show = {
  let structure_reason = structure =>
    Format.asprintf(
      "%a",
      Reason_toolchain.RE.print_implementation_with_comments,
      (Reason_toolchain.From_current.copy_structure(structure), []),
    );

  let structure_ocaml = structure =>
    Format.asprintf("%a", Pprintast.structure, structure);

  let structure_binary = (~filename, structure) => {
    open Migrate_parsetree;
    open Reason_syntax_util;

    let structure =
      structure
      |> Reason_toolchain.From_current.copy_structure
      |> apply_mapper_to_structure(_, remove_stylistic_attrs_mapper)
      |> apply_mapper_to_structure(_, backport_letopt_mapper)
      |> Reason_toolchain.To_current.copy_structure;

    Ast_io.to_bytes(
      filename,
      Ast_io.Impl((module OCaml_current), structure),
    );
  };
};

let generate = (kind, files) => {
  open Reason_jvm_ffi_parse;
  open Reason_jvm_ffi_bindgen;

  let file =
    switch (files) {
    | [file] => file
    | _ => failwith("single file required")
    };
  let ic = file |> open_in_bin;
  let ir = Parse.parse_bytecode(ic);
  let structure = gen_class(ir);

  switch (kind) {
  | `OCaml => Show.structure_ocaml(structure) |> output_string(Stdlib.stdout)
  | `Reason =>
    Show.structure_reason(structure) |> output_string(Stdlib.stdout)
  | `Binary =>
    let filename = Filename.basename(file);
    Show.structure_binary(~filename, structure)
    |> output_bytes(Stdlib.stdout);
  };
};

open Cmdliner;

let print = {
  let doc = "print in one of: (ml | re | binary)";
  let opts =
    Arg.enum([("ml", `OCaml), ("re", `Reason), ("binary", `Binary)]);
  Arg.(
    value & opt(opts, `Reason) & info(["p", "print"], ~docv="FORM", ~doc)
  );
};

let files = Arg.(value & pos_all(file, []) & info([], ~docv="FILE"));

let generate_t = Term.(const(generate) $ print $ files);
let () =
  Term.exit @@ Term.eval((generate_t, Term.info("reason-jvm-low-level")));
