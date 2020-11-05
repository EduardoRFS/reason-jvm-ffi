module Print = {
  let flush = format => Format.pp_print_flush(format, ());
  let structure_reason = (~format=Format.std_formatter, structure) => {
    let structure = Reason_toolchain.From_current.copy_structure(structure);
    Reason_toolchain.RE.print_implementation_with_comments(
      format,
      (structure, []),
    );
    flush(format);
  };
  let structure_ocaml = (~format=Format.std_formatter, structure) => {
    Pprintast.structure(format, structure);
    flush(format);
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
  | `OCaml => Print.structure_ocaml(structure)
  | `Reason => Print.structure_reason(structure)
  };
};

open Cmdliner;

let print = {
  let doc = "print in one of: (ml | re)";
  let opts = Arg.enum([("ml", `OCaml), ("re", `Reason)]);
  Arg.(
    value & opt(opts, `Reason) & info(["p", "print"], ~docv="FORM", ~doc)
  );
};
let files = Arg.(value & pos_all(file, []) & info([], ~docv="FILE"));

let generate_t = Term.(const(generate) $ print $ files);
let () =
  Term.exit @@ Term.eval((generate_t, Term.info("reason-jvm-generate")));
