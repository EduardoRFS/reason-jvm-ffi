open Emit_Helper;
open Basic_types;

// TODO: do this properly because apply
let concat_lid = lids =>
  lids |> List.map(Longident.name) |> String.concat(".") |> Longident.parse;

let class_lid = ({name, package}) => {
  let (first_name, remaining_path) =
    switch (package) {
    | [] => (name, [])
    | [first_name, ...remaining] => (
        first_name,
        List.append(remaining, [name]),
      )
    };
  remaining_path
  |> List.fold_left((lid, name) => Ldot(lid, name), Lident(first_name));
};

let unsafe_name = name => "unsafe_" ++ name;
let unsafe_module_lid = Longident.parse("Unsafe.Please.Stop");
let unsafe_module = content => [%stri
  module Unsafe = {
    module Please = {
      module Stop = {
        %s
        content;
      };
    };
  }
];
let unsafe_module_type = content => [%sigi:
  module Unsafe: {module Please: {module Stop: {[%%s content];};};}
];
let unsafe_class_lid = class_name =>
  concat_lid([
    class_lid(class_name),
    unsafe_module_lid,
    Lident(unsafe_name("t")),
  ]);

let new_unsafe_class = (class_name, jobj) => {
  let new_fn = pexp_new(unsafe_class_lid(class_name) |> Located.mk);
  eapply(new_fn, [jobj]);
};