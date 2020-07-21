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
let unsafe_lid = name => Lident(unsafe_name(name));
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
  concat_lid([class_lid(class_name), unsafe_module_lid, unsafe_lid("t")]);

let get_unsafe_jobj = id => pexp_send(id, loc("get_jni_jobj"));

let unsafe_class_cast = (class_name, jobj) => {
  let new_fn = pexp_new(unsafe_class_lid(class_name) |> loc);
  eapply(new_fn, [jobj]);
};

// TODO: should we trust the Java return? I have a bad feeling on that
let unsafe_cast_returned_value = (return_type, returned_value) => {
  switch (return_type) {
  | Object(class_name) => unsafe_class_cast(class_name, returned_value)
  | Array(_) => failwith("TODO: too much work bro")
  | _ => returned_value
  };
};
