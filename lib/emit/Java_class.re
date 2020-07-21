open Emit_Helper;
open Basic_types;

// TODO: keep same method order as in the bytecode

let (let.some) = Option.bind;

let find_required_classes = t => {
  let extends =
    switch (t.extends) {
    | Some(extends) => Java_Type.find_required_class(Object(extends))
    | None => []
    };
  let methods =
    t.methods |> List.concat_map(Java_Method.find_required_classes);
  let requireds = extends @ methods;
  requireds |> List.filter((!=)(t.name));
};

let get_methods_by_kind = t =>
  t.methods  // TODO: this clearly shouldn't be here
  |> List.map(method => Relativize.java_method(t.name, method))
  |> List.partition(({static, _}) => static);

let jni_class_name = "unsafe_jni_class";
let object_id = "jni_jobj";

let emit_curried_method = method => {
  let modules = method.static ? ["Static"] : ["Methods"];
  eapply(evar(~modules, unsafe_name(method.name)), [evar(object_id)]);
};
