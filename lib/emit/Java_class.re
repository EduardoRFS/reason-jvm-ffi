open Emit_Helper;
open Basic_types;

/*
   why two different functors?
   Because inheritance and recursive modes don't play well,
   the type system can even check it, but it will crash at runtime during boot
 */

// TODO: keep same method order as in the bytecode

let (let.some) = Option.bind;

let find_required_classes = t => {
  let extends =
    switch (t.extends) {
    | Some(extends) => Java_Type.find_required_class(Object(extends))
    | None => []
    };
  let methods =
    t.methods |> List.concat_map(method => method.required_classes);
  extends @ methods;
};

let jni_class_name = "unsafe_jni_class";
let object_id = "jni_jobj";

let emit_curried_method = (clazz, env, method) => {
  let static = Java_Method.is_static(method.kind);
  let find_lid = static ? Env.function_lid : Env.method_lid;
  let lid = find_lid(clazz, method.name, env) |> loc |> pexp_ident;
  let arg = static ? evar(jni_class_name) : evar(object_id);
  eapply(lid, [arg]);
};
