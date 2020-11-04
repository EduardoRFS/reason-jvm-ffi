open Reason_jvm_ffi_ir;
open Utils;

let gen_runtimelib_jni_getter = field => {
  let runtimelib_name = jvm_type_to_runtimelib_name(field.jf_type);
  let prefix = field.jf_static ? "get_static" : "get";
  let name = prefix ++ "_" ++ runtimelib_name ++ "_field";
  Longident.Ldot(Lident("Jvm_ffi_runtime"), name) |> loc |> pexp_ident;
};
let gen_runtimelib_jni_setter = field => {
  let runtimelib_name = jvm_type_to_runtimelib_name(field.jf_type);
  let prefix = field.jf_static ? "set_static" : "set";
  let name = prefix ++ "_" ++ runtimelib_name ++ "_field";
  Longident.Ldot(Lident("Jvm_ffi_runtime"), name) |> loc |> pexp_ident;
};
let gen_runtimelib_ref_make = field => {
  let name = estring(field.jf_name);
  let signature = field.jf_type |> jvm_type_to_string |> estring;
  let getter = gen_runtimelib_jni_getter(field);
  let setter = gen_runtimelib_jni_setter(field);
  let function_to_call =
    field.jf_static
      ? [%expr Jvm_ffi_runtime.make_global_variable]
      : [%expr Jvm_ffi_runtime.make_field];
  let field_source =
    field.jf_static
      // TODO: remove this find_class
      ? find_class_expr(field.jf_classpath) : evar(this_id);
  pexp_apply(
    function_to_call,
    [
      (Labelled("name"), name),
      (Labelled("signature"), signature),
      (Labelled("getter"), getter),
      (Labelled("setter"), setter),
      (Nolabel, field_source),
    ],
  );
};
let gen_field = field => {
  let ref_make = gen_runtimelib_ref_make(field);
  field.jf_static
    ? ref_make : pexp_fun([(Nolabel, pvar(this_id))], ref_make);
};
