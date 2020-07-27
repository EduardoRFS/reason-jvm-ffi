open Emit_Helper;
open Basic_types;

let this = {
  label: Nolabel,
  pat: pvar("this"),
  expr: evar("this"),
  typ: [%type: Jni.obj],
  jni_argument: None,
};

let emit_make_field = (name, java_signature, static, type_, _env) => {
  let emit_jni_field_access = (kind, static, type_) =>
    Java_Type.emit_camljava_jni_to_call(kind, static, type_);
  let call =
    pexp_apply(
      [%expr JavaFFI.make_field],
      [
        (Labelled("name"), estring(name)),
        (Labelled("signature"), estring(java_signature)),
        (Labelled("getter"), emit_jni_field_access(`Getter, static, type_)),
        (Labelled("setter"), emit_jni_field_access(`Setter, static, type_)),
        (Nolabel, this.expr),
      ],
    );
  // TODO: cast objects, getter and setter individually;
  pexp_fun_helper([(this.label, this.pat)], call);
};
let emit_type = java_type => {
  let content_type = Java_Type.emit_type(java_type);
  ptyp_constr(loc(lident("ref")), [content_type]);
};

let make = (~java_signature, ~name, ~static, ~java_type: java_type) => {
  let signature = emit_type(java_type);
  let make_field = emit_make_field(name, java_signature, static, java_type);
  {signature, make_field, name, static};
};
