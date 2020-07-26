open Emit_Helper;
open Basic_types;

let emit_jni_field_access = (kind, static, type_) =>
  Java_Type_Emit.emit_camljava_jni_to_call(kind, static, type_);

let this = {
  label: Nolabel,
  pat: pvar("this"),
  expr: evar("this"),
  typ: [%type: Jni.obj],
  jni_argument: None,
};

let emit = (name, java_signature, static, type_, _env) => {
  let call =
    pexp_apply(
      [%expr id],
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
let emit_type = (kind, java_type, static) => {
  let content_type = Java_Type_Emit.emit_type(java_type);
  let rtype = ptyp_constr(Located.mk(lident("ref")), [content_type]);

  switch (kind) {
  | `Field => rtype
  | `Unsafe =>
    let make_type = static ? [%type: unit => Jni.clazz] : [%type: Jni.obj];
    ptyp_arrow(Nolabel, make_type, rtype);
  };
};

let make = (~java_signature, ~name, ~static, ~java_type: java_type) => {
  let signature = emit_type(`Field, java_type, static);
  let make_field = emit(name, java_signature, static, java_type);
  {signature, make_field, name, static};
};
