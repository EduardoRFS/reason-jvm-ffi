open Emit_Helper;
open Basic_types;

type t = java_field;

let emit_jni_field_access = (getter_or_setter, t) => {
  let type_name =
    switch (t.kind) {
    | Object(_)
    | Array(_) => "object"
    | java_type => Java_Type.to_code_name(java_type)
    };
  let function_name =
    switch (getter_or_setter, t.static) {
    | (`Getter, true) => "get_static_" ++ type_name ++ "_field"
    | (`Getter, false) => "get_" ++ type_name ++ "_field"
    | (`Setter, true) => "set_static_" ++ type_name ++ "_field"
    | (`Setter, false) => "set_" ++ type_name ++ "_field"
    };
  evar(~modules=["Jni"], function_name);
};
let emit_make_reference = (clazz_id, object_id, field_id, t) => {
  let create_getter_or_setter = kind => {
    let function_to_call = emit_jni_field_access(kind, t);
    eapply(
      function_to_call,
      [t.static ? evar(clazz_id) : evar(object_id), evar(field_id)],
    );
  };
  let getter = create_getter_or_setter(`Getter);
  let getter = pexp_fun(Nolabel, None, punit, getter);
  let setter = create_getter_or_setter(`Setter);
  eapply([%expr Ref.make], [getter, setter]);
};

let object_id = "this";
let emit = (jni_class_name, t: t) => {
  // TODO: escape these names + jni_class_name
  let field_id = "jni_fieldID";
  let declare_field_id =
    eapply(
      [%expr Jni.get_fieldID],
      [
        eapply(evar(jni_class_name), [eunit]),
        estring(t.name),
        estring(t.java_signature),
      ],
    );
  let declare_function = {
    let make_reference =
      emit_make_reference(jni_class_name, object_id, field_id, t);
    let parameter = t.static ? pvar(jni_class_name) : pvar(object_id);
    pexp_fun(Nolabel, None, parameter, make_reference);
  };
  %expr
  {
    let [%p pvar(field_id)] = [%e declare_field_id];
    %e
    declare_function;
  };
};
let emit_type = (kind, t) => {
  let content_type = Java_Type_Emit.emit_type(t.kind);
  let rtype = ptyp_constr(Located.mk(lident("ref")), [content_type]);

  switch (kind) {
  | `Field => rtype
  | `Unsafe =>
    let make_type = t.static ? [%type: unit => Jni.clazz] : [%type: Jni.obj];
    ptyp_arrow(Nolabel, make_type, rtype);
  };
};
