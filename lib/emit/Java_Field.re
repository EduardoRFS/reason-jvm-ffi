open Emit_Helper;
open Basic_types;
open Structures;

type t = java_field;

let emit_jni_field_access = (kind, t: java_field) =>
  Java_Type_Emit.emit_camljava_jni_to_call(kind, t.static, t.kind);
let emit_make_reference = (clazz_id, object_id, field_id, t) => {
  let create_getter_or_setter = kind => {
    let returned_value =
      eapply(
        emit_jni_field_access(kind, t),
        [t.static ? evar(clazz_id) : evar(object_id), evar(field_id)],
      );
    unsafe_cast_returned_value(t.kind, returned_value);
  };
  let getter = create_getter_or_setter(`Getter);
  let getter = pexp_fun(Nolabel, None, punit, getter);
  let setter = create_getter_or_setter(`Setter);
  eapply([%expr Ref.make], [getter, setter]);
};

let object_id = "this";
// TODO: escape these names + jni_class_name
let field_id = "jni_fieldID";

let emit = (jni_class_name, t: t) => {
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
    pexp_fun_helper([(Nolabel, parameter)], make_reference);
  };
  pexp_let_alias(field_id, declare_field_id, declare_function);
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
