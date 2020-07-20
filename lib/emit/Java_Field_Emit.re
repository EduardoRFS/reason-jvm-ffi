open Emit_Helper;
open Java_Type;
open Java_Field;

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
let emit = (jni_class_name, t) => {
  // TODO: escape these names + jni_class_name
  let field_id = "jni_fieldID";
  let declare_field_id =
    eapply(
      [%expr Jni.get_fieldID],
      [
        evar(jni_class_name),
        estring(t.name),
        estring(Java_Type.to_jvm_signature(t.kind)),
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
let emit_type = (~is_unsafe=false, jni_class_name, t) => {
  let parameters = [
    (Nolabel, typ_unit),
    ...switch (is_unsafe, t.static) {
       | (false, _) => []
       | (true, true) => [(Labelled(jni_class_name), [%type: Jni.clazz])]
       | (true, false) => [(Labelled(object_id), [%type: Jni.obj])]
       },
  ];
  let return_type = Java_Type_Emit.emit_type(t.kind);
  List.fold_left(
    (typ, (label, parameter)) => ptyp_arrow(label, parameter, typ),
    return_type,
    parameters,
  );
};
