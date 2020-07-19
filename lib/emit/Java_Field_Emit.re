open Emit_Helper;
open Java_Type;
open Java_Field;

let emit_field_access = (clazz_id, object_id, field_id, t) => {
  let id_to_call = {
    let type_name =
      switch (t.kind) {
      | Object(_)
      | Array(_) => "object"
      | java_type => Java_Type.to_code_name(java_type)
      };
    let function_name =
      t.static
        ? "get_static_" ++ type_name ++ "_field"
        : "get_" ++ type_name ++ "_field";
    evar(~modules=["Jni"], function_name);
  };
  eapply(id_to_call, [t.static ? clazz_id : object_id, field_id]);
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
    let field_access =
      emit_field_access(
        evar(jni_class_name),
        evar(object_id),
        evar(field_id),
        t,
      );
    let parameters = [
      (Nolabel, punit),
      t.static
        ? (Labelled(jni_class_name), pvar(jni_class_name))
        : (Labelled(field_id), pvar(field_id)),
    ];
    List.fold_left(
      (acc, (label, parameter)) => pexp_fun(label, None, parameter, acc),
      field_access,
      parameters,
    );
  };
  %expr
  {
    let [%p pvar(field_id)] = [%e declare_field_id];
    %e
    declare_function;
  };
};
