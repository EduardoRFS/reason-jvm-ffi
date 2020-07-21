open Fun;
open Emit_Helper;
open Basic_types;
open Structures;

let find_required_classes = t =>
  List.concat_map(
    ((_, java_type)) => Java_Type.find_required_class(java_type),
    t.parameters,
  )
  @ Java_Type.find_required_class(t.return_type);

let emit_argument = ((name, java_type)) => {
  let identifier = evar(name);
  let read_expr =
    switch (java_type) {
    | Object(_) => get_unsafe_jobj(identifier)
    | Array(_) => failwith("TODO: ")
    // | Array(_) => [%expr Jni.array_to_jobj([%e identifier])]
    | _ => identifier
    };

  switch (java_type) {
  | Void => failwith("void cannot be an argument")
  | Boolean => id([%expr Boolean([%e read_expr])])
  | Byte => id([%expr Byte([%e read_expr])])
  | Char => id([%expr Char([%e read_expr])])
  | Short => id([%expr Short([%e read_expr])])
  | Int => id([%expr Int([%e read_expr])])
  | Long => id([%expr Long([%e read_expr])])
  | Float => id([%expr Float([%e read_expr])])
  | Double => id([%expr Double([%e read_expr])])
  | Object(_)
  | Array(_) => id([%expr Obj([%e read_expr])])
  };
};

let emit_jni_method_name = t =>
  Java_Type_Emit.emit_camljava_jni_to_call(`Method, t.static, t.return_type);
let emit_method_call = (clazz_id, object_id, method_id, args, t) => {
  let args = args |> List.map(emit_argument) |> pexp_array;
  let returned_value =
    eapply(
      emit_jni_method_name(t),
      [t.static ? clazz_id : object_id, method_id, args],
    );

  unsafe_cast_returned_value(t.return_type, returned_value);
};

let object_id = "this";
// TODO: escape these names + jni_class_name
let method_id = "jni_methodID";

let emit_jni_get_methodID = (jni_class_name, t) =>
  eapply(
    [%expr Jni.get_methodID],
    [
      eapply(evar(jni_class_name), [eunit]),
      estring(t.java_name),
      t.java_signature |> estring,
    ],
  );

let emit = (jni_class_name, t) => {
  // TODO: duplicated code between type and code
  let parameters = {
    let parameters =
      t.parameters
      |> List.rev_map(((name, _)) => (Labelled(name), pvar(name)));
    let parameters =
      switch (parameters) {
      | [] => [(Nolabel, punit)]
      | parameters => parameters
      };
    let additional_parameter =
      t.static
        ? [(Nolabel, pvar(jni_class_name))]
        : [(Nolabel, pvar(object_id))];
    List.append(parameters, additional_parameter);
  };

  let method_call =
    emit_method_call(
      // TODO: proper solve the problem of recursion + jni class
      eapply(evar(jni_class_name), [eunit]),
      evar(object_id),
      evar(method_id),
      t.parameters,
      t,
    );

  // parameters => method_call
  let declare_function = pexp_fun_helper(parameters, method_call);

  pexp_let_alias(
    method_id,
    emit_jni_get_methodID(jni_class_name, t),
    declare_function,
  );
};

let emit_type = (kind, t) => {
  let parameters = {
    let parameters =
      t.parameters
      |> List.rev_map(((name, value)) =>
           (Labelled(name), Java_Type_Emit.emit_type(value))
         );
    let parameters =
      switch (parameters) {
      | [] => [(Nolabel, typ_unit)]
      | parameters => parameters
      };
    let additional_parameter =
      switch (kind, t.static) {
      | (`Method, _) => []
      | (`Unsafe, true) => [(Nolabel, [%type: unit => Jni.clazz])]
      | (`Unsafe, false) => [(Nolabel, [%type: Jni.obj])]
      };
    List.append(parameters, additional_parameter);
  };
  let return_type = Java_Type_Emit.emit_type(t.return_type);
  ptyp_arrow_helper(parameters, return_type);
};
