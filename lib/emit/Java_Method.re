open Fun;
open Emit_Helper;
open Basic_types;
open Structures;

type parameter = Basic_types.parameter;
type t = java_method;

// TODO: this seems hackish as hell
type internal_t = {
  java_name: string,
  java_signature: string,
  name: string,
  kind: [ | `Constructor | `Method | `Function],
  this: option(parameter),
  parameters: list(parameter),
  return_type: java_type,
};
let is_static = kind => kind == `Function;

let find_required_classes = (parameters, return_type) =>
  (
    parameters
    |> List.concat_map(((_, java_type)) =>
         Java_Type.find_required_class(java_type)
       )
  )
  @ Java_Type.find_required_class(return_type);

let emit_argument = (identifier, java_type) => {
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

let parse_parameters = (kind, parameters) => {
  let parameters =
    switch (parameters) {
    | [] => [
        {
          label: Nolabel,
          pat: punit,
          expr: eunit,
          typ: typ_unit,
          jni_argument: None,
          java_type: Void,
        },
      ]
    | parameters =>
      parameters
      |> List.map(((name, java_type)) =>
           {
             label: Labelled(name),
             pat: pvar(name),
             expr: evar(name),
             // TODO: look at open object types later
             typ: Java_Type_Emit.emit_type(java_type),
             jni_argument: Some(emit_argument(evar(name), java_type)),
             java_type,
           }
         )
    };
  let additional =
    is_static(kind)
      ? None
      : Some({
          label: Nolabel,
          pat: pvar("this"),
          expr: evar("this"),
          typ: [%type: Jni.obj],
          jni_argument: None,
          java_type: Void,
        });
  (additional, parameters);
};

let emit_type = (parameters, return_type) => {
  let parameters =
    parameters |> List.map(({label, typ, _}) => (label, typ));
  let return_type = Java_Type_Emit.emit_type(return_type);
  ptyp_arrow_helper(parameters, return_type);
};

let emit_jni_method_name = (t: internal_t) =>
  Java_Type_Emit.emit_camljava_jni_to_call(
    `Method,
    t.kind == `Function,
    t.kind == `Constructor ? Void : t.return_type,
  );
let emit_method_call =
    (env, clazz_id, object_id, method_id, args, t: internal_t) => {
  let args =
    args |> List.filter_map(param => param.jni_argument) |> pexp_array;
  let call_method =
    eapply(
      emit_jni_method_name(t),
      [is_static(t.kind) ? clazz_id : evar(object_id), method_id, args],
    );
  let returned_value =
    switch (t.kind) {
    | `Constructor =>
      let call_return =
        pexp_let(
          Nonrecursive,
          [value_binding(~pat=punit, ~expr=call_method)],
          evar(object_id),
        );
      let allocate_object =
        pexp_let(
          Nonrecursive,
          [
            value_binding(
              ~pat=pvar(object_id),
              ~expr=[%expr Jni.alloc_object([%e clazz_id])],
            ),
          ],
          call_return,
        );
      allocate_object;
    | _ => call_method
    };

  unsafe_cast_returned_value(env, t.return_type, returned_value);
};

let object_id = "this";
// TODO: escape these names + jni_class_name
let method_id = "jni_methodID";

let emit_jni_get_methodID = (clazz, t: internal_t) =>
  eapply(
    is_static(t.kind)
      ? [%expr Jni.get_static_methodID] : [%expr Jni.get_methodID],
    [clazz, estring(t.java_name), t.java_signature |> estring],
  );

let emit_call_jni = (t: internal_t, ~clazz, env) => {
  let parameters = {
    let parameters =
      t.parameters |> List.map(({label, pat, _}) => (label, pat));
    switch (t.this) {
    | Some({label, pat, _}) => [(label, pat), ...parameters]
    | None => parameters
    };
  };

  let method_call =
    emit_method_call(
      env,
      clazz,
      object_id,
      evar(method_id),
      t.parameters,
      t,
    );

  // parameters => method_call
  let declare_function = pexp_fun_helper(parameters, method_call);

  pexp_let_alias(
    method_id,
    emit_jni_get_methodID(clazz, t),
    declare_function,
  );
};

let make =
    (~java_name, ~java_signature, ~name, ~kind, ~parameters, ~return_type) => {
  let required_classes = find_required_classes(parameters, return_type);
  let (this, parameters) = parse_parameters(kind, parameters);

  let internal_t = {
    java_name,
    java_signature,
    name,
    kind,
    parameters,
    this,
    return_type,
  };

  let signature = emit_type(parameters, return_type);
  let call_jni = emit_call_jni(internal_t);
  {required_classes, signature, call_jni, name, kind};
};
