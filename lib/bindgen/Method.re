open Reason_jvm_ffi_ir;
open Utils;

let parameters_with_identifier = parameters =>
  parameters
  |> List.mapi((i, (name, jvm_type)) => {
       switch (name) {
       | Some(name) => (`Labelled, name, jvm_type)
       | None => (`Nolabel, "param_" ++ string_of_int(i + 1), jvm_type)
       }
     });
let gen_parameters = (method, parameters_with_identifier) => {
  let implicit_parameters =
    switch (method.jm_kind) {
    | `Method => [(Asttypes.Nolabel, pvar(this_id))]
    | _ => []
    };
  let explicit_parameters =
    parameters_with_identifier
    |> List.map(((kind, name, _)) => {
         let label =
           switch (kind) {
           | `Nolabel => Asttypes.Nolabel
           | `Labelled => Asttypes.Labelled(name)
           };
         (label, pvar(name));
       });
  let explicit_parameters =
    switch (explicit_parameters) {
    | [] => [(Asttypes.Nolabel, punit)]
    | parameters => parameters
    };
  implicit_parameters @ explicit_parameters;
};
let gen_argument = ((_, identifier, jvm_type)) => {
  let gen_array_tojobject = identifier => [%expr
    Jvm_ffi_runtime.Array.to_jobject([%e identifier])
  ];
  let identifier = Ast_builder.evar(identifier);

  switch (jvm_type) {
  | Boolean => id([%expr Jvm_ffi_runtime.Boolean([%e identifier])])
  | Byte => id([%expr Jvm_ffi_runtime.Byte([%e identifier])])
  | Char => id([%expr Jvm_ffi_runtime.Char([%e identifier])])
  | Short => id([%expr Jvm_ffi_runtime.Short([%e identifier])])
  | Int => id([%expr Jvm_ffi_runtime.Int([%e identifier])])
  | Long => id([%expr Jvm_ffi_runtime.Long([%e identifier])])
  | Float => id([%expr Jvm_ffi_runtime.Float([%e identifier])])
  | Double => id([%expr Jvm_ffi_runtime.Double([%e identifier])])
  | Object(_) => id([%expr Jvm_ffi_runtime.Obj([%e identifier])])
  | Array(_) =>
    id([%expr Jvm_ffi_runtime.Obj([%e gen_array_tojobject(identifier)])])
  };
};
let gen_runtime_jni_caller_name = method => {
  let type_name =
    switch (method.jm_return) {
    | Some(jvm_type) => jvm_type_to_runtimelib_name(jvm_type)
    | None => "void"
    };
  let prefix =
    switch (method.jm_kind) {
    | `Function => "call_static"
    | `Constructor
    | `Method => "call"
    };
  let name = prefix ++ "_" ++ type_name ++ "_method";
  Longident.Ldot(Lident("Jvm_ffi_runtime"), name) |> loc |> pexp_ident;
};
let gen_runtime_call = (method, parameters_with_identifier) => {
  let name = method.jm_name |> estring;
  let signature = jvm_method_to_signature(method) |> estring;
  let runtime_jni_caller = gen_runtime_jni_caller_name(method);
  // TODO: remove this find_class
  let find_class_expr = find_class_expr(method.jm_classpath);
  let arguments =
    parameters_with_identifier
    |> List.map(gen_argument)
    |> Ast_builder.pexp_array;
  let runtime_to_call =
    switch (method.jm_kind) {
    | `Method =>
      id(
        [%expr
          Jvm_ffi_runtime.call_method(
            ~name=[%e name],
            ~signature=[%e signature],
            [%e runtime_jni_caller],
            [%e evar(this_id)],
            [%e arguments],
          )
        ],
      )
    | `Constructor =>
      id(
        [%expr
          Jvm_ffi_runtime.call_constructor(
            ~signature=[%e signature],
            [%e find_class_expr],
            [%e arguments],
          )
        ],
      )
    | `Function =>
      id(
        [%expr
          Jvm_ffi_runtime.call_function(
            ~name=[%e name],
            ~signature=[%e signature],
            [%e runtime_jni_caller],
            [%e find_class_expr],
            [%e arguments],
          )
        ],
      )
    };

  switch (method.jm_return) {
  | Some(Array(_)) =>
    id([%expr Jvm_ffi_runtime.Array.unsafe_of_jobject([%e runtime_to_call])])
  | _ => runtime_to_call
  };
};
let gen_method = method => {
  let parameters_with_identifier =
    parameters_with_identifier(method.jm_parameters);
  let runtime_call = gen_runtime_call(method, parameters_with_identifier);
  let parameters = gen_parameters(method, parameters_with_identifier);
  pexp_fun(parameters, runtime_call);
};
