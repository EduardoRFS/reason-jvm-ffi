[@deriving (show, eq, ord)]
type class_name = {
  package: list(string),
  name: string,
};

[@deriving show]
type java_type =
  // TODO: remove void
  | Void
  | Boolean
  | Byte
  | Char
  | Short
  | Int
  // TODO: maybe Camlint?
  | Long
  | Float
  | Double
  | Object(class_name)
  | Array(java_type);

// TODO: access, final and static
[@deriving show]
type java_field = {
  java_signature: string,
  name: string,
  static: bool,
  kind: java_type,
};

// TODO: access
[@deriving show]
type java_method = {
  java_name: string,
  java_signature: string,
  name: string,
  static: bool,
  parameters: list((string, java_type)),
  return_type: java_type,
};

// TODO: access
[@deriving show]
type java_class = {
  java_name: class_name,
  name: class_name,
  extends: option(class_name),
  fields: list(java_field),
  // TODO: maybe static should be separated?
  methods: list(java_method),
};

module Java_Env =
  Map.Make({
    type t = class_name;
    let compare = compare_class_name;
  });

module Structures = {
  open Emit_Helper;

  // TODO: do this properly because apply
  let concat_lid = lids =>
    lids |> List.map(Longident.name) |> String.concat(".") |> Longident.parse;

  // TODO: do this properly, what if package is empty?
  let package_lid = package =>
    package
    |> List.map(String.capitalize_ascii)
    |> String.concat(".")
    |> Longident.parse;
  let class_lid = ({name, package}) =>
    concat_lid(
      package == []
        ? [Lident(name)] : [package_lid(package), Lident(name)],
    );

  let unsafe_name = name => "unsafe_" ++ name;
  let unsafe_lid = name => Lident(unsafe_name(name));
  let unsafe_module_lid = Longident.parse("Unsafe.Please.Stop");
  let unsafe_module = content => [%stri
    module Unsafe = {
      module Please = {
        module Stop = {
          %s
          content;
        };
      };
    }
  ];
  let unsafe_module_type = content => [%sigi:
    module Unsafe: {module Please: {module Stop: {[%%s content];};};}
  ];
  let unsafe_class_lid = class_name =>
    concat_lid([
      class_lid(class_name),
      unsafe_module_lid,
      Lident("Class"),
      unsafe_lid("t"),
    ]);

  let get_unsafe_jobj = id => pexp_send(id, loc("get_jni_jobj"));

  let unsafe_class_cast = (class_name, jobj) => {
    let new_fn = pexp_new(unsafe_class_lid(class_name) |> loc);
    eapply(new_fn, [jobj]);
  };

  // TODO: should we trust the Java return? I have a bad feeling on that
  let unsafe_cast_returned_value = (return_type, returned_value) => {
    switch (return_type) {
    | Object(class_name) => unsafe_class_cast(class_name, returned_value)
    | Array(_) => failwith("TODO: too much work bro")
    | _ => returned_value
    };
  };
};

/**
  this is an optimization pass to make the access of values local
*/
module Relativize = {
  /** so if you have a same package access it doesn't go through the full path */
  let class_name = (clazz_id, class_name) => {
    clazz_id.package == class_name.package
      ? {package: [], name: class_name.name} : class_name;
  };
  let java_type = clazz_id =>
    fun
    | Object(object_type) => {
        let object_type = class_name(clazz_id, object_type);
        Object(object_type);
      }
    | java_type => java_type;
  let java_field = (clazz_id, field) => {
    ...field,
    kind: java_type(clazz_id, field.kind),
  };
  let java_method = (clazz_id, method) => {
    let relativize = java_type(clazz_id);
    let parameters =
      method.parameters
      |> List.map(((name, value)) => (name, relativize(value)));
    let return_type = relativize(method.return_type);
    {...method, parameters, return_type};
  };
  let java_class = (clazz_id, java_class) => {
    let name = class_name(clazz_id, java_class.name);
    let extends = Option.map(class_name(clazz_id), java_class.extends);
    let fields = java_class.fields |> List.map(java_field(clazz_id));
    let methods = java_class.methods |> List.map(java_method(clazz_id));
    {...java_class, name, extends, fields, methods};
  };
  let java_env = (clazz_id, env) => Java_Env.map(java_class(clazz_id), env);
};
