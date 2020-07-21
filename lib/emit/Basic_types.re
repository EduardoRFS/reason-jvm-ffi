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
  name: class_name,
  extends: option(class_name),
  fields: list(java_field),
  // TODO: maybe static should be separated?
  methods: list(java_method),
};

module Structures = {
  open Emit_Helper;

  // TODO: do this properly because apply
  let concat_lid = lids =>
    lids |> List.map(Longident.name) |> String.concat(".") |> Longident.parse;

  let class_lid = ({name, package}) => {
    let (first_name, remaining_path) =
      switch (package) {
      | [] => (name, [])
      | [first_name, ...remaining] => (
          first_name,
          List.append(remaining, [name]),
        )
      };
    remaining_path
    |> List.fold_left((lid, name) => Ldot(lid, name), Lident(first_name));
  };

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
    concat_lid([class_lid(class_name), unsafe_module_lid, unsafe_lid("t")]);

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
  let class_name = (clazz, t) => {
    clazz.package == t.package ? {package: [], name: t.name} : t;
  };
  let java_type = clazz =>
    fun
    | Object(object_type) => {
        let object_type = class_name(clazz, object_type);
        Object(object_type);
      }
    | java_type => java_type;
  let java_method = (clazz, t) => {
    let relativize = java_type(clazz);
    let parameters =
      t.parameters
      |> List.map(((name, value)) => (name, relativize(value)));
    let return_type = relativize(t.return_type);
    {...t, parameters, return_type};
  };
};
