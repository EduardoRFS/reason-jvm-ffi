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
