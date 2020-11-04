type jvm_classpath = string;
type jvm_type =
  | Boolean
  | Byte
  | Char
  | Short
  | Int
  | Long
  | Float
  | Double
  | Object(jvm_classpath)
  | Array(jvm_type);

type jvm_method = {
  jm_classpath: jvm_classpath,
  jm_name: string,
  jm_parameters: list((option(string), jvm_type)),
  jm_return: option(jvm_type),
  jm_kind: [ | `Constructor | `Method | `Function],
};
