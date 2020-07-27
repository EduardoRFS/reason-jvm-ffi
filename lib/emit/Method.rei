open Basic_types;
let make:
  (
    ~java_name: string,
    ~java_signature: string,
    ~name: string,
    ~kind: [ | `Constructor | `Method | `Function],
    ~parameters: list((string, java_type)),
    ~return_type: java_type
  ) =>
  java_method;
let is_static: [ | `Constructor | `Method | `Function] => bool;
