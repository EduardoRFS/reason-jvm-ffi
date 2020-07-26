open Basic_types;
let make:
  (
    ~java_signature: string,
    ~name: string,
    ~static: bool,
    ~java_type: java_type
  ) =>
  java_field;
