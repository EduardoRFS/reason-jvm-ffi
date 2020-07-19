// TODO: access, final and static
[@deriving show]
type t = {
  name: string,
  static: bool,
  kind: Java_Type.t,
};
