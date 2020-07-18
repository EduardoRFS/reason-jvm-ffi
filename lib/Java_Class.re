open Java_Type;

let (let.some) = Option.bind;

type id = Object_Type.t;

type t = {
  id,
  extends: option(id),
  // TODO: fields
  methods: list(Java_Method.t),
};
