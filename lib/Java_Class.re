open Java_Type;

let (let.some) = Option.bind;

[@deriving show]
type id = Object_Type.t;

[@deriving show]
type t = {
  id,
  extends: option(id),
  // TODO: fields
  methods: list(Java_Method.t),
};

let find_required_classes = t => {
  let extends =
    switch (t.extends) {
    | Some(extends) => Java_Type.find_required_class(Object(extends))
    | None => []
    };
  let methods =
    t.methods |> List.concat_map(Java_Method.find_required_classes);
  extends @ methods;
};
