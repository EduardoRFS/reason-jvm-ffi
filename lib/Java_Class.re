open Migrate_parsetree;
open Ast_410;
open Ast_helper;
open Asttypes;
open Longident;

let (let.some) = Option.bind;

type id = {
  package: string,
  name: string,
};
// TODO: proper lid
let id_to_lid = id => {
  let last_module = String.capitalize_ascii(id.name);
  let modules =
    id.package
    |> String.split_on_char('.')
    |> List.map(String.capitalize_ascii);
  let ident =
    switch (modules) {
    | [] => Ldot(Lident(last_module), "t")
    | [first_mod, ...remaining] =>
      let ldot =
        List.fold_left(
          (acc, md) => Ldot(acc, md),
          Lident(first_mod),
          remaining,
        );
      Ldot(Ldot(ldot, last_module), "t");
    };
  {txt: ident, loc: Location.none};
};

/** a fully qualified name, using / instead of . */
let id_to_jvm_name = id => {
  let package =
    id.package |> String.split_on_char('.') |> String.concat("/");
  package ++ "/" ++ id.name;
};

type t = {
  id,
  extends: option(id),
  // TODO: fields
  methods: list(Java_Method.t),
};

let emit = t => {
  let ident = name => Exp.ident({txt: Lident(name), loc: Location.none});
  let var = name => Pat.var({txt: name, loc: Location.none});

  let clazz_id = "jni_jclazz";
  let object_id = "jni_jobj";
  let declare_methods = List.concat_map(Java_Method.emit, t.methods);

  let method_fields =
    List.map(
      ({Java_Method.name, _}) =>
        Cf.method(
          {txt: name, loc: Location.none},
          Public,
          Cf.concrete(
            Fresh,
            [%expr [%e ident(name)]([%e ident(object_id)])],
          ),
        ),
      t.methods,
    );
  let inheritance_field = {
    let.some extends_id = t.extends;
    let extends_lid = id_to_lid(extends_id);
    let extends = Cl.constr(extends_lid, []);
    let apply_class = Cl.apply(extends, [(Nolabel, ident(object_id))]);
    Some(Cf.inherit_(Fresh, apply_class, None));
  };
  let fields =
    List.concat([
      List.filter_map(Fun.id, [inheritance_field]),
      method_fields,
    ]);
  let class_expr = fields |> Cstr.mk(Pat.any()) |> Cl.structure;
  let class_fun =
    Cl.fun_(Asttypes.Nolabel, None, var(object_id), class_expr);
  let class_declaration =
    Ci.mk(
      {txt: t.id.name |> String.lowercase_ascii, loc: Location.none},
      class_fun,
    );

  let find_class = {
    let name = id_to_jvm_name(t.id) |> Const.string |> Exp.constant;
    [%str let [%p var(clazz_id)] = Jni.find_class([%e name])];
  };
  List.concat([
    find_class,
    declare_methods,
    [Str.class_([class_declaration])],
  ]);
};
