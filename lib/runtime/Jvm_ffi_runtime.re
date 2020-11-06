module Ref = {
  type ref('a) = {
    getter: unit => 'a,
    setter: 'a => unit,
  };
  let make = (getter, setter) => {getter, setter};
  let (^) = ref_ => ref_.getter();
  let (:=) = (ref_, value) => ref_.setter(value);
};

type ref('a) = Ref.ref('a);

module Jni = Camljava.Jni;
include Jni;

module Array = {
  type t = Jni.obj;
  let unsafe_of_jobject = a => a;
  let to_jobject = a => a;
};

let make_field = (~name, ~signature, ~getter, ~setter, this) => {
  let clazz = Jni.get_object_class(this);
  let field_id = Jni.get_fieldID(clazz, name, signature);
  Ref.make(
    () => getter(this, field_id),
    value => setter(this, field_id, value),
  );
};

let make_global_variable = (~name, ~signature, ~getter, ~setter, clazz) => {
  let field_id = Jni.get_static_fieldID(clazz, name, signature);
  Ref.make(
    () => getter(clazz, field_id),
    value => setter(clazz, field_id, value),
  );
};

let call_method = (~name, ~signature, to_call, this, args) => {
  let clazz = Jni.get_object_class(this);
  let method_id = Jni.get_methodID(clazz, name, signature);
  to_call(this, method_id, args);
};
let call_function = (~name, ~signature, to_call, clazz, args) => {
  let method_id = Jni.get_static_methodID(clazz, name, signature);
  to_call(clazz, method_id, args);
};

let call_constructor = (~signature, clazz, args) => {
  // TODO: is okay to hard code this?
  let name = "<init>";
  let obj = Jni.alloc_object(clazz);
  call_method(~name, ~signature, Jni.call_void_method, obj, args);
  obj;
};

// TODO: this is hackish as hell
let set_static_object_field = (a, b, c) => {
  print_endline("static go");
  Camljava.Jni.set_static_obj_field(a, b, c);
};
