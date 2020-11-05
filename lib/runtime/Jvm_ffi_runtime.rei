module Ref: {
  type ref('a);
  let make: (unit => 'a, 'a => unit) => ref('a);
  let (^): ref('a) => 'a;
  let (:=): (ref('a), 'a) => unit;
};

type ref('a) = Ref.ref('a);

open Camljava;
include (module type of Camljava.Jni);

let make_field:
  (
    ~name: string,
    ~signature: string,
    ~getter: (Jni.obj, Jni.fieldID) => 'a,
    ~setter: (Jni.obj, Jni.fieldID, 'a) => unit,
    Jni.obj
  ) =>
  Ref.ref('a);
let make_global_variable:
  (
    ~name: string,
    ~signature: string,
    ~getter: (Jni.clazz, Jni.fieldID) => 'a,
    ~setter: (Jni.clazz, Jni.fieldID, 'a) => unit,
    Jni.clazz
  ) =>
  Ref.ref('a);

let call_constructor:
  (~signature: string, Jni.clazz, array(Jni.argument)) => Jni.obj;
let call_method:
  (
    ~name: string,
    ~signature: string,
    (Jni.obj, Jni.methodID, array(Jni.argument)) => 'a,
    Jni.obj,
    array(Jni.argument)
  ) =>
  'a;
let call_function:
  (
    ~name: string,
    ~signature: string,
    Jni.clazz,
    (Jni.clazz, Jni.methodID, array(Jni.argument)) => 'a,
    array(Jni.argument)
  ) =>
  'a;

let set_static_object_field: (Jni.clazz, Jni.fieldID, Jni.obj) => unit;
