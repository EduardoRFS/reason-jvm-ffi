open Migrate_parsetree;
open Setup;
open Reason_jvm_ffi_ir;

describe("transform jvm_method to an expression", ({test, _}) => {
  let test = (name, input, output) => {
    let run_test = suite => {
      let input = Reason_jvm_ffi_bindgen.gen_method(input);
      Helpers.compare_expr(input, output, suite);
    };
    test(name, run_test);
  };

  test(
    "void method without parameter",
    {
      jm_classpath: "com/github/eduardorfs/RandomClass",
      jm_name: "doMagic",
      jm_parameters: [],
      jm_return: None,
      jm_kind: `Method,
    },
    [%expr
      (this, ()) =>
        Jvm_ffi_runtime.call_method(
          ~name="doMagic",
          ~signature="()V",
          Jvm_ffi_runtime.call_void_method,
          this,
          [||],
        )
    ],
  );
  test(
    "method without parameter",
    {
      jm_classpath: "com/github/eduardorfs/RandomClass",
      jm_name: "getWidth",
      jm_parameters: [],
      jm_return: Some(Int),
      jm_kind: `Method,
    },
    [%expr
      (this, ()) =>
        Jvm_ffi_runtime.call_method(
          ~name="getWidth",
          ~signature="()I",
          Jvm_ffi_runtime.call_int_method,
          this,
          [||],
        )
    ],
  );
  test(
    "void method with parameter",
    {
      jm_classpath: "com/github/eduardorfs/RandomClass",
      jm_name: "setWidth",
      jm_parameters: [(Some("width"), Int)],
      jm_return: None,
      jm_kind: `Method,
    },
    [%expr
      (this, ~width) =>
        Jvm_ffi_runtime.call_method(
          ~name="setWidth",
          ~signature="(I)V",
          Jvm_ffi_runtime.call_void_method,
          this,
          [|Jvm_ffi_runtime.Int(width)|],
        )
    ],
  );

  test(
    "method with multiple named parameters",
    {
      jm_classpath: "com/github/eduardorfs/RandomClass",
      jm_name: "callYwithX",
      jm_parameters: [
        (Some("y"), Object("com/github/eduardorfs/AnotherClass")),
        // TODO: should we actually map OCaml float -> Java float?
        (Some("x"), Float),
      ],
      jm_return: None,
      jm_kind: `Method,
    },
    [%expr
      (this, ~y, ~x) =>
        Jvm_ffi_runtime.call_method(
          ~name="callYwithX",
          ~signature="(Lcom/github/eduardorfs/AnotherClass;F)V",
          Jvm_ffi_runtime.call_void_method,
          this,
          // TODO: should objects have a wrapper like arrays?
          [|Jvm_ffi_runtime.Obj(y), Jvm_ffi_runtime.Float(x)|],
        )
    ],
  );
  test(
    "void method with array parameter",
    {
      jm_classpath: "com/github/eduardorfs/RandomClass",
      jm_name: "addItems",
      jm_parameters: [(Some("items"), Array(Int))],
      jm_return: None,
      jm_kind: `Method,
    },
    [%expr
      (this, ~items) =>
        Jvm_ffi_runtime.call_method(
          ~name="addItems",
          ~signature="([I)V",
          Jvm_ffi_runtime.call_void_method,
          this,
          [|Jvm_ffi_runtime.Obj(Jvm_ffi_runtime.Array.to_jobject(items))|],
        )
    ],
  );
  test(
    "array method with no parameter",
    {
      jm_classpath: "com/github/eduardorfs/RandomClass",
      jm_name: "getItems",
      jm_parameters: [],
      jm_return: Some(Array(Int)),
      jm_kind: `Method,
    },
    [%expr
      (this, ()) =>
        Jvm_ffi_runtime.Array.unsafe_of_jobject(
          Jvm_ffi_runtime.call_method(
            ~name="getItems",
            ~signature="()[I",
            Jvm_ffi_runtime.call_object_method,
            this,
            [||],
          ),
        )
    ],
  );
  test(
    "method with no named parameter",
    {
      jm_classpath: "com/github/eduardorfs/RandomClass",
      jm_name: "noNamedParameters",
      jm_parameters: [
        (None, Double),
        // TODO: Jni.Char(char) instead of Jni.Char(int)
        (None, Char),
      ],
      jm_return: None,
      jm_kind: `Method,
    },
    [%expr
      (this, param_1, param_2) =>
        Jvm_ffi_runtime.call_method(
          ~name="noNamedParameters",
          ~signature="(DC)V",
          Jvm_ffi_runtime.call_void_method,
          this,
          [|
            Jvm_ffi_runtime.Double(param_1),
            Jvm_ffi_runtime.Char(param_2),
          |],
        )
    ],
  );
  // constructors
  test(
    "constructor with no parameter",
    {
      jm_classpath: "com/github/eduardorfs/RandomClass",
      jm_name: "<init>",
      jm_parameters: [],
      // TODO: maybe do a variant, as constructor always has no return
      jm_return: None,
      jm_kind: `Constructor,
    },
    [%expr
      () =>
        Jvm_ffi_runtime.call_constructor(
          ~signature="()V",
          // TODO: avoid duplicated class instance
          Jvm_ffi_runtime.find_class("com/github/eduardorfs/RandomClass"),
          [||],
        )
    ],
  );
  test(
    "constructor with parameters",
    {
      jm_classpath: "com/github/eduardorfs/RandomClass",
      jm_name: "<init>",
      jm_parameters: [(Some("valid"), Boolean)],
      jm_return: None,
      jm_kind: `Constructor,
    },
    [%expr
      (~valid) =>
        Jvm_ffi_runtime.call_constructor(
          ~signature="(Z)V",
          Jvm_ffi_runtime.find_class("com/github/eduardorfs/RandomClass"),
          [|Jvm_ffi_runtime.Boolean(valid)|],
        )
    ],
  );
  // functions
  test(
    "void function with no parameter",
    {
      jm_classpath: "com/github/eduardorfs/RandomClass",
      jm_name: "logTuturu",
      jm_parameters: [],
      jm_return: None,
      jm_kind: `Function,
    },
    [%expr
      () =>
        Jvm_ffi_runtime.call_function(
          ~name="logTuturu",
          ~signature="()V",
          Jvm_ffi_runtime.call_static_void_method,
          Jvm_ffi_runtime.find_class("com/github/eduardorfs/RandomClass"),
          [||],
        )
    ],
  );
  test(
    "void function with int parameter",
    {
      jm_classpath: "com/github/eduardorfs/RandomClass",
      jm_name: "printInt",
      jm_parameters: [(Some("x"), Int)],
      jm_return: None,
      jm_kind: `Function,
    },
    [%expr
      (~x) =>
        Jvm_ffi_runtime.call_function(
          ~name="printInt",
          ~signature="(I)V",
          Jvm_ffi_runtime.call_static_void_method,
          Jvm_ffi_runtime.find_class("com/github/eduardorfs/RandomClass"),
          [|Jvm_ffi_runtime.Int(x)|],
        )
    ],
  );
  test(
    "int function with float parameter",
    {
      jm_classpath: "com/github/eduardorfs/RandomClass",
      jm_name: "toInt",
      jm_parameters: [(Some("float"), Float)],
      jm_return: Some(Int),
      jm_kind: `Function,
    },
    [%expr
      (~float) =>
        Jvm_ffi_runtime.call_function(
          ~name="toInt",
          ~signature="(F)I",
          Jvm_ffi_runtime.call_static_int_method,
          Jvm_ffi_runtime.find_class("com/github/eduardorfs/RandomClass"),
          [|Jvm_ffi_runtime.Float(float)|],
        )
    ],
  );
});

// Jvm_ffi_runtime.array_to_argument
