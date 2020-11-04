# Low-level API

The basic generation of this project is a low-level API the intent is to represent what the JVM would accept, any exception will be documented below.

## Arrays

Currently arrays are losing information as all arrays are treated as Object[], but there is enough information to know if an Array is any other primitve like int[].

## Constructors

They will act as Java constructors, instead of acting like methods, so you can only call a constructor if you're creating an object unlike with Reflection where you can call a constructor on any object
