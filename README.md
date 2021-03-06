HasVis is a library which enables one to visualise the heap representation of a value in a GHCi session. The value is represented as a graph, and displayed as an interactive SVG image in the browser.

Graph layout is done using [WebCola](https://github.com/tgdwyer/WebCola), though only basic layout rules are used at the moment.

The library is inspired by [ghc-vis](http://felsin9.de/nnis/ghc-vis/). The main improvement is in the presentation - HasVis tries to improve readability by hiding all unnecessary information (e.g. type class dictionaries and other runtime artifacts), using a more sophisticated layout algorithm, and grouping lists into a single node where possible.

### Setup

- Install with `cabal install`.
- Run GHCi from the main directory.
- Type `:script ghci-script` to load the script into GHCi.

The library is confirmed to be working on Mac and Linux.

### Usage

The commands are:
- `:vis` - Initialise the visualisation (opens in browser).
- `:view x` - View the expression x.
- `:clear` - Clear the visualisation.
- `:close` - Close the visualisation.

The graph originates at a node labelled `Expression: <blah>`, generated by typing `:view <blah>` into GHCi. References from a node (a.k.a. arguments, where applicable) are ordered left-to-right beneath each node. The arguments of nodes representing data structures will be in the same order seen in the type declaration.

`:view` accepts any expression in scope in the GHCi session. It does not evaluate the expression. The component of an expression that is not evaluated will appear as an opaque "Thunk".

Be sure to annotate numeric values with a concrete type, since unresolved type class parameters will cause a value to show up as a thunk.

For example, `let x = [1,2,3]` has type `Num a => a` and will show up as a thunk. The user must annotate the expression with a concrete type, like: `let x = [1,2,3] :: [Int]`.

To view multiple expressions at once, simply view them as a tuple, e.g. `:view (x,y,z)`.

Graph nodes can be dragged with the mouse. Dragging the background will pan the view, and the scroll wheel can be used to zoom in and out.

### Features and Limitations

The library is mainly suited to visualising relatively small data structures. It can display the names of data constructors, such as `True`, `False`, or `Just a`, and will display the value of `Int`s and `Char`s. Currently, it does not show the values of `Integer`s or floating-point types.

Lists are treated specially. Instead of being represented as a series of "cons" (`:`) data constructors, they are lumped together into a traditional list format (i.e. `[1,2,3]` style), except when parts of the list are "shared" (i.e. have multiple references to them). For example, if the value '2' is shared in the list `[1,2,3]`, then the list will appear as `[1,_,3]`, with an edge from the list node to the node with '2' in it. If the tail of a list is shared, it will appear without a closing bracket, and an edge will point to the rest of the list.

This project began as an experiment on how to visualise the data structures and evaluation behaviour of functional (Haskell) programs to help users better understand how their code operates. Whilst this library may have some utility, it is severely limited by the lack of type (and other) information available in the heap of a GHC/GHCi-based Haskell program. This limitation means we can't see the type of thunks or functions on the heap, let alone the corresponding code, without modifications to GHC and/or its runtime. To contrast, the "code is data" approach and run time type information available in languages like those in the Lisp family make collecting this information far simpler.

GHC's evaluation model is also problematic. While we can view all the data that resides in the heap, much of the evaluation takes place on the stack. For example, the evaluation of a `fold` that produces an atomic result like an `Int` will have no intermediate state to show on the heap; the value will simply 'appear' when evaluation is forced.


