# mattyscript

Simple Lisp to JS Compiler.  Clojurescript is very nice, but too big for some applications.  Mattyscript gives you nice Clojure syntax (including destructuring) without cljs.core.

Recompile the output with webpack to get final minified js.

## Usage

### Special Forms

Operators such as `+` and `-` are compiled to JS equivalents rather than being treated as functions as in Clojure.  You can work around this limitation with anonymous functions.  For example to sum an array

```clojure
(.reduce [1 2 3] 0 #(+ %1 %2)) ;6
```

### Import

```clojure
(import "./my-file" [func1 func2])
```

### Let

Nice destructuring

```clojure
(let [
       [arg1 arg2 & rest] my-array
       {:keys [one two three]} my-object
       ]
       do-your-thing)
```

### Classes

```clojure
(class ParentClass
  (fn constructor []
    (super)
    (init)))

(class ^:export ^:default Subclass ParentClass
  (fn method-1 [arg1 {:keys [kind? good?] :as arg2}])
  (fn method-2 []))
```

Add `^:export` and `^:default` metadata as you see fit

### Functions

```clojure
(defn ^:export my-method [[a b :as arg1]] ...)
```

Functions also accept `^:export` and `^:default` metadata.

### Do

```clojure
(do
  (well-thats-a-bit-boring)
  (try-to-limit-side-effects))
```

### Methods and functions

```clojure
(.myMethod obj arg1 arg2)
(.-key obj)
```clojure

### Instantiation

```clojure
(new MyClass arg1 arg2)
;or
(MyClass. arg1 arg2)
```

### If, If else, when
```clojure
(let [
       gender? (if crazy? "girl" "boy")
      ...
      ])
```

`if` can be nested anywhere and returns a value, so its customary to use `when` for side effects.
You can also name side-effecting functions with exclamation marks.

```clojure
(when too-lazy-for-functional-programming?
  (do-some-side-effects!))
```

### Cond

```clojure
(cond
  ultra-crazy? "chinese girl"
  crazy? "girl"
  :default "boy")
```

### Get

Get is also a special form rather than a function
```clojure
(get {1 2} 1) ;2
```

Maps are not functions in Mattyscript, so remember to use `get`.

### For and Doseq

Two very useful features missing from regular JS

```clojure
(for [inner-array outer-array
      item inner-array
      :when (< item 5)]
      (+ item 1))

(doseq [i [1 2 3]] (console.log i))
```

The `:while` keyword is unimplemented because I'm lazy, however destructuring does work.
`for` can also be used to convert JS pseudo arrays into proper arrays, for example the Files object returned by a file selector

```clojure
(defn file-selector-callback [files]
  (let [
        ;believe it or not, files is not actually an array
        better-files (for [file files] file)
        ;but now it is
        ...))
```

### Apply

Where would we be without apply?

```clojure
(fn swap-state! [f & args]
  (set! this.state (apply f this.state args)))
```

### Try catch

Mentioned for completeness

```clojure
(try (throw (Error. "pointless")) (catch e (console.log "caught")))
```

### Literal

Because Google

```clojure
(literal "var functionFromGoogle = function() {...}")
```

### Keywords

Keywords don't exist in Javascript, so they are compiled to strings.

### Map and Set Literals

`{1 2}` compiles to `{1: 2}` and `#{"hi"}` compiles to `{"hi": "hi"}`.  Because Javascript object keys are always strings, it's advised to only put strings into sets.

### Hyperscript vs Arrays

`[1 2 3]` compiles to `[1, 2, 3]`
however `[:div {} 1 2 3]` compiles to h("div", {}, 1, 2, 3) which is useful for [hyperscript](https://github.com/hyperhype/hyperscript).  The distinction is that the second array has a keyword literal in first place.  Since keywords and strings are the same in Mattyscript, you can turn this feature off by changing the keyword to a string.

### Macros

The best has been saved for last.  Macros are defined in Clojure land

```clojure
(defmacro my-macro [arg1 arg2] ...)
```

To invoke them wrap them in an `expand` form

```clojure
(expand (my-macro arg1 arg2))
```

## Pull Requests

Are welcome.  If you are too lazy to read my source code, make your own compiler.  It's only a few hundred lines of code.

## License

Copyright © 2017 Matthew Molloy

Distributed under the Eclipse Public License either version 1.0 or (at
your option) any later version.
