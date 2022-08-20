# Configuration 

The REPL comes with a configuration file called `config.edn`. It's contents can be changed.

A cheat-sheet can be found [here](cheat-sheet.md).

## Key bindings
A list of every supported and/or unsupported key/binding can be found [here](cheat-sheet.md#key-bindings).

**Note: Due to either operating system key priorities and/or the terminal backend,
some key bindings might not work directly out of the box.**


Key bindings are defined as a map of the form:

```clojure
{ :key   <key> 
  :ctrl  <boolean> 
  :alt   <boolean> 
  :shift <boolean> }
```

Example:
```clojure
{:key \a :ctrl true}
```

### `:key`

Expected key to be pressed. Can be either:
1. A simple clojure character:
```clojure
{:key \c}
```

2. Special key:
```clojure
{:key :enter}
```

### `:ctrl`, `:alt`, `:shift`
Boolean values that specify if of them should also be pressed.\
Any combination of these is allowed.

**Fields set to `false` can be omitted from the map**.

## Syntax highlighting

Syntax highlights is represented as a map of the form `{ <construct> <color> }`.

The colours can either be the:
1. Preset colours
```clojure
:yellow, :white, :red 
```
2. Custom RGB colours
```clojure
[123, 44, 15]
```
A list of all supported constructs and colours can be found [here](cheat-sheet.md#syntax-highlighting).

Example:
```clojure
{ :lists   :white,
  :strings [22, 45, 91]
  ...}
```
 
**Note: Multi-line string highlighting is not supported.**

## Fonts

The default font used is [Hasklig](https://github.com/i-tu/Hasklig).

Both the font itself and its size can be changed.\
To change them, specify the path to a different `otf` file and/or a different number.

Details about the config fields can be found [here](cheat-sheet.md#fonts).

## History

Evaluations of a REPL session are persisted to a file.\
The size of history, as well as the file it's stored into, can be changed.

To change them, simply specify a path to another file and/or a different number.

Details about the config fields can be found [here](cheat-sheet.md#history).