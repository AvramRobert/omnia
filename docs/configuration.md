# Configuration 

The REPL comes with a configuration file called `config.edn`. It's contents can be changed.

A cheat-sheet can be found [here](cheat-sheet.md).

## Key bindings
Key bindings are defined as a map: <br />

```clojure
{ :key   <key> 
  :ctrl  <boolean> 
  :alt   <boolean> 
  :shift <boolean> }
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
Boolean values specifying if one of those also needs to be pressed.
Any combination of these is allowed.

A list of every supported and/or unsupported key/binding can be found [here](cheat-sheet.md#key-bindings).

**Note: Due to either operating system key priorities and/or the terminal backend,
some key bindings might not work directly out of the box.**

## Syntax highlighting

Syntax highlights is represented as a map in the form of `{ <construct> <color> }`.

The colours can either be the preset, named colours or custom RGB colours.

A list of all supported constructs and colours can be found [here](cheat-sheet.md#syntax-highlighting).

```clojure
{ :lists   :white,
  :strings [22, 45, 91]
  ...}
```
 
**Note: Multi-line string highlighting is not supported.**

## Fonts

Both the font itself and its size can be altered.

The default font used is [Hasklig](https://github.com/i-tu/Hasklig). 
To change it, specify the path to a different `otf` file.
The same goes for its size.

Details about the config fields can be found [here](cheat-sheet.md#fonts)

## History

Evaluations of a REPL session are persisted to a file.

Which file it is and how many evaluations are to be persisted can be changed.

Details about the config fields can be found [here](cheat-sheet.md#history)