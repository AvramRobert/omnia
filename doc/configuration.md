## Configuration 

The default keymap and colour scheme can be found here:
* [Keymap](https://github.com/AvramRobert/omnia/blob/master/doc/keymap.md)
* [Colourscheme](https://github.com/AvramRobert/omnia/blob/master/doc/colourscheme.md)

Omnia has a configuration file called `omnia.edn`, wherein one can configure its key bindings, 
and syntax highlighting.

### Key bindings
Each operation omnia supports is bound to a unique key or combination of keys. 
Every keybind is defined as a map similar to the following: <br />

```clojure
{ :key   <key> 
  :ctrl  <boolean> 
  :alt   <boolean> 
  :shift <boolean> }
```

`:key` denotes the key to be pressed, whilst `ctrl`, `alt` and `shift` denote if 
`:key` should be pressed together with either `ctrl`, `alt` or `shift`. <br /> 

For simple character keys, the value of that `:key` field should just be a normal Clojure character. <br />

Example - I want something to happen when I press "c": <br />

```clojure
{:key \c}
```

For more esoteric keys, like `enter`, `space`, `tab` etc, the keyword version of the name of that specific 
key is required. <br />

Example - I want something to happen when I press `enter`: <br />

```clojure
{:key :enter}
```

`ctrl`, `alt` and `shift` are supported as `boolean` flags and used in conjunction with the `:key` field. <br />

Example - I want something to happen when I press `ctrl`, `alt` and `e`: <br /> 

```clojure
{:key  \e 
 :alt  true
 :ctrl true}
```

### Supported keys
**NOTE**: Please keep in mind that all the character keys, `ENTER`, `SPACE`, `BACKSPACE`, `DELETE`
and the arrow keys are used for actual input. I would advise not reserving them for other operations. <br />
Combinations of key + ctrl/alt/shift are however open. 
 
 * All character keys
 * `:left, :right, :up, :down` (the arrows)
 * `:enter`
 * `:space`
 * `:backspace`
 * `:tab`
 * `:page-up, :page-down`
 * `:escape`
 * `:home`
 * `:end`
 * `:insert`
 * `:delete`
 * `:f1` through `:f19`

### Unsupported key combinations
Let's face it, operating systems are different and thus treat terminal emulators differently.
As such, there are various key combinations, that do not currently work:

 * CTRL + ENTER
 * CTRL + UP
 * CTRL + DOWN
 * CTRL + BACKSPACE

In addition to this, the `command` key on MacOS is not supported.

Should you discover additional issues, please feel free to report them so I can add them to the list.
Solutions for this issue are currently being investigated.

### Syntax highlighting

Syntax highlights are represented as a map, where the `key` 
denotes the highlighted _type_ of token and the `value` its colour.

Omnia can highlight the following things and defines the following keys for them:

- Function calls: `:function`
- Numbers: `:number`
- Strings: `:string`
- Keywords: `:keyword`
- Characters: `:char`
- Comments: `:comment`
- Lists: `:list`
- Vectors: `:vector`
- Maps: `:map`
- Special words: `:word`
- Simple text: `:text`
- Selections: `:selection`

To change the colour of any of these, just associate them with the desired colour 
in the map.

Omnia supports the following colours: 

 * `:black`
 * `:white`
 * `:red`
 * `:blue`
 * `:yellow`
 * `:cyan`
 * `:magenta`
 * `:green`
 * `:default` (transparent)
 
**NOTE** Multi-line string highlighting is not supported.
