# omnia

A Clojure REPL for prototyping, experimenting and trying things
out. <br />

Omnia gives you almost all the power of a fully-fledged IDE, without
the actual IDE. 

## Installation
* Download the latest archived release: <br />
[Releases](https://github.com/AvramRobert/omnia/releases)

* Extract the `.tar` or `.zip` file

* Make the `omnia` file inside executable

* Run

## Configuration

Each Omnia release comes with a configuration file called `omnia.edn`, wherein
one can configure things like keybindings, the colourscheme and toggling of features. 

### Colourscheme

Currently supported colours: 
 * :black
 * :white
 * :red 
 * :blue
 * :yellow
 * :cyan 
 * :magenta
 * :green
 * :default (transparent)

### Keybindings

**NOTE**: The syntax for configuring keybindings may change in the future.

Each operation omnia supports is bound to a unique key or combination of keys. 
Every keybind is defined as a map similar to the following: <br />

```clojure
{ :key   <key> 
  :ctrl  <boolean> 
  :alt   <boolean> 
  :shift <boolean> }
```

The :key field represents the actual key being pressed. For simple character keys, 
the value of that field should just be the normal Clojure character version of that key. <br />
For example, if I want for something to happen when I press "c": <br />

```clojure
{ :key   \c 
  :ctrl  false 
  :alt   false 
  :shift false }
```

For more esoteric keys, like "enter", "space", "tab" etc, the keyword version of the name of that specific 
key is required. <br />
For example, if I want for something to happen when I press "enter": <br />

```clojure
{:key   :enter 
 :ctrl  false 
 :alt   false 
 :shift false}
```

Any key combination with "ctrl", "alt" and/or "shift" is done by setting the value of "ctrl", "alt" and/or "shift"
to "true" in that map. <br />
For example, if I want for something to happen when I press "alt" and "e": <br /> 

```clojure
{:key   \e 
 :ctrl  false 
 :alt   true 
 :shift false }
```

Lastly, you need only specify "ctrl", "alt" and "shift" when you want to turn them *on*.
By default, they are always set to "false". That means, that <br />

```clojure
{:key  :enter 
 :ctrl  false 
 :shift false 
 :alt   false}
```
is equivalent to
```clojure
{:key :enter}
```

#### Supported keys:
**NOTE**: Please keep in mind that all the character keys, "enter", "space", "backspace", "delete"
and the arrow keys are used for actual input. I would advise not reserving them for other operations. <br />
Combinations of key + ctrl/alt/shift are however open. 
 
 * All character keys
 * The arrows: :left, :right, :up, :down
 * :enter
 * :space
 * :backspace
 * :tab
 * :page-up, :page-down
 * :escape
 * :home
 * :end
 * :insert
 * :delete
 * :f1 through :f19

**NOTE**: Some key combinations do not currently work (issue with the terminal library omnia uses)
 * ctrl + enter
 * ctrl + up
 * ctrl + down
 * ctrl + backspace

Should you find additional ones, please feel free to report them so I can add them to the list.


## Features

FIXME

## Examples

FIXME 

### Bugs

Bug reports can be submitted as issues here on github.

**Note**: The REPL has a built-in failure-handling system and spits 
out a stack-trace dump when it crashes. 
Should it, for some reason, crash for you, then in the same directory
where you ran `omnia`, you shall find a file named
`.omnia.error` containing the said stack-trace dump. <br />
Should you decide to report the crash, I would very much appreciate if you would, 
together with a short description of the actions performed before the crash, also
attach the contents of that file to your issue.

### Feature requests
Feature requests can be submitted as issues with the label `feature`. <br />
I would, however, like for every feature request to have a short description of its use case. <br />
Their implementation priority will be determined in terms of their complexity and
said use case. 
