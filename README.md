# omnia

A Clojure REPL for prototyping, experimenting and trying things out. <br />

Omnia is a REPL with almost all the power of a fully fledged editor, without being an editor.

## Installation
### General

* Download the latest archived release: <br />
[Releases](https://github.com/AvramRobert/omnia/releases)

* Extract the `.tar` file

* Make the `omnia` file inside executable

* Run

### Arch Linux

Download it from the AUR:

    yaourt -S omnia
    
## Features

#### Configurable
 * Omnia has configurable syntax highlighting and key bindings: 
 * Take a look at [Configuration](https://github.com/AvramRobert/omnia/blob/master/doc/configuration.md) 

#### Syntax highlighting
 * For configuration please look at [Configuration](https://github.com/AvramRobert/omnia/blob/master/doc/configuration.md).

 ![syntax-highlighting](images/syntax-highlighting.gif)
 
 ![syntax-highlighting-config](images/syntax-highlighting-config.gif)

#### Structural editing
 * Manipulate s-exprs in a paredit-like fashion

 ![sexprs](images/structural.gif)

#### Multi-line input
 * Input, edit and structure code in multiple lines

 ![multi-line](images/multi-line.gif)
 
#### Multi-view input
 * The view has no bottom, so you can exceed it whilst being able to navigate back and forth

 ![multi-view](images/multi-view.gif)


#### Automatic parens matching
 * Always know in which expression you are:
 
 ![parens-matching](images/parens-matching.gif)

#### Code formatting
 * Format the current input code *by need*
 * Automatic formatting not yet supported

 ![formatting](images/formatting.gif)

#### Input suggestions with autocompletion
 * Receive input suggestions and select from a truncated list

 ![input-suggestions](images/suggestions.gif)

#### Signature lookup
 * Look up the signature of a function
 
 ![signature-lookup](images/signature.gif)
 
#### Documentation lookup
 * Look up the documentation of a function and scroll through it
 
 ![docs-lookup](images/documentation.gif)

#### Selection system
 * Select code forward, backward up and down
 * Selected code can be cut, copied, deleted or overwritten

 ![selection](images/selection.gif)

#### Selection expansion
 * Select code by means of incremental expansion

 ![selection-expansion](images/expansion.gif)
 
#### Copy/Cut/Paste
 * Copy/cut selections of input
 * Paste copied or cut code wherever in the input

**Note:** Only supported from within the REPL. Copying/cutting from external sources
and then pasting inside the REPL is not *currently* directly supported.

 ![copy-cut-paste](images/copy-cut-paste.gif)

#### Undo/Redo
 * Undo actions and redo undoes
 
 ![undo-redo](images/undo-redo.gif)

#### Scrolling
 * Scroll up and down the view arbitrarily
 * No mouse support for scrolling as of yet

 ![scrolling](images/scrolling.gif)
 
#### Output clearing
 * Clear the output history
 
 ![clearing](images/clearing.gif)

#### Dependency resolution
 * Bind external libraries at runtime and use them in the REPL
 * Use the `retrieve` function and pass the desired dependency
 * `retreive` currently only supports `clojars` and `mavencentral`

 ![dependency-resolution](images/retrieval.gif)
 
 * Other repositories are supported through `retrieve-from` by explicitly specifying them:
 ```clojure
 (retrieve-from {"sonatype" "https://oss.sonatype.org/content/repositories/releases/"}
                '[joda-time/joda-time "2.9.9"])
 ```
 
#### Persistent REPL history
 * Evaluations from previous REPL sessions are stored on disk
 * Histories have a limit of 1000 evaluations

 ![repl-history](images/history.gif)
 
#### Graceful failures
 * Should omnia crash for reasons unknown, the REPL will shut down with an appropriate
   message and log the stack trace of the error in a file called `.omnia.error`
 * The stack trace log can be found in the directory where the omnia executable is

### Bugs

Bug reports can be submitted as issues here on github.

**Note**: The REPL has a built-in failure-handling system and spits 
out a stack-trace dump when it crashes. 
Should it, for some reason, crash for you, then in the same directory
where the `omnia` executable is, you shall find a file named
`.omnia.error` containing the said stack-trace dump. <br />
Should you decide to report the crash, I would very much appreciate if you would, 
together with a short description of the actions performed before the crash, also
attach the contents of that file to your issue.

### Feature requests
Feature requests can be submitted as issues with the label `feature`. <br />
I would however like for every feature request to have a short description of its use case.
Their implementation priority will be determined in terms of their complexity and
said use case.

Pull requests are also very welcome!

### Things to look forward to
Omnia is under constant development, enhancement and improvement.
Incoming features in (not necessarily) chronological order:

 * Slurping / barfing expressions
 * REPL content manipulation
 * Automatic code formatting
 * Additional performance improvements
 * Better exception printing
 * ..

## License

Copyright © 2017-2018 Robert Marius Avram

Distributed under the Apache-2.0 License.