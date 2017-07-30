# omnia

A Clojure REPL for prototyping, experimenting and trying things
out quickly. <br />

Omnia gives you almost all the power of a fully-fledged IDE, without
the actual IDE. 

## Installation
* Download the latest released jar: <br />
[Releases](https://github.com/AvramRobert/omnia/releases)

* Add this alias to your shell `rc` file (ex: `~/.bashrc`, `~/.zshrc`): <br />
`alias omnia="cd </path/to/omnia/jar> java -jar omnia"`

* Restart terminal
* Run `omnia`

## Features

FIXME

## Examples

FIXME 

### Bugs

Bug reports can be submitted as issues here on github.

**Note**: The REPL has a built-in failure-handling system and spits 
out a stack-trace dump when it crashes. 
Should it, for some reason, crash for you, then in the same directory
as the `.jar` file, you shall find a file named
`.omnia.error` containing the said stack-trace dump. <br />
Should you decide to report the crash, I would very much appreciate if you would, 
together with a short description of the actions performed before the crash, also
attach the contents of that file to your issue.

### Feature requests
Feature requests can be submitted as issues with the label `feature`. <br />
I would, however, like for every feature request to have a short description of its use case. <br />
Their implementation priority will be determined in terms of their complexity and
said use case. 

## License

Copyright © 2017 Robert Marius Avram

Distributed under the Eclipse Public License either version 1.0 or (at
your option) any later version.
