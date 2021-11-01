<img src="docs/images/omnia-logo.png" width="100" height="101">

# Omnia

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

Description and example GIFs for every feature [HERE](docs/features.md).

## Bugs

Bug reports can be submitted as issues here on github.

**Note**: The REPL has a built-in failure-handling system and spits 
out a stack-trace dump when it crashes. 
Should it, for some reason, crash for you, then in the same directory
where the `omnia` executable is, you shall find a file named
`.omnia.error` containing the said stack-trace dump. <br />
Should you decide to report the crash, I would very much appreciate if you would, 
together with a short description of the actions performed before the crash, also
attach the contents of that file to your issue.

## Feature requests
Feature requests can be submitted as issues with the label `feature`. <br />
I would however like for every feature request to have a short description of its use case.
Their implementation priority will be determined in terms of their complexity and
said use case.

Pull requests are also very welcome!

## Rationale

 Omnia is a stand-alone REPL. It doesn't support interactive development with existing Clojure projects.
 It was never meant to be used for interactive development or to be integrated with
 any kind of editors. The rationale behind it is to provide a better sandbox for experimenting with ideas.

 Hence its description: _A Clojure REPL for prototyping, experimenting and trying things out_.

 I've given a lightning talk about it some time ago.
 
The talk starts roughly at 1:05: https://www.youtube.com/watch?v=qdeU-2eoEIY
## License

Copyright © 2017-2018 Robert Marius Avram

Distributed under the Apache-2.0 License.