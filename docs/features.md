## Persistent REPL history

* Documentation found [here](configuration.md#history)

## Configurable Key Bindings
* Documentation found [here](configuration.md#key-bindings)

## Configurable syntax highlighting
* Documentation found [here](configuration.md#syntax-highlighting)

![syntax-highlighting](images/syntax-highlighting.gif)

## Structural editing
* Barf and Slurp not supported

![structural-editing](images/structural-editing.gif)

## Multi-line input

![multi-line-editing](images/multi-line-editing.gif)

## Multi-view input

![multi-view](images/multi-view-editing.gif)


## Automatic parens matching

![parens-matching](images/parens-matching.gif)

## On-demand code formatting

![formatting](images/reformatting.gif)

## Input suggestions with autocompletion

![input-suggestions](images/suggestions.gif)

## Signature lookup

![signature-lookup](images/signature.gif)

## Documentation lookup

![docs-lookup](images/documentation.gif)

## Selection system
![selection](images/selection.gif)

## Selection expansion

![expanding-selection](images/expanding-selection.gif)

## Copy/Cut/Paste

![copy-cut-paste](images/copy-cut-paste.gif)

## Undo/Redo

![undo-redo](images/undo-redo.gif)

## Scrolling
* Mouse scrolling not supported

![scrolling](images/scrolling.gif)

## Output clearing

![clearing](images/clearing.gif)

## Dependency resolution
* Other repositories are supported through `retrieve-from` by explicitly specifying them:
 ```clojure
 (retrieve-from {"sonatype" "https://oss.sonatype.org/content/repositories/releases/"}
                '[joda-time/joda-time "2.9.9"])
 ```

![dependency-resolution](images/dependency-resolution.gif)
