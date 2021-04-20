## Keymap

**MacOS users**: Some of the default key bindings may not work directly, because MacOS assigns them
to other OS-related actions that have higher priority.

To avoid this, please take a look at [configuration](https://github.com/AvramRobert/omnia/blob/master/docs/configuration.md) 
and just re-bind those omnia actions to other keys.


| Operation    | Keybind        | Description | Config parameter|
|--------------|----------------|-------------|-----------------|
|Input char| any character key | Inserts character | - |
|Move up/down/left/right | Arrows UP/DOWN/LEFT/RIGHT | Moves the cursor | :up, :down, :left, :right |
|Break | ENTER | Jumps to the next line | :break |
|Backspace | BACKSPACE | Deletes character to the left or removes selected input | :backspace |
|Delete | DELETE | Deletes character to the right or removes selected input | :delete |
|Evaluate| ALT + E | Evaluates the input | :evaluate |
|Clear | CTRL + R | Clears screen of previous evaluations | :clear |
|Indent| CTRL + ALT + L | Indents/formats input | :indent |
|Match | CTRL + P | Explicitly highlights the parens of the current expression| :match |
|Suggest | TAB | Displays a list of possible inputs. Inputs can be selected by repeatedly pressing the key | :suggest |
|Signature | ALT + P | Displays a list of all signatures of the function | :signature |
|Documentation | ALT + I | Displays a scrollable view of the function's documentation | :docs |
|Jump | CTRL + LEFT/RIGHT | Jumps over words and spaces | :jump-left, :jump-right |
|Select | SHIFT + LEFT/RIGHT/UP/DOWN | Selects input in the direction specified by the arrow | :select-up, :select-down, :select-left, :select-right |
|Expand select| CTRL + W | Selects by gradually expanding from words to expressions | :expand |
|Jump select | CTRL + SHIFT + LEFT/RIGHT | Selects the words and spaces it jumps over | :jump-select-left, jump-select-right |
|Copy | ALT + C | Copies the current selection | :copy |
|Cut  | ALT + X | Cuts the current selection | :cut |
|Paste | ALT + P | Pastes the cut/copied selection | :paste |
|Select all | CTRL + A | Selects the whole input | :select-all |
|Scroll up  | PAGE-UP | Scrolls up the view | :scroll-up |
|Scroll down | PAGE-DOWN | Scrolls down the view | :scroll-down |
|Previous evaluation | ALT + UP | Goes to the previous evaluation in the history | :prev-eval |
|Next evaluation | ALT + DOWN | Goes to the following evaluation in the history | :next-eval |
|Undo | ALT + Z | Undoes the last action | :undo |
|Redo | ALT + Y | Redoes the last undo | :redo |
|Exit | CTRL + D | Exits the REPL | :exit |
|Force quit | CTRL + C | Forcibly terminates the REPL | - |