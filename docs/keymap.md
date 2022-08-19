## Keymap

**Note**: Due to either operating system key priorities and/or the terminal backend _omnia_ uses,
some key bindings might not work directly out of the box.

The key bindings can be changed. For more information, take a look at [configuration](https://github.com/AvramRobert/omnia/blob/master/docs/configuration.md)

| Operation    | Default key binding        | Description | Config parameter|
|--------------|----------------|-------------|-----------------|
|Input char| any character key | Inserts character | - |
|Move up/down/left/right | Arrows UP/DOWN/LEFT/RIGHT | Moves the cursor | :up, :down, :left, :right |
|New line | ENTER | Creates a new line | :new-line |
|Delete previous | BACKSPACE | Deletes the previous character | :delete-previous |
|Delete current | DELETE | Deletes the current character | :delete-current |
|Evaluate| CTRL + ENTER | Evaluates the input | :evaluate |
|Clear | CTRL + R | Clears screen | :clear |
|Reformat| CTRL + ALT + L | Reformats input | :reformat |
|Suggestion | TAB | Lists matching suggestions for input. Suggestions can be scrolled through by repeatedly pressing the key | :suggestion |
|Signature | CTRL + P | Lists all signatures of the function | :signature |
|Documentation | CTRL + Q | Displays a scrollable view of the function's documentation | :documentation |
|Jump | CTRL + LEFT/RIGHT | Jumps over words and spaces | :jump-left, :jump-right |
|Select | SHIFT + LEFT/RIGHT/UP/DOWN | Selects input in the direction specified by the arrow | :select-up, :select-down, :select-left, :select-right |
|Expand selection| CTRL + W | Selects by gradually expanding from words to expressions | :expand-selection |
|Jump select | CTRL + SHIFT + LEFT/RIGHT | Selects the words and spaces it jumps over | :jump-select-left, jump-select-right |
|Copy | CTRL + C | Copies the current selection | :copy |
|Cut  | CTRL + X | Cuts the current selection | :cut |
|Paste | CTRL + P | Pastes the cut/copied selection | :paste |
|Select all | CTRL + A | Selects the whole input | :select-all |
|Scroll up  | PAGE-UP | Scrolls up the view | :scroll-up |
|Scroll down | PAGE-DOWN | Scrolls down the view | :scroll-down |
|Previous evaluation | CTRL + UP | Goes to the previous evaluation in the history | :previous-evaluation |
|Next evaluation | CTRL + DOWN | Goes to the following evaluation in the history | :next-evaluation |
|Undo | ALT + Z | Undoes the last action | :undo |
|Redo | ALT + Y | Redoes the last undo | :redo |
|Exit | CTRL + D | Exits the REPL | :exit |