(ns omnia.highlight-beta)

(comment
  "Take the whole input stream,
  fragment each relevant section and tag it with the colour it needs
  to be highlighted.
  The idea is to invert control through a function.

  Give the highlighter the input and a function and let it process
  the input whilst applying the function at each emission.
  You can also define an catamorphic or anamorphic highlighter

  For individual words, define a concrete map within the colourscheme
  where you define the words you want highlighted, together with the colour.
  The engine shall then process these inputs with the state machine model i've defined.

  A good idea would be to somehow simplify and improve the state machine API.
  Right now, it requires too many states.

  I basically need some sort of an algebra for this.
  But the idea is to define processing states that have
  influence over the actual state transitions, so that it can force
  the transitions to either stop, continue or skip until some end state is
  reached.

  Each word has to be lifted into its own state")

(def ^:const -list :list)
(def ^:const -vec :vector)
(def ^:const -map :map)
(def ^:const -num :number)
(def ^:const -str :string)
(def ^:const -char :character)
(def ^:const -cmt :comment)
(def ^:const -kwd :keyword)
(def ^:const -fn :function)
(def ^:const -tkn :token)
(def ^:const -txt :text)