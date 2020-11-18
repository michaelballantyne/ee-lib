#lang scribble/manual

@(require (for-label racket))

@title[#:tag "ee-lib"]{ee-lib: Library Support for DSL Macro Expanders}
@author+email["Michael Ballantyne" "michael.ballantyne@gmail.com"]

@section{Scope, binding, and hygiene operations for DSL expanders}
@(require (for-label ee-lib))
@defmodule[ee-lib]

@subsection{Scope}

@defform[(with-scope id body ...)]{
TODO
}

@defproc[(scope? [v any/c]) boolean?]{
TODO
}

@defproc[(add-scope [syntax syntax?] [scope scope?]) syntax?]{
TODO
}

@defproc[(add-scopes [syntax syntax?] [scopes (listof scope?)]) syntax?]{
TODO
}

@defproc[(splice-from-scope [syntax syntax?] [scope scope?]) syntax?]{
TODO
}

@defproc[(syntax-local-introduce-splice [syntax syntax?]) syntax?]{
TODO
}


@subsection{Binding}

@defproc*[([(bind! [id identifier?] [value any/c]) identifier?]
           [(bind! [ids (listof identifier?)] [values (listof any/c)]) (listof identifier?)])]{
TODO
}

@defproc[(lookup [id identifier?] [predicate (-> any/c boolean?)]) (or/c #f any/c)]{
TODO
}


@subsection{Transformer evaluation}

@defproc[(eval-transformer [syntax syntax?]) any/c]{
TODO
}

@subsection{Hygiene}

@defform[(define/hygienic (id id ...) ctx-type body ...)
         #:grammar [(ctx-type #:expression #:definition)]]{
TODO
}

@defform[(define/hygienic-metafunction (id id ...) ctx-type body ...)]{
TODO
}

@defproc[(apply-as-transformer [proc procedure?] [ctx-type (or/c 'expression 'definition)]
                               [arg any/c] ...)
          any]{
TODO
}

@subsection{Integrating with Racket}

@defproc[(racket-var) racket-var?]{
TODO
}

@defproc[(current-def-ctx) internal-definition-context?]{
TODO
}

@defproc[(current-ctx-id) symbol?]{
TODO
}

@subsection{Preserving source locations}

@defform[(qstx/rc template)]{
TODO
}

@defform[(qstx/lp orig template)]{
TODO
}

@section{Defining literals and categories of macros}
@(require (for-label ee-lib/define))
@defmodule[ee-lib/define]


@section{Persistent free-identifier tables}
@(require (for-label ee-lib/persistent-id-table))
@defmodule[ee-lib/persistent-id-table]


@section{Runtime errors that highlight syntax}
@(require (for-label ee-lib/errors))
@defmodule[ee-lib/errors]
