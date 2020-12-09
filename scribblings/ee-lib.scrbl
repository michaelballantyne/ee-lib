#lang scribble/manual

@(require (for-label racket))

@title[#:tag "ee-lib"]{ee-lib: Library Support for DSL Macro Expanders}
@author+email["Michael Ballantyne" "michael.ballantyne@gmail.com"]


@(define (tech/reference str)
   (tech #:doc '(lib "scribblings/reference/reference.scrbl") str))

@;-----------------------

@section{Scope, binding, and hygiene operations for DSL expanders}
@(require (for-label ee-lib))
@defmodule[ee-lib]

Import at phase 1.

@subsection{Scope}

@deftech{scope tag}


@defform[(with-scope id body ...)]{

@deftech{scope tagger}

Binds @racket[id] to a fresh @tech{scope tagger} and evaluates the @racket[body] forms in an a @tech{library local binding context} extended with a new @tech{segment}. Bindings established within the dynamic extent of the evaluation of the @racket[body ...] will thus only be available in the @tech{library local binding context} within that dynamic extent.
}

@defproc[(scope-tagger? [v any/c]) boolean?]{
Returns @racket[#t] if @racket[v] is a @tech{scope tagger} created by @racket[with-scope] and @racket[#f] otherwise.
}

@defproc[(add-scope [syntax syntax?] [tagger scope-tagger?]) syntax?]{

@tech{scope tagger}
Annotates the @racket[syntax] with the @tech/reference{scope} tags represented by the @racket[tagger].
}

@defproc[(add-scopes [syntax syntax?] [taggers (listof scope-tagger?)]) syntax?]{
Annotates the @racket[syntax] with all of the @tech/reference{scope} tags represented by the @racket[taggers].
}

@defproc[(splice-from-scope [syntax syntax?] [tagger scope-tagger?]) syntax?]{
Removes the the scope tags represented by the @racket[tagger] from the @racket[syntax].
}

@defproc[(syntax-local-introduce-splice [syntax syntax?]) syntax?]{
Flips the current @tech/reference{macro-introduction scope} and removes any @tech/reference{use-site scopes} created for the current expansion context.
}


@subsection{Binding}

@tech/reference{local binding context}

@deftech{library local binding context}

@deftech{binding context segment}

@deftech{sealed}

@deftech{disappeared use}

@deftech{disappeared binding}


@defproc*[([(bind! [id identifier?] [v any/c]) identifier?]
           [(bind! [ids (listof identifier?)] [vs (listof any/c)]) (listof identifier?)])]{
Creates a @tech/reference{binding} for the given @racket[id] and extends the current @tech{binding context segment} with an entry mapping the binding to the value @racket[v]. The scope set for the binding always includes the current scope's @tech/reference{inside-edge scope} tag. Also records a @tech{disappeared binding} for @racket[id].

This operation is legal only in a @tech{library local binding context} with an unsealed segment.

The second form works like the first, but for lists of corresponding @racket[ids] and @racket[vs].
}

@defproc[(lookup [id identifier?] [predicate (-> any/c (or/c #f any/c))]) (or/c #f any/c)]{
Looks for a binding and corresponding entry in the @tech{library local binding context} for @racket[id]. If the binding exists, has an value in the local binding context, and the value satisfies @racket[predicate], the value is returned and a @tech{disappeared use} is recorded for @racket[id]. Otherwise returns @racket[#f].
}

@subsection{Transformer evaluation}

@defproc[(eval-transformer [syntax syntax?]) any/c]{
Evaluates @racket[syntax] at phase 1.
}

@subsection{Hygiene}

@subsubsection{Hygiene for expander definitions}

@defform[(define/hygienic (id id ...) ctx-type body ...)
         #:grammar [(ctx-type #:expression #:definition)]]{
TODO
}

@defform[(define/hygienic-metafunction (id id ...) ctx-type body ...)]{
TODO
}

@subsubsection{Hygiene for macro application}

@defproc[(apply-as-transformer [proc procedure?] [ctx-type (or/c 'expression 'definition)]
                               [arg any/c] ...)
          any]{
TODO
}

@subsection{Integrating with Racket}


@defthing[racket-var any/c]{
A singleton value used with @racket[bind!] to create bindings for Racket variables.
}


@defproc[(current-def-ctx) (or/c #f internal-definition-context?)]{
Returns a @tech/reference{internal-definition context} value corresponding to the current @tech{library local binding context}, or @racket[#f] when the @tech{library local binding context} corresponds to the core @tech/reference{local binding context}. The return value is suitable as the @racket[_intdef-ctx] argument to @racket[local-expand].
}

@defproc[(current-ctx-id) symbol?]{
Returns a value suitable for use as the @racket[_context-v] argument to @racket[local-expand] for @tech/reference{internal-definition context} expansion.
}

@; TODO
@;@subsection{Preserving source locations}
@;
@;@defform[(qstx/rc template)]{
@;TODO
@;}



@;-----------------------

@section{Defining literals}
@(require (for-label ee-lib/define))
@defmodule[ee-lib/define]

Import at phase 0.

@defform[(define-literal-forms literal-set-id message (form-id ...))]{
Binds each @racket[form-id] as a macro that raises a syntax error with message @racket[message]. Binds @racket[literal-set-id] as a @tech[#:doc '(lib "syntax/scribblings/syntax.scrbl")]{literal set} containing the @racket[form-id]s.
}

@; TODO
@;@defform[(define-extensible-syntax id)]{
@;
@;Defines
@;
@;@racket[_gen:id]
@;@racket[_id-transform]
@;@racket[_id-rep]
@;@racket[_id-rep-procedure]
@;@racket[_define-id]
@;@racket[_define-simple-id]
@;
@;
@;TODO
@;}

@;-----------------------

@; TODO
@;@section{Persistent free-identifier tables}
@;@(require (for-label ee-lib/persistent-id-table))
@;@defmodule[ee-lib/persistent-id-table]
@;
@;@defform[(define-persistent-free-id-table id)]{
@;TODO
@;}
@;
@;@defproc[(persistent-free-id-table? [v any/c]) boolean?]{
@;TODO
@;}
@;
@;@defproc[(persistent-free-id-table-set! [table persistent-free-id-table?] [id identifier?] [v any/c]) void?]{
@;TODO
@;}
@;
@;@defproc[(persistent-free-id-table-ref [table persistent-free-id-table?] [id identifier?]) any/c]{
@;TODO
@;}
@;
@;@; I want this to be syntax, a module macro. Think it makes better sense than as a fn for this API.
@;@;    enforces correct use.
@;@defform[(persist-free-id-table-extensions! id)]{
@;TODO
@;}

@;-----------------------

@; TODO

@;@section{Runtime errors that highlight syntax}
@;@(require (for-label ee-lib/errors))
@;@defmodule[ee-lib/errors]
@;
@;@defstruct[(exn:fail:contract:srcloc exn:fail:contract) ([srcloc srcloc?])]{
@;TODO
@;}
@;
@;@defproc[(raise-argument-error/stx [name symbol?] [expected string?] [v any/c] [stx syntax?]) any]{
@;Similar to @racket[raise-argument-error].
@;}


@; TODO
@;@;-----------------------
@;
@;@section{Changes since "Macros for Domain-Specific Languages"}
@;
@;@racket[define/hygienic] and @racket[apply-as-transformer] serve different purposes. (Sealing)
@;
@;No @racket[local-expand] with implicit context. Use normal @racket[local-expand] plus @racket[current-def-ctx] and @racket[current-ctx-id] instead.
@;
@;
@;
@;@;-----------------------
@;
@;@section{Limitations}
@;
@;Implementing arbitrarily nested definition contexts depends on control flow due to dynamic-extent behavior of @racket[with-scope].
@;
@;Doesn't get hygiene for splicing forms right.
@;
@;Doesn't get use-site hygiene for nested definition contexts right; use-site scopes are tracked
@;via @racket[define/hygienic] context, not per scope.
@;
@;Doesn't get hygiene at boundary with Racket right. Have to enter a @racket[define/hygienic] first.
