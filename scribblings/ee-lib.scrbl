#lang scribble/manual

@(require (for-label racket))

@title[#:tag "ee-lib"]{ee-lib: Library Support for DSL Macro Expanders}
@author+email["Michael Ballantyne" "michael.ballantyne@gmail.com"]

This library provides a higher-level API to Racket's syntax system, designed
for implementing macro expanders for DSLs. The paper
@hyperlink["https://dl.acm.org/doi/pdf/10.1145/3428297"]{"Macros for
Domain-Specific Languages"} serves as the guide-level explanation of the
library and associated programming patterns. This page provides reference
documentation.


@;-----------------------

@(define (tech/reference str)
   (tech #:doc '(lib "scribblings/reference/reference.scrbl") str))

@(define (seclink/reference sec str)
   (seclink sec #:doc '(lib "scribblings/reference/reference.scrbl") str))
          
@;-----------------------


@section{Scope, binding, and hygiene operations for DSL expanders}
@(require (for-label ee-lib))
@defmodule[ee-lib]

Import at phase 1.

@subsection{Scope}

Intuitively, a scope is a region of program text in which certain bindings are
available, determined by scoping forms such as @racket[let] or @racket[block].
In the context of macro expansion, syntax may be moved in and out of scopes
during the process of expansion, and some scopes aren't immediately evident in
the source program text. The Racket expander uses several kinds of
@tech/reference{scope} values to represent regions of partially expanded
programs to implement macro hygiene. This documentation refers to these scope
values as @deftech{scope tags}, and reserves the word "scope" for the intuitive
notion.

@defform[(with-scope id body ...)]{

Introduces the elements needed to implement a scope: an
@tech/reference{outside-edge scope} tag, an @tech/reference{inside-edge scope}
tag, and a new @tech{binding context segment}.

The two scope tags are encapsulated in a @deftech{scope tagger} value
accessible via @racket[id]. The new @tech{binding context segment} is added to
the @tech{library local binding context} for the dynamic extent of the
evaluation of the @racket[body] forms.

}

@defproc[(scope-tagger? [v any/c]) boolean?]{

Returns @racket[#t] if @racket[v] is a @tech{scope tagger} created by
@racket[with-scope] and @racket[#f] otherwise.

}

@defproc[(add-scope [stx syntax?] [tagger scope-tagger?]) syntax?]{

Annotates the @racket[stx] with the @tech{scope tags} represented by the
@racket[tagger].

}

@;@defproc[(add-scopes [stx syntax?] [taggers (listof scope-tagger?)]) syntax?]{

@;Annotates the @racket[stx] with all of the @tech/reference{scope} tags
@;represented by the @racket[taggers].

@;}

@defproc[(splice-from-scope [id identifier?] [tagger scope-tagger?]) syntax?]{

Removes the the @tech{scope tags} represented by the @racket[tagger] from the
@racket[id].

Useful for implementing splicing forms like @racket[splicing-let] where certain
bindings that initially appear to be within a scope in fact splice outside of
it.

}

@defproc[(syntax-local-introduce-splice [stx syntax?]) syntax?]{

Flips the current @tech/reference{macro-introduction scope} and removes any
@tech/reference{use-site scope} tags created for the current expansion context.

Useful when moving syntax out of the context of a given macro expansion, as
when lifting a definition to a surrounding context.

}

@subsection{Binding}

This library implicitly maintains a @deftech{library local binding context}
with entries that map bindings to values, similar to the core expander's
@tech/reference{local binding context}. See @racket[current-def-ctx] for a way
to access the library local binding context as a first-class definition context
that can be used with the core expander's API.

The library local binding context consists of nested @deftech{binding context
segments} corresponding to the nested dynamic extents of @racket[with-scope]
uses. The @racket[bind!] operation adds new entries to the innermost segment.
The binding context may be @deftech{sealed}, preventing further bindings within
the context until a new segment is added. All operations that use syntax to
create or lookup bindings in the binding context first annotate the syntax with
the @tech/reference{inside-edge scope} tag for the scope corresponding to the
innermost binding context segment.

@defproc*[([(bind! [id identifier?] [v any/c]) identifier?]
           [(bind! [ids (listof identifier?)] [vs (listof any/c)]) (listof identifier?)])]{

Creates a @tech/reference{binding} for the given @racket[id] and extends the
current @tech{binding context segment} with an entry mapping the binding to the
value @racket[v]. The scope set for the binding always includes the current
scope's @tech/reference{inside-edge scope} tag. Also records a
@tech{disappeared binding} for @racket[id].

This operation is legal only in a @tech{library local binding context} with an
@tech[#:key "sealed"]{unsealed} segment.

The second form works like the first, but for lists of corresponding
@racket[ids] and @racket[vs].

}

@defproc[(lookup [id identifier?] [predicate (-> any/c (or/c #f any/c))]) (or/c #f any/c)]{

Looks for a binding and corresponding entry in the @tech{library local binding
context} for @racket[id]. If the binding exists, has an value in the
@tech{library local binding context}, and the value satisfies
@racket[predicate], the value is returned and a @tech{disappeared use} is
recorded for @racket[id]. Otherwise returns @racket[#f].

}


@subsection{Hygiene for expander definitions}

As an expander traverses syntax, it needs to enter new
@seclink/reference["expand-context-model"]{expansion contexts}
and adjust the @tech{library local binding context}. And hygiene should treat
syntax constructed introduced by templates in the expander similarly to syntax
introduced by a macro. The following forms define expand functions with these
behaviors.

@defform[(define/hygienic (id arg ...) ctx-type body ...)
         #:grammar [(arg id)
                    (ctx-type #:expression #:definition)]]{

Defines a function @racket[id] with positional arguments @racket[arg ...] and
body @racket[body]. Invocations of the function enter a new
@seclink/reference["expand-context-model"]{expansion context}
of the type specified the @racket[ctx-type]: an expression context, or an
internal-definition context. Entering an expression context @tech[#:key
"sealed"]{seals} the @tech{library local binding context}; entering an
internal-definition context does not.

Invocations of the function are hygienic in the same way macro applications are
hygienic: for syntax-valued arguments and returns, arguments are tagged by a
fresh @tech/reference{use-site scope} tag, and syntax returned from the
function that was not part of one of the arguments is tagged with a fresh
@tech/reference{macro-introduction scope} tag.

The expansion context type determines the treatment of
@tech/reference{use-site scope} tags at uses of @racket[bind],
@racket[syntax-local-identfier-as-binding], and
@racket[syntax-local-introduce-splice] within. During expansion in an
internal-definition context, the expansion context tracks a set of use-site
scopes created during expansion of the context. The operations just mentioned
remove use-site scopes present in that set. Entering an expression context
resets the set to empty.

}

@defform[(define/hygienic-metafunction (id id ...) ctx-type body ...)]{
Like @racket[define/hygienic], but wrapped as a template metafunction.
}

@subsection{Hygiene for macro application}

Expanders also need to apply macro hygiene when invoking macros, via
@racket[apply-as-transformer]. Macros should expand to forms such as
@racket[define] rather than directly extending the binding context with
@racket[bind!], so @racket[apply-as-transformer] @tech[#:key "sealed"]{seals}
the binding context.

@defproc[(apply-as-transformer [proc procedure?]
                               [binding-id (or/c identifier? #f)]
                               [ctx-type (or/c 'expression 'definition)]
                               [arg any/c] ...)
          any]{

Calls the function @racket[proc] with the same expansion context and hygiene
behavior as calling a function defined by @racket[define/hygienic], except that
the @tech{library local binding context} is always @tech{sealed}.

The @racket[binding-id] argument specifies a binding associated with the
@racket[proc], which the expander uses to determine whether to add
@tech/reference{use-site scopes} and which @tech/reference{code inspector}
to use during expansion.

@history[#:changed "1.0" @elem{Added the @racket[binding-id] argument.}]
}

@subsection{Transformer evaluation}

@defproc[(eval-transformer [stx syntax?]) any/c]{

Evaluates @racket[stx] at phase 1 in the @tech{library local binding context}.

Useful for implementing forms like @racket[let-syntax] for a DSL.

}

@subsection{Integrating with Racket's expander}

When a DSL's syntax has Racket subexpression positions, the DSL expander needs
to call the Racket expander via @racket[local-expand]. The following operations
help connect the Racket expansion with the library-managed binding context.

@defproc[(current-def-ctx) (or/c #f internal-definition-context?)]{

Returns a @tech/reference{internal-definition context} value corresponding to
the current @tech{library local binding context}, or @racket[#f] when the
@tech{library local binding context} corresponds to the core
@tech/reference{local binding context}. The return value is suitable as the
@racket[_intdef-ctx] argument to @racket[local-expand].

}

@defproc[(current-ctx-id) symbol?]{

Returns a value suitable for use as the @racket[_context-v] argument to
@racket[local-expand] for @tech/reference{internal-definition context}
expansion.

}

@deftogether[(@defproc[(racket-var) racket-var?]
              @defproc[(racket-var? [v any/c]) boolean?])]{

DSL syntax may bind variables that should be accessible from Racket syntax. Use
the @racket[racket-var] datatype to represent Racket variable bindings in the
@tech{library local binding context} via @racket[bind!].

}


@subsection{Disappeared uses and bindings}

For DrRacket to provide behaviors such as binding arrows, it needs to know
which identifiers act as bindings and references. In the case of DSL code these
identifiers may not be present as bindings and references in the program's
compilation to Racket. The @racket[bind!] and @racket[lookup] operations
automatically record such @deftech{disappeared uses} and @deftech{disappeared
bindings}.

DrRacket looks for information about disappeared uses and bindings on syntax in
fully-expanded modules (see
@secref["Syntax_Properties_that_Check_Syntax_Looks_For" #:doc '(lib
"scribblings/tools/tools.scrbl")]). This library inserts extra syntax without
runtime meaning into the expanded module to carry this information.




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

Binds each @racket[form-id] as a macro that raises a syntax error with message
@racket[message]. Binds @racket[literal-set-id] as a @tech[#:doc '(lib
"syntax/scribblings/syntax.scrbl")]{literal set} containing the
@racket[form-id]s.

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
