---
title: "Tufte style sidenotes and marginnotes in Pollen"
tags: Programming, Pollen, Racket
---

When evaluating Pollen [I complained][prev-post] about markdown/pandoc's lack of sidenote handling. I have solved it for Pollen but felt it deserved it's own post.

A caveat: I generated [Tufte CSS][] style sidenotes and marginnotes which made it more complex than if I had simply generated "standard" sidenotes. If you want to adapt this yourself I'm sure you can simplify the code to fit your needs.

So in Pollen markup I want to be able to write this:

```pollen
Lisp is a pretty nice ◊sn{cult} language.

◊ndef["cult"]{
    Some may say it's the language to rule them all.
}
```

To generate this:

```html
Lisp is a pretty nice
<label for="cult"
       class="margin-toggle sidenote-number">
</label>
<input type="checkbox"
       id="cult"
       class="margin-toggle"/>
<span class="sidenote">
    Some may say it's the language to rule them all.
</span>
language.
```

The order of `◊sn` and `◊ndef` shouldn't matter.

By having the sidenote span right in the middle of the text it allows us to [toggle it without javascript][toggle-noscript]. This appeals to me as a heavy noscript user but it has a significant drawback: you cannot have block level tags like `div` or `p` inside a `span`.

You also cannot use an `aside` instead of a `span` directly here as you cannot have it inside a paragraph tag.

Tufte has both sidenotes and marginnotes which we can implement in a general way. This is the markup:

```pollen
This has a sidenote with numbers.◊sn{note}

This has a marginnote without numbers.◊mn{note}

◊ndef["note"]{
    The note itself
}
```

These are the Pollen tags with their markup difference:

```racket
(define (mn ref-in)
  (note-ref #:label-class "margin-toggle"
            #:label-content "⊕"
            #:span-class "marginnote"
            #:ref ref-in))

(define (sn ref-in)
  (note-ref #:label-class "margin-toggle sidenote-number"
            #:label-content ""
            #:span-class "sidenote"
            #:ref ref-in))
```

We'll get to the `note-ref` definition in a little bit.

We can use the same markup for sidenote and marginnote content. The idea is to store the content in a map and look it up and insert it into the markup later.


```racket
;; The note ref -> definition map
(define note-defs (make-hash))
;; The tag
(define (ndef ref-in . def)
  (define id (format "nd-~a" ref-in))
  (define ref (string->symbol id))
  (hash-set! note-defs ref def)
  "")
```

Simple enough right? If we want to apply post-processing, like adding paragraphs, we need to do some more work. Especially since we cannot have `p` tags inside the `span`! I solved this by instead wrapping paragraphs with a span I style like paragraphs. This is the actual code:

```racket
(define (ndef ref-in . def)
  (define id (format "nd-~a" ref-in))
  (define ref (string->symbol id))

  ;; Because p doesn't allow block elements
  ;; and span doesn't allow p elements
  ;; use a special span .snp element to emulate paragraphs.
  ;; This is workaround is required as we want to inject a whole sidenote
  ;; inline to use the checkbox css toggling to avoid javascript.
  (define (wrap xs)
    (list* 'span '((class "snp")) xs))
  (define content
    (decode-elements def
                     #:txexpr-elements-proc (λ (x) (decode-paragraphs x wrap))
                     #:string-proc string-proc))

  (hash-set! note-defs ref content)
  "")
```

Here `string-proc` can contain smart quotes expansion or whatever extra decoding you want to use.

Now to another problem: we want to have refs before defs and vice versa. This means we might parse the references before we've registered the definitions. We can solve this by making a decode pass in the root tag and marking refs with a special symbol which we replace. This can be made very general:

```racket
;; Register symbols which gets inline replaced
;; by function return values.
(define replacements (make-hash))
(define (register-replacement sym f)
  (hash-set! replacements sym f))

(define (replace-stubs x)
  (let ((f (hash-ref replacements x #f)))
    (if f
      (f x)
      x)))
```

Which is used like this:

```racket
(register-replacement 'sym-to-replace (λ (x) "REPLACED"))

(define (root . args)
  (define decoded (decode-elements args
    #:entity-proc replace-stubs)))
```

Where `root` in Pollen allows you to transform the whole document.

And now we can get back to our reference tag:

```racket
(define (note-ref #:label-class label-class
                  #:label-content label-content
                  #:span-class span-class
                  #:ref ref-in)
  (define id (format "nd-~a" ref-in))
  (define ref (string->symbol id))
  (define (replace ref)
    (define def (hash-ref note-defs ref #f))
    (unless def (error (format "missing ref '~s'" ref)))
    `(span
      (label ((class ,label-class) (for ,id)) ,label-content)
      (input ((id ,id) (class "margin-toggle") (type "checkbox")))
      (span ((class ,span-class)) ,@def)))

  (register-replacement ref replace)
  ref)
```

It's basically just registering a replacement function which returns the markup:

```racket
`(span
  (label ((class ,label-class) (for ,id)) ,label-content)
  (input ((id ,id) (class "margin-toggle") (type "checkbox")))
  (span ((class ,span-class)) ,@def)))
```

Are we done? Almost. This would create markup wrapped in a span, like:

```html
<span>
     <label for="cult"
            class="margin-toggle sidenote-number">
     </label>
     <input type="checkbox"
            id="cult"
            class="margin-toggle"/>
     <span class="sidenote">
         Some may say it's the language to rule them all.
     </span>
</span>
```

The replacement function can only return a single element so we had to wrap it in something. In this particular case it's not a big deal but it is indeed a general limitation of Pollen tags. Is it something we can get around?

Yes it is. We can add a special symbol in the markup:

```racket
`(splice-me ; <- instead of span
  (label ((class ,label-class) (for ,id)) ,label-content)
  (input ((id ,id) (class "margin-toggle") (type "checkbox")))
  (span ((class ,span-class)) ,@def)))
```

And then do an extra post process step to replace it with it's content, inline:

```racket
;; A splicing tag to support returning multiple inline
;; values. So '((splice-me "a" "b")) becomes '("a" "b")
(define (splice-me? x)
  (match x
    [(cons 'splice-me _) #t]
    [else #f]))

;; Expand '(splice-me ...) into surrounding list
(define (expand-splices in)
  (if (list? in)
    (foldr (λ (x acc)
              (if (splice-me? x)
                (append (expand-splices (cdr x)) acc)
                (cons (expand-splices x) acc)))
           '()
           in)

    in))

(define (root . args)
  (define decoded (decode-elements args
    #:entity-proc replace-stubs))

  ;; Expand splices afterwards
  (txexpr 'root empty (expand-splices decoded)))
```

We need to do it after `decode-elements` since it doesn't support such a transformation.

And we're done! It wasn't very straightforward to implement Tufte style notes but with Pollen you do get the ability to do it.



[prev-post]: /blog/2019/03/03/first_impressions_of_pollen/ "First impressions of Pollen"
[Tufte CSS]: https://edwardtufte.github.io/tufte-css/ "Tufte CSS"
[toggle-noscript]: https://stackoverflow.com/questions/11023816/toggle-divs-without-using-javascript "Toggle divs without using javascript"

