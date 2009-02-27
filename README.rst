==============================================
 malabar-mode :: A better Java mode for Emacs
==============================================

You may want to skip to Installation_

Why yet another Java mode?
==========================

After all there is java-mode, included with recent Emacsen, and
JDEE_ if you want something more IDEish.  So why yet another?

java-mode
---------

There's nothing (much) wrong with java-mode, but it doesn't provide
enough power to do the things a Java developer (I, at least) needs to
do every day.

JDEE
----

Enter JDEE.  It's big, it's powerful, it probably includes more
features than you'll ever need; I mean, who needs to `run jdb on an
applet`_ these days?

Yet even so, something is lacking.  It's something big, it's something
new, it's something no Java developer can live without these days:

Generics.

That's right; use generics (or enums, or foreach loops - annotations,
on the other hand, are quite reasonably supported) and JDEE will, at
best, get confused.  Part of the reason is that JDEE uses BeanShell_
underneath; BSH is, sadly, unmaintained and lacks support for Java 5
features.

So why not use an IDE?
======================

Because, in my arrogant opinion, the current crop of IDEs is complete
and utter crap, not worth the bits to store or the CPU cycles to run
(and *definitely* not worth the enormous amounts of memory they
require).  I have a major rant brewing on this subject; watch `my
blog`_ if you care.

You see, when it comes down to brass tacks, code is really just text.
And Emacs beats any other text editor out there hands down (vi lovers,
I hear you; I like vi, too, but Emacs is just *better*).

What malabar-mode offers
========================

Since malabar-mode is derived from java-mode, we get some things for free:

- Syntax highlighting

- Movement commands (C-M-f/-b is a winner; so is M-f/-b, especially
  with c-subword-mode turned on)

- Electric punctuation

There's lots more; and since this is Emacs, you can turn off or modify
anything you don't like.

But there is more:
------------------

- Tight integration with Maven_

- A Groovy_ console for rapid prototyping and exploratory programming

- JUnit_ integration, both for running tests standalone and through Maven

- Import help; import one class or all needed classes in the buffer
  (with prompting if the class name is ambiguous)

- Extend class / implement interface / override method helpers

and more.

==============
 Installation
==============

As malabar-mode is still in a state of considerable flux, there is no
released version.  Fortunately, building is pretty easy:

1. Make sure you have Maven and Groovy installed (and git)
   
2. Clone the repository from git://github.com/espenhw/malabar-mode.git
   
3. Build with ``mvn package``
   
4. Unpack the resulting ``malabar-<version>-dist.zip`` somewhere
   
5. Add ``malabar-<version>/lisp`` to your Emacs load-path
   
6. Add the following to your .emacs::
   
     (require 'malabar-mode)
     (setq malabar-groovy-lib-dir "/path/to/malabar/lib")
     ;; If groovysh is not on your PATH
     (setq malabar-groovy-command "/path/to/groovysh")
     (add-to-list 'auto-mode-alist '("\\.java\\'" . malabar-mode))

=======
 Usage
=======

Here is a list of available interactive commands:

malabar-import-one-class : C-c C-v C-z
  Adds an import statement for a single unqualified class (defaults to
  the symbol at point).  If more than one class matches the
  unqualified name you will be asked which class to import.

  The variable ``malabar-import-excluded-classes-regexp-list``
  contains a list of regular expressions; if one of these matches the
  qualified class name, the class will be excluded from import.  The
  default value excludes classes from ``java.lang``, JRE internal
  classes and inner classes.

malabar-import-all : C-c C-v z
  Adds import statements for all unqualified classes in the buffer, as
  if by performing malabar-import-one-class on each.

malabar-install-project : C-c C-v C-b
  Runs ``mvn install`` on your project.

malabar-compile-file : C-c C-v C-c
  Compiles the current file.

malabar-visit-corresponding-test
  Visits the corresponding test class; that is, the file in the
  parallel src/test/java hierarchy that matches the class in the
  current buffer (with ``malabar-test-class-suffix`` appended).

  E.g., ``M-x malabar-visit-corresponding-test`` in a buffer visiting
  ``src/main/java/org/grumblesmurf/malabar/MvnServer.java`` will visit
  the file
  ``src/test/java/org/grumblesmurf/malabar/MvnServerTest.java`` with
  the default value of ``malabar-test-class-suffix``.

  If the current buffer satisfies ``malabar-test-class-buffer-p``,
  this command does nothing.

malabar-run-junit-test-no-maven : C-c C-v C-t
  Runs the corresponding test to this buffer using JUnit.

malabar-run-test : C-c C-v t
  Runs the corresponding test to this buffer using Maven (``mvn test -Dtest=classname``)

malabar-goto-start-of-class
  Moves point to the beginning of the class at point (supports inner classes).

malabar-goto-end-of-class
  Moves point to the end of the class at point (supports inner classes).

malabar-override-method : C-c C-v C-o
  Prompts for an eligible method from the superclass of the class at
  point and adds a stub implementation of that method.  If the chosen
  method is Object.equals or Object.hashCode, override both.

malabar-update-package
  Updates the package statement of the current buffer to match its place
  in the source directory.

malabar-implement-interface : C-c C-v C-i
  Prompts for an interface, adds stub implementations of all that
  interface's methods and adds the interface to the class's implements
  clause.

malabar-extend-class : C-c C-v C-e
  Prompts for a class, adds stub implementations of all that class's
  abstract methods and accessible constructors and inserts the
  appropriate extends clause.

malabar-test-class-buffer-p
  Not really a command, but it is central to malabar-mode's function;
  this predicate decides whether a buffer uses Maven's test or compile
  scope.

  In essence, the predicate tests whether the primary class in the
  current buffer either

  a. extends junit.framework.TestCase or junit.framework.TestSuite or

  b. contains a method annotated with an annotation named Test

============================
This is cool, I want to help
============================

Github has excellent support for forking!  If you don't want to go
that far, git is, after all, a *distributed* VCS.  Just commit to your
local repository and then use git-format-patch to extract patches in a
useful format.

But where do I send patches?
============================

To the issue tracker (see the next section).

===========================
 I found a bug!  You suck!
===========================

Quite possibly.  I have an issue tracker over at Lighthouse_; create a
ticket there and I will do my best to help you.

Hint:  Bugs with patches tend to be fixed faster...

==============================================
 Wouldn't it be cool if malabar-mode could...
==============================================

Yes!  Either describe the feature that you want in the issue tracker,
or (even better) fork, code, and ask me to pull.

.. _JDEE: http://jdee.sourceforge.net/
.. _run jdb on an applet: http://jdee.sourceforge.net/jdedoc/html/jde-ug/jde-ug-content.html#d0e4142
.. _BeanShell: http://www.beanshell.org/
.. _my blog: http://blog.grumblesmurf.org/
.. _Maven: http://maven.apache.org/
.. _Groovy: http://groovy.codehaus.org/
.. _Junit: http://www.junit.org/
.. _Lighthouse: http://espenhw.lighthouseapp.com/projects/26275-malabar-mode/
