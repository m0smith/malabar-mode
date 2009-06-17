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

1. You probably already have Emacs.  However, you may want to consider
   getting Emacs 23 (at the time of writing it is in pre-test), as
   that's what I develop this beast for.

2. Get CEDET_ and arrange for it to be on you Emacs load-path (I
   develop using CVS HEAD; older versions may or may not work),
   e.g. by linking the CEDET directory into your site-lisp directory.

3. Clone the repository from git://github.com/espenhw/malabar-mode.git.
   
4. Build with ``mvn package``.  If load-test.el fails, it is most
   likely because CEDET is not on your load-path.
   
5. Unpack the resulting ``malabar-<version>-dist.zip`` somewhere
   
6. Add ``malabar-<version>/lisp`` to your Emacs load-path
   
7. Add the following to your .emacs::

     (require 'cedet)
     (semantic-load-enable-minimum-features) ;; or enable more if you wish
     (require 'malabar-mode)
     (setq malabar-groovy-lib-dir "/path/to/malabar/lib")
     (add-to-list 'auto-mode-alist '("\\.java\\'" . malabar-mode))

=======
 Usage
=======

Here is a list of available interactive commands, with default
keybindings where applicable:

malabar-compile-file : C-c C-v C-c
  Compiles the current file.

malabar-clear-typecache
  Clears the type cache (used for code completion) if it gets
  confused.  If you have to use this often, please file a bug.
  
malabar-extend-class : C-c C-v C-e
  Prompts for a class, adds stub implementations of all that class's
  abstract methods and accessible constructors and inserts the
  appropriate extends clause.

malabar-groovy-start
  Start the Groovy console, or pop to it if it is running.

malabar-groovy-stop
  Kill the Groovy console process.

malabar-implement-interface : C-c C-v C-i
  Prompts for an interface, adds stub implementations of all that
  interface's methods and adds the interface to the class's implements
  clause.

malabar-import-all : C-c C-v z
  Adds import statements for all unqualified classes in the buffer, as
  if by performing ``malabar-import-one-class`` on each.

malabar-import-one-class : C-c C-v C-z
  Adds an import statement for a single unqualified class (defaults to
  the symbol at point).  If more than one class matches the
  unqualified name you will be asked which class to import.

  The variable ``malabar-import-excluded-classes-regexp-list``
  contains a list of regular expressions; if one of these matches the
  qualified class name, the class will be excluded from import.  The
  default value excludes classes from ``java.lang``, JRE internal
  classes and inner classes.

malabar-run-maven-command
  Prompts for and executes an (almost) arbitrary Maven command line.
  Honors profile activation, property definitions and lifecycle
  phases/goals.  E.g.: ``-DskipTests=true -Pdev-mode install`` will
  run the install lifecycle with the dev-mode profile active, skipping
  tests.

malabar-install-project : C-c C-v C-b
  Runs ``mvn install`` on your project.  With prefix argument (C-u),
  cleans the project first (``mvn clean install``).

malabar-override-method : C-c C-v C-o
  Prompts for an eligible method from the superclass of the class at
  point and adds a stub implementation of that method.  If the chosen
  method is one of ``Object.equals`` or ``Object.hashCode``, override both of them.

malabar-run-all-tests : C-c C-v M-t
  Runs ``mvn test`` on your project.  With prefix argument (C-u),
  cleans the project first (``mvn clean test``).
  
malabar-run-junit-test-no-maven : C-c C-v C-t
  Compiles the current file, performs
  ``malabar-visit-corresponding-test``, compiles that file (if not the
  same as where we started) and runs the now-current buffer as a
  standalone JUnit test.

malabar-run-test : C-c C-v t
  Runs the corresponding test to this buffer using Maven (``mvn test -Dtest=classname``)

malabar-update-package
  Updates the package statement of the current buffer to match its place
  in the source directory.

malabar-visit-corresponding-test
  Visits the corresponding test class; that is, the file in the
  parallel src/test/java hierarchy that matches the class in the
  current buffer (with ``malabar-test-class-suffix`` appended).

  E.g., ``M-x malabar-visit-corresponding-test`` in a buffer visiting
  ``src/main/java/org/grumblesmurf/malabar/MvnServer.java`` will visit
  the file
  ``src/test/java/org/grumblesmurf/malabar/MvnServerTest.java`` with
  the default value of ``malabar-test-class-suffix``.

  If the current buffer looks like a test class, this command does nothing.

malabar-visit-project-file : C-c C-v C-p
  Visit the project file, that is the closest file named ``pom.xml``
  searching upwards in the directory structure.

In addition, `standard Semantic code completion`_ is available; trigger
this however you wish.  By default, ``semantic-ia-complete-symbol`` is
bound to ``C-c C-v C-.`` and ``semantic-ia-complete-symbol-menu`` is
bound to ``C-c C-v .``.

Abbrevs
=======

Some default abbrevs are set up, see the variable
``malabar-case-fixed-abbrevs`` for the current list.

Note the presence of the ``#Test`` abbrev; this expands to::

     @Test
     public void |() throws Exception {
         fail("Unfinished test");
     }

With point left at the position marked with ``|``.

============================
This is cool, I want to help
============================

Github has excellent support for forking!  If you don't want to go
that far, git is, after all, a *distributed* VCS.  Just commit to your
local repository and then use git-format-patch to extract patches in a
useful format.

But where do I send patches?
============================

To the `issue tracker`_ (see the next section).

===============
 I found a bug!
===============

Good for you.  Create a ticket in the `issue tracker`_ and stuff will happen.

Hint #1:  Tell me what you did, what you expected to happen and what
actually happened.  Include any error messages (Emacs backtraces,
output in the buffers named starting with ``*Malabar``, interesting
stuff from ``*Messages*`` etc.).

Hint #2:  Bugs with patches tend to be fixed faster (see the previous
section).

==============================================
 Wouldn't it be cool if malabar-mode could...
==============================================

Yes, it probably would!  Either describe the feature that you want in
the `issue tracker`_, or (even better) fork, code, and ask me to pull.

And of course, if I nix your feature request, you're free to maintain
your own local patch branch if you wish (or, for that matter, a
complete fork).  malabar-mode is Open Source, after all.

=================
 Acknowledgments
=================

* JDEE for being a source of frustration and inspiration (and sometimes of code)
* `Nikolaj Schumacher`_ for fringe-helper and elk-test

====================
 Boring legal stuff
====================

malabar-mode is copyright (c) 2009 Espen Wiborg <espenhw@grumblesmurf.org>

This program is free software; you can redistribute it and/or
modify it under the terms of the GNU General Public License as
published by the Free Software Foundation; either version 2 of the
License, or (at your option) any later version.

This program is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.

For the full text of the GPL, see http://www.gnu.org/licenses/gpl2.txt.

.. _JDEE: http://jdee.sourceforge.net/
.. _run jdb on an applet: http://jdee.sourceforge.net/jdedoc/html/jde-ug/jde-ug-content.html#d0e4142
.. _BeanShell: http://www.beanshell.org/
.. _my blog: http://blog.grumblesmurf.org/
.. _Maven: http://maven.apache.org/
.. _CEDET: http://cedet.sourceforge.net/
.. _Groovy: http://groovy.codehaus.org/
.. _Junit: http://www.junit.org/
.. _issue tracker: http://github.com/espenhw/malabar-mode/issues
.. _Nikolaj Schumacher: http://nschum.de/src/emacs/
.. _standard Semantic code completion: http://cedet.sourceforge.net/intellisense.shtml
