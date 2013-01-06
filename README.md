# malabar-mode :: A better Java mode for Emacs

malabar-mode extends java-mode with hooks into Maven that make
it easy to compile files on the fly and execute Maven build
commands.

This is a fork of
[buzztaiki's fork](https://github.com/buzztaiki/malabar-mode) of
[espenhw's malabar-mode](https://github.com/espenhw/malabar-mode). Development
of malabar-mode has stagnated. This fork is an attempt to keep
it up to date and develop at a modest pace.

You may want to skip to [Installation](#Installation).

## What malabar-mode offers

Since malabar-mode is derived from java-mode, we get some things for free:

- Syntax highlighting

- Movement commands (`C-M-f/-b` is a winner; so is `M-f/-b`, especially
  with `c-subword-mode` turned on)

- Electric punctuation

There's lots more; and since this is Emacs, you can turn off or modify
anything you don't like.

### But there is more:

- Tight integration with [Maven][]. If
  you're not using Maven, you should not consider malabar-mode.

- A [Groovy][] console for rapid prototyping and exploratory programming

- [JUnit][] integration, both for running tests standalone and through Maven

- Import help; import one class or all needed classes in the buffer
  (with prompting if the class name is ambiguous)

- Extend class / implement interface / override method helpers

- Simplistic refactorings

and more.

<a name="Installation" />
# Installation

malabar-mode must be installed by hand. There's
[an issue for making it installable from a package manager](https://github.com/dstu/malabar-mode/issues/1),
but for now you must build it yourself.

## Prerequisites

### Emacs

malabar-mode was originally developed on Emacs 23, but
development now targets Emacs 24.

### CEDET

A relatively recent [CEDET][] is needed in your Emacs
environment. If you have a version of Emacs 23.2 or later,
malabar-mode should work fine with the embedded CEDET.

## Build latest development snapshot from source

1. Clone the repository from
   `git://github.com/dstu/malabar-mode.git`.

2. Build with `mvn package`. If load-test.el fails, it is most
   likely because CEDET is not on your load-path.

3. Unpack the resulting `malabar-<version>-dist.zip` somewhere
   
4. Add `malabar-<version>/lisp` to your Emacs load-path
   
5. Add the following to your .emacs::

        (require 'cedet)
        (semantic-load-enable-minimum-features) ;; or enable more if you wish
        (require 'malabar-mode)
        (setq malabar-groovy-lib-dir "/path/to/malabar/lib")
        (add-to-list 'auto-mode-alist '("\\.java\\'" . malabar-mode))
        
   Alternatively, using Emacs 23.2+ and the embedded CEDET:
   
        ;; Or enable more if you wish
        (setq semantic-default-submodes '(global-semantic-idle-scheduler-mode
                                          global-semanticdb-minor-mode
                                          global-semantic-idle-summary-mode
                                          global-semantic-mru-bookmark-mode))
        (semantic-mode 1)
        (require 'malabar-mode)
        (setq malabar-groovy-lib-dir "/path/to/malabar/lib")
        (add-to-list 'auto-mode-alist '("\\.java\\'" . malabar-mode))

6. (optional) If you want to mimic the IDEish compile-on-save
   behaviour, add the following as well::

        (add-hook 'malabar-mode-hook
             (lambda () 
               (add-hook 'after-save-hook 'malabar-compile-file-silently
                          nil t)))
          
# Usage

Update:  malabar-mode now has a menu.  Yay!

Here is a list of available interactive commands, with default
keybindings where applicable:

<dl>
<dt>malabar-compile-file <span class="classifier">(C-c C-v C-c)</span></dt>
<dd>Compiles the current file.</dd>

<dt>malabar-clear-typecache</dt>
<dd>Clears the type cache (used for code completion) if it gets
  confused.  If you have to use this often, please file a bug.</dd>
  
<dt>malabar-extend-class <span class="classifier">(C-c C-v C-e)</span></dt>
<dd>Prompts for a class, adds stub implementations of all that class's
  abstract methods and accessible constructors and inserts the
  appropriate extends clause.</dd>
  
<dt>malabar-groovy-start</dt>
<dd>Start the Groovy console, or pop to it if it is running.</dd>

<dt>malabar-groovy-stop</dt>
<dd>Kill the Groovy console process.</dd>

<dt>malabar-implement-interface <span class="classifier">(C-c C-v C-i)</span></dt>
<dd>Prompts for an interface, adds stub implementations of all that
  interface's methods and adds the interface to the class's implements
  clause.</dd>
  
<dt>malabar-import-all <span class="classifier">(C-c C-v z)</span></dt>
<dd>Adds import statements for all unqualified classes in the buffer, as
  if by performing <code>malabar-import-one-class</code> on each.</dd>
  
<dt>malabar-import-one-class <span class="classifier">(C-c C-v C-z)</span></dt>
<dd><p>Adds an import statement for a single unqualified class (defaults to
  the symbol at point).  If more than one class matches the
  unqualified name you will be asked which class to import.</p>

  <p>The variable <code>malabar-import-excluded-classes-regexp-list</code>
  contains a list of regular expressions; if one of these matches the
  qualified class name, the class will be excluded from import.  The
  default value excludes classes from <code>java.lang</code>, JRE internal
  classes and inner classes.</p></dd>
  
<dt>malabar-run-maven-command</dt>
<dd>Prompts for and executes an (almost) arbitrary Maven command line.
  Honors profile activation, property definitions and lifecycle
  phases/goals.  E.g.: <code>-DskipTests=true -Pdev-mode install</code> will
  run the install lifecycle with the dev-mode profile active, skipping
  tests.</dd>
  
<dt>malabar-install-project <span class="classifier">(C-c C-v C-b)</span></dt>
<dd>Runs <code>mvn install</code> on your project.  With prefix argument (C-u),
  cleans the project first (<code>mvn clean install</code>).</dd>
  
<dt>malabar-override-method <span class="classifier">(C-c C-v C-o)</span></dt>
<dd>Prompts for an eligible method from the superclass of the class at
  point and adds a stub implementation of that method.  If the chosen
  method is one of <code>Object.equals` or `Object.hashCode</code>, override both of them.</dd>
  
<dt>malabar-run-all-tests <span class="classifier">(C-c C-v M-t)</span></dt>
<dd>Runs <code>mvn test</code> on your project.  With prefix argument (C-u),
  cleans the project first (<code>mvn clean test</code>).</dd>
  
<dt>malabar-run-junit-test-no-maven <span class="classifier">(C-c C-v C-t)</span></dt>
<dd>Compiles the current file, performs
  <code>malabar-visit-corresponding-test</code>, compiles that file (if not the
  same as where we started) and runs the now-current buffer as a
  standalone JUnit test.</dd>
  
<dt>malabar-run-test <span class="classifier">(C-c C-v t)</span></dt>
<dd>Runs the corresponding test to this buffer using Maven (<code>mvn test -Dtest=classname</code>)</dd>

<dt>malabar-update-package</dt>
<dd>Updates the package statement of the current buffer to match its place
  in the source directory.</dd>
  
<dt>malabar-visit-corresponding-test</dt>
<dd><p>Visits the corresponding test class; that is, the file in the
  parallel src/test/java hierarchy that matches the class in the
  current buffer (with <code>malabar-test-class-suffix</code> appended).</p>

  <p>E.g., <code>M-x malabar-visit-corresponding-test</code> in a buffer visiting
  <code>src/main/java/org/grumblesmurf/malabar/MvnServer.java</code> will visit
  the file
  <code>src/test/java/org/grumblesmurf/malabar/MvnServerTest.java</code> with
  the default value of <code>malabar-test-class-suffix</code>.</p>

  <p>If the current buffer looks like a test class, this command does nothing.</p></dd>
  
<dt>malabar-visit-project-file <span class="classifier">(C-c C-v C-p)</span></dt>
<dd>Visit the project file, that is the closest file named <code>pom.xml</code>
  searching upwards in the directory structure.</dd>
  
<dt>malabar-jump-to-thing <span class="classifier">(C-c C-v C-y)</span></dt>
<dd>Jumps to the definition of the 'thing' at point. More technically,
  uses <code>semantic-analyze-current-context</code> output to identify an origin
  for the code at point, taking type membership into account.  This
  function is much like <code>semantic-ia-fast-jump</code>, only a little
  smarter.</dd>
  
<dt>malabar-refactor-extract-constant <span class="classifier">(C-c C-v C-r C-c)</span></dt>
<dd>Extracts the thing at point as a named constant.  The scope of the
  constant will default to
  <code>malabar-refactor-extract-constant-default-scope</code>, but with a
  prefix arg will prompt for the scope.</dd>
</dl>

In addition, [standard Semantic code completion][] is available; trigger
this however you wish.  By default, `semantic-ia-complete-symbol` is
bound to `C-c C-v C-.` and `semantic-ia-complete-symbol-menu` is
bound to `C-c C-v .`.

## Abbrevs

Some default abbrevs are set up, see the variable
`malabar-case-fixed-abbrevs` for the current list.

Note the presence of the `#Test` abbrev; this expands to::

     @Test
     public void |() throws Exception {
         fail("Unfinished test");
     }

With point left at the position marked with `|`.

## Electric expansions

In addition to the electric insertion offered by CC-mode, malabar-mode
offers an expansion that simulates the [Elvis operator][], although the
generated code is not as efficient as a language-provided Elvis
operator would be.  This expansion is controlled by the variable
`malabar-electric-elvis-p`.

# This is cool, I want to help

Github has excellent support for forking!  Just hit the fork button at
the top, code and go.  For everyone's convenience, you should probably
rebase to espenhw/malabar-mode/master every now and then.  :)

If you don't want to go that far, git is, after all, a *distributed*
VCS.  Just commit to your local repository and then use
git-format-patch to extract patches in a useful format.

## But where do I send patches?

To the [issue tracker][] (see the next section).

# I found a bug!

Good for you.  Create a ticket in the [issue tracker][] and stuff will happen.

Hint #1:  Tell me what you did, what you expected to happen and what
actually happened.  Include any error messages (Emacs backtraces,
output in the buffers named starting with `*Malabar`, interesting
stuff from `*Messages*` etc.).

Hint #2:  Bugs with patches tend to be fixed faster (see the previous
section).

# Wouldn't it be cool if malabar-mode could...

Yes, it probably would!  Either describe the feature that you want in
the [issue tracker][], or (even better) fork, code, and ask me to pull.

And of course, if I nix your feature request, you're free to maintain
your own local patch branch if you wish (or, for that matter, a
complete fork).  malabar-mode is Open Source, after all.

# Hacking

This fork of malabar-mode uses [git-flow][] to manage branching
in development. As such, branches are:

<dl>
<dt>develop</dt>
<dd>tracks the latest state of development (that isn't occurring in an isolated branch)</dd>
<dt>master</dt>
<dd>tracks the latest stable snapshot</dd>
</dl>

Feature, release, feature, hotfix, and support branches won't
usually be shared. If they are pushed to github, they shouldn't
be rebased but may disappear after they are merged with
`develop` or `master`.

## Emacs environment

Your Emacs environment can be configured to use a "live" version
of malabar-mode with the following steps:

1. Add the following to your `.emacs`:

    (setq malabar-groovy-lib-dir "~/src/malabar-mode/target/lib")
    (setq malabar-groovy-extra-classpath '("~/src/malabar-mode/target/classes"))
    (add-to-list 'load-path "~/src/malabar-mode/src/main/lisp/")

2. Run `mvn package -P devel` to extract libraries into
   `target/lib`.
   
With this configuration, you can rebuild malabar-mode's JVM
component with `mvn compile`, which will compile classes into
`target/classes`. To apply these changes, restart malabar-mode
in Emacs with `M-x malabar-groovy-restart`.

After editing elisp files, eval them to apply changes
immediately.

# Acknowledgments

* [JDEE][] for being a source of frustration and inspiration (and sometimes of code)
* [Nikolaj Schumacher][] for fringe-helper and elk-test
* [espenhw](https://github.com/espenhw) for getting the whole thing rolling
* [nflath](https://github.com/nflath), [stepb](https://github.com/stepb) and [bbatsov](https://github.com/bbatsov) for caring enough to help
* Everybody else for caring enough read this and report bugs

# Boring legal stuff

malabar-mode is copyright (c) 2009-2010 Espen Wiborg <espenhw@grumblesmurf.org>

This program is free software; you can redistribute it and/or
modify it under the terms of the GNU General Public License as
published by the Free Software Foundation; either version 2 of the
License, or (at your option) any later version.

This program is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.

For the full text of the GPL, see <http://www.gnu.org/licenses/gpl2.txt>.

[JDEE]: http://jdee.sourceforge.net/
[run jdb on an applet]: http://jdee.sourceforge.net/jdedoc/html/jde-ug/jde-ug-content.html#d0e4142
[BeanShell]: http://www.beanshell.org/
[my blog]: http://blog.grumblesmurf.org/
[Maven]: http://maven.apache.org/
[CEDET]: http://cedet.sourceforge.net/
[Groovy]: http://groovy.codehaus.org/
[Junit]: http://www.junit.org/
[issue tracker]: http://github.com/dstu/malabar-mode/issues
[Nikolaj Schumacher]: http://nschum.de/src/emacs/
[standard Semantic code completion]: http://cedet.sourceforge.net/intellisense.shtml
[Elvis operator]: http://groovy.codehaus.org/Operators#Operators-ElvisOperator
[git-flow]: http://nvie.com/posts/a-successful-git-branching-model/
