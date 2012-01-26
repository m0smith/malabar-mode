// Stolen more-or-less verbatim from the Groovy tree
// Changes:
// - Ripped out the Ansi stuff
// - Customized prompt
// - Removed user script behavior
/*
 * Copyright 2003-2011 the original author or authors.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package org.grumblesmurf.malabar

import jline.Terminal
import jline.History

import org.codehaus.groovy.tools.shell.util.MessageSource
import org.codehaus.groovy.tools.shell.util.XmlCommandRegistrar
import org.codehaus.groovy.runtime.StackTraceUtils
import org.codehaus.groovy.tools.shell.util.Preferences

import org.codehaus.groovy.tools.shell.BufferManager
import org.codehaus.groovy.tools.shell.ExitNotification
import org.codehaus.groovy.tools.shell.IO
import org.codehaus.groovy.tools.shell.InteractiveShellRunner
import org.codehaus.groovy.tools.shell.Interpreter
import org.codehaus.groovy.tools.shell.ParseCode
import org.codehaus.groovy.tools.shell.Parser
import org.codehaus.groovy.tools.shell.Shell


/**
 * An interactive shell for evaluating Groovy code from the command-line (aka. groovysh).
 *
 * @version $Id$
 * @author <a href="mailto:jason@planet57.com">Jason Dillon</a>
 */
class Groovysh extends Shell {

    private static final MessageSource messages = new MessageSource(org.codehaus.groovy.tools.shell.Groovysh.class)

    final BufferManager buffers = new BufferManager()

    final Parser parser

    final Interpreter interp
    
    final List imports = []
    
    final String project

    InteractiveShellRunner runner
    
    History history

    boolean historyFull  // used as a workaround for GROOVY-2177
    String evictedLine  // remembers the command which will get evicted if history is full

    Groovysh(final String project, final ClassLoader classLoader, final Binding binding, final IO io, final Closure registrar) {
        super(io)
        
        assert classLoader
        assert binding
        assert registrar

        this.project = project

        parser = new Parser()
        
        interp = new Interpreter(classLoader, binding)

        registrar.call(this)
        this << new SetProjectCommand(this)
    }

    private static Closure createDefaultRegistrar() {
        return { shell ->
            def r = new XmlCommandRegistrar(shell, classLoader)
            r.register(Shell.getResource('commands.xml'))
        }
    }

    Groovysh(final String project, final ClassLoader classLoader, final Binding binding, final IO io) {
        this(project, classLoader, binding, io, createDefaultRegistrar())
    }

    Groovysh(final String project, final Binding binding, final IO io) {
        this(project, Thread.currentThread().contextClassLoader, binding, io)
    }

    //
    // Execution
    //

    /**
     * Execute a single line, where the line may be a command or Groovy code (complete or incomplete).
     */
    Object execute(final String line) {
        assert line != null
        
        // Ignore empty lines
        if (line.trim().size() == 0) {
            return null
        }

        maybeRecordInput(line)

        def result
        
        // First try normal command execution
        if (isExecutable(line)) {
            result = executeCommand(line)
            
            // For commands, only set the last result when its non-null/true
            if (result) {
                lastResult = result
            }
            
            return result
        }
        
        // Otherwise treat the line as Groovy
        def current = []
        current += buffers.current()

        // Append the line to the current buffer
        current << line

        // Attempt to parse the current buffer
        def status = parser.parse(imports + current)

        switch (status.code) {
            case ParseCode.COMPLETE:
                log.debug("Evaluating buffer...")

                if (io.verbose) {
                    displayBuffer(buffer)
                }

                // Evaluate the current buffer w/imports and dummy statement
                def buff = imports + [ 'true' ] + current

                lastResult = result = interp.evaluate(buff)
                buffers.clearSelected()
                break

            case ParseCode.INCOMPLETE:
                // Save the current buffer so user can build up complex multi-line code blocks
                buffers.updateSelected(current)
                break

            case ParseCode.ERROR:
                throw status.cause

            default:
                // Should never happen
                throw new Error("Invalid parse status: $status.code")
        }

        return result
    }

    protected Object executeCommand(final String line) {
        return super.execute(line)
    }

    /**
     * Display the given buffer.
     */
    private void displayBuffer(final List buffer) {
        assert buffer

        buffer.eachWithIndex { line, index ->
            def lineNum = formatLineNumber(index)
            
            io.out.println(" ${lineNum}> $line")
        }
    }

    //
    // Prompt
    //

    /*
        Builds the command prompt name in 1 of 3 ways:
           1.  Checks the groovysh.prompt property passed into groovysh script.   -Dgroovysh.prompt="hello"
           2.  Checks an environment variable called GROOVYSH_PROMPT.             export GROOVYSH_PROMPT
           3.  If no value is defined returns the default groovy shell prompt.

        The code will always assume you want the line number in the prompt.  To implement differently overhead the render
        prompt variable.
     */
    private String buildPrompt(){
       def lineNum = formatLineNumber(buffers.current().size())
       def formattedPrompt = "groovy:${lineNum}> "

       def GROOVYSHELL_PROPERTY =  System.getProperty("groovysh.prompt")
       def GROOVYSHELL_ENV      =  System.getenv("GROOVYSH_PROMPT")

       if (GROOVYSHELL_PROPERTY)  return  "${GROOVYSHELL_PROPERTY}:${lineNum}> "
       if (GROOVYSHELL_ENV)       return  "${GROOVYSHELL_ENV}:${lineNum}> "

       return formattedPrompt
    }

    public String renderPrompt() {
        return buildPrompt()
    }

    /**
     * Format the given number suitable for rendering as a line number column.
     */
    private String formatLineNumber(final int num) {
        assert num >= 0

        // Make a %03d-like string for the line number
        return num.toString().padLeft(3, '0')
    }

    //
    // Recording
    //

    private void maybeRecordInput(final String line) {
        def record = registry['record']

        if (record != null) {
            record.recordInput(line)
        }
    }

    private void maybeRecordResult(final Object result) {
        def record = registry['record']

        if (record != null) {
            record.recordResult(result)
        }
    }

    private void maybeRecordError(Throwable cause) {
        def record = registry['record']

        if (record != null) {
            boolean sanitize = Preferences.sanitizeStackTrace

            if (sanitize) {
                cause = StackTraceUtils.deepSanitize(cause);
            }

            record.recordError(cause)
        }
    }
    
    //
    // Hooks
    //

    final Closure defaultResultHook = { result ->
        boolean showLastResult = !io.quiet && (io.verbose || Preferences.showLastResult)

        if (showLastResult) {
            // Need to use String.valueOf() here to avoid icky exceptions causes by GString coercion
            io.out.println("===> ${String.valueOf(result)}")
        }
    }

    Closure resultHook = defaultResultHook

    private void setLastResult(final Object result) {
        if (resultHook == null) {
            throw new IllegalStateException("Result hook is not set")
        }

        resultHook.call((Object)result)

        interp.context['_'] = result

        maybeRecordResult(result)
    }

    private Object getLastResult() {
        return interp.context['_']
    }

    final Closure defaultErrorHook = { Throwable cause ->
        assert cause != null

        io.err.println("ERROR ${cause.class.name}:")
        io.err.println("${cause.message}")

        maybeRecordError(cause)

        if (log.debug) {
            // If we have debug enabled then skip the fancy bits below
            log.debug(cause)
        }
        else {
            boolean sanitize = Preferences.sanitizeStackTrace

            // Sanitize the stack trace unless we are in verbose mode, or the user has request otherwise
            if (!io.verbose && sanitize) {
                cause = StackTraceUtils.deepSanitize(cause);
            }

            def trace = cause.stackTrace

            def buff = new StringBuffer()

            for (e in trace) {
                buff << "        at ${e.className}.${e.methodName} ("

                buff << (e.nativeMethod ? 'Native Method' :
                            (e.fileName != null && e.lineNumber != -1 ? "${e.fileName}:${e.lineNumber}" :
                                (e.fileName != null ? e.fileName : 'Unknown Source')))

                buff << ')'

                io.err.println(buff)

                buff.setLength(0) // Reset the buffer

                // Stop the trace once we find the root of the evaluated script
                if (e.className == Interpreter.SCRIPT_FILENAME && e.methodName == 'run') {
                    io.err.println('        ...')
                    break
                }
            }
        }
    }

    Closure errorHook = defaultErrorHook

    private void displayError(final Throwable cause) {
        if (errorHook == null) {
            throw new IllegalStateException("Error hook is not set")
        }

        errorHook.call(cause)
    }

    //
    // Interactive Shell
    //

    int run(final String[] args) {
        String commandLine = null

        if (args != null && args.length > 0) {
            commandLine = args.join(' ')
        }

        return run(commandLine as String)
    }

    int run(final String commandLine) {
        def term = Terminal.terminal

        if (log.debug) {
            log.debug("Terminal ($term)")
            log.debug("    Supported:  $term.supported")
            log.debug("    ECHO:       $term.echo (enabled: $term.echoEnabled)")
            log.debug("    H x W:      $term.terminalHeight x $term.terminalWidth")
            log.debug("    ANSI:       ${term.isANSISupported()}")

            if (term instanceof jline.WindowsTerminal) {
                log.debug("    Direct:     ${term.directConsole}")
            }
        }

        def code

        try {
            // if args were passed in, just execute as a command
            // (but cygwin gives an empty string, so ignore that)
            if (commandLine != null && commandLine.trim().size() > 0) {
                // Run the given commands
                execute(commandLine)
            }
            else {
                // Setup the interactive runner
                runner = new InteractiveShellRunner(this, this.&renderPrompt as Closure)

                // Setup the history
                runner.history = history = new History()

                // Setup the error handler
                runner.errorHandler = this.&displayError

                //
                // TODO: See if we want to add any more language specific completions, like for println for example?
                //

                // Display the welcome banner
                if (!io.quiet) {
                    def width = term.terminalWidth

                    // If we can't tell, or have something bogus then use a reasonable default
                    if (width < 1) {
                        width = 80
                    }

                    io.out.println(messages.format('startup_banner.0', GroovySystem.version, System.properties['java.version']))
                    io.out.println(messages['startup_banner.1'])
                    io.out.println('-' * (width - 1))
                }

                // And let 'er rip... :-)
                runner.run()
            }

            code = 0
        }
        catch (ExitNotification n) {
            log.debug("Exiting w/code: ${n.code}")

            code = n.code
        }
        catch (Throwable t) {
            io.err.println(messages.format('info.fatal', t))
            t.printStackTrace(io.err)

            code = 1
        }

        assert code != null // This should never happen

        return code
    }
}
