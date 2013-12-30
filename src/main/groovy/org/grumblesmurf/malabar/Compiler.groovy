/**
 * Copyright (c) 2009, 2010 Espen Wiborg <espenhw@grumblesmurf.org>
 * 
 * This program is free software; you can redistribute it and/or
 * modify it under the terms of the GNU General Public License as
 * published by the Free Software Foundation; either version 2 of the
 * License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
 * 02110-1301 USA.
 */ 
package org.grumblesmurf.malabar

import javax.tools.*;
import java.nio.charset.Charset;


import org.codehaus.groovy.control.*;
import org.codehaus.groovy.control.messages.WarningMessage;

class Compiler
{
    def project;
    def LOGGER;

    Compiler(project) {
        this.project = project
        this.LOGGER = project.LOGGER;
        //LOGGER.setThreshold(LOGGER.LEVEL_INFO);
    }

    def compile(file) {
        def output = project.classesDirectory;
        def classpath = project.compileClasspath;
        if (project.testSrcDirectories.any{ file.startsWith(it) }) {
            output = project.testClassesDirectory
            classpath = project.testClasspath
        }
        return compile(file, classpath, output);
    }
    
    def compile(file, classpath, output) {
        (output as File).mkdirs()
        if (file.endsWith(".java")) {
            def compiler = ToolProvider.getSystemJavaCompiler();
            def fileManager =
                compiler.getStandardFileManager(null, null, Charset.forName("UTF-8"));
            def compilationUnits =
                fileManager.getJavaFileObjectsFromStrings([file]);
            def diagnosticCollector = new DiagnosticCollector()
            def task =
                compiler.getTask(Utils.getOut(), fileManager, diagnosticCollector,
                                 ["-cp", classpath.asClassPath(),
                                  "-g", "-deprecation",
                                  "-d", output,
                                  "-source", project.source,
                                  "-target", project.target,
                                  "-encoding", project.encoding,
                                  "-Xlint:all", "-Xlint:-serial"],
                                 null, compilationUnits);
            def success = task.call();
	    LOGGER.debug(String.format("compiler result: %s",success));
	    LOGGER.debug(String.format("diagnostics: %s",diagnosticCollector.diagnostics));

            diagnosticCollector.diagnostics.each {
                if (it.source) {
		    def info = String.format("source: %s linenumber: %d, columnNumber: %d strartposition: %d endposition: %d position: %d kind: %s message %s", 
		                             it.source.getClass(), 
					     it.lineNumber,
					     it.columnNumber,
					     it.startPosition, 
					     it.endPosition, 
					     it.position,	
					     it.kind,
					     it.getMessage(null));
                    LOGGER.debug(info);
		    try {                           
                    	def src = new File(it.source.toUri().toString()).path
			def start = [src, it.lineNumber].join(":")
                        def message = it.getMessage(null).replace(start + ":", "")
                        println([it.kind, Utils.standardizeSlashes(src), 
                                 it.lineNumber, it.columnNumber,
                                 it.startPosition, it.endPosition, it.position,
                                 message].join("::"))
		    } catch (IllegalArgumentException ex ) {
			throw new IllegalArgumentException(ex.getMessage() + " " + info, ex);
		    }
                } else {
                    println("[${it.kind}] ${it.getMessage(null)}")
                }
            }
            fileManager.close();
            if (success)
               project.successfulCompilation();
            return success;
        } else if (file.endsWith(".groovy")) {
            CompilerConfiguration cc = new CompilerConfiguration(
                classpath:classpath.asClassPath(),
                output:Utils.getOut(),
                sourceEncoding:project.encoding,
                targetDirectory:output as File,
                tolerance:0,
                verbose:true,
                warningLevel:WarningMessage.PARANOIA)
            
            org.codehaus.groovy.tools.Compiler compiler = new org.codehaus.groovy.tools.Compiler(cc);
            try {
                compiler.compile(file);
            } catch (CompilationFailedException e) {
		LOGGER.debug("Groovy compiler failure", e);
                return false;
            }
            project.successfulCompilation();
            return true;
        }
        return false;
    }
}
