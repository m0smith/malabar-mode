/**
 * Copyright (c) 2009 Espen Wiborg <espenhw@grumblesmurf.org>
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

class Compiler
{
    def project;

    Compiler(project) {
        this.project = project
    }

    def compile(file) {
        def output = project.classesDirectory;
        def classpath = project.compileClasspath;
        if (project.testSrcDirectories.any{ file.startsWith(it) }) {
            output = project.testClassesDirectory
            classpath = project.testClasspath
        }
        (output as File).mkdirs()
        
        JavaCompiler compiler = ToolProvider.getSystemJavaCompiler();
        StandardJavaFileManager fileManager =
            compiler.getStandardFileManager(null, null, Charset.forName("UTF-8"));
        Iterable<? extends JavaFileObject> compilationUnits =
            fileManager.getJavaFileObjectsFromStrings([file]);
        JavaCompiler.CompilationTask task =
            compiler.getTask(Utils.getOut(), fileManager, null,
                             ["-cp", classpath.asClassPath(),
                              "-g", "-deprecation",
                              "-d", output,
                              "-source", project.source,
                              "-target", project.target,
                              "-encoding", project.encoding,
                              "-Xlint:all", "-Xlint:-serial"],
                             null, compilationUnits);
        boolean success = task.call();
        fileManager.close();
        return success;
    }
}
