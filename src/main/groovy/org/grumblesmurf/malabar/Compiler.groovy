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
        JavaCompiler compiler = ToolProvider.getSystemJavaCompiler();
        StandardJavaFileManager fileManager =
            compiler.getStandardFileManager(null, null, Charset.forName("UTF-8"));
        Iterable<? extends JavaFileObject> compilationUnits =
            fileManager.getJavaFileObjectsFromStrings(Arrays.asList(file));
        JavaCompiler.CompilationTask task =
            compiler.getTask(null, fileManager, null,
                             ["-cp", project.compileClasspath.asClassPath(),
                              "-g", "-deprecation",
                              "-d", project.classesDirectory,
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
