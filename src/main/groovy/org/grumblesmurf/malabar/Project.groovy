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

class Project
{
    def compileClasspath;
    def runtimeClasspath;
    def testClasspath;

    def resources;
    def testResources;

    def name;
    def description;
    def pomFile;

    def modStamp;

    def srcDirectories;
    def classesDirectory;

    def testSrcDirectories;
    def testClassesDirectory;

    def encoding = "UTF-8" as String;
    def source = "1.3" as String;
    def target = "1.1" as String;

    def mavenProject;

    def compiler;

    def mvnServer;
    
    def runtest(testname) {
        def run = mvnServer.run(pomFile, false, "test");
        run.addProperty("test", testname);
        return run.run();
    }

    def run(String... goals) {
        def run = mvnServer.run(pomFile, false, goals);
        return run.run();
    }

    def runJunit(testname) {
        // Extra care must be taken to ensure class reloadability
        def classloader = testClasspath.newClassLoader()
        def junitcore = classloader.loadClass("org.junit.runner.JUnitCore").newInstance()
        def testclass = classloader.loadClass(testname)
        // our RunListener must be loaded by a descendant of the test
        // classloader, otherwise all kinds of weird errors happen
        def byteStream = new ByteArrayOutputStream();
        def listenerName = MalabarRunListener.name;
        this.class.classLoader.getResourceAsStream(listenerName.replace('.', '/') + ".class").eachByte {
            byteStream.write(it)
        }
        def runListenerClassLoader =
            new SingleClassClassLoader(listenerName,
                                       byteStream.toByteArray(),
                                       classloader);
        junitcore.addListener(runListenerClassLoader.loadClass(listenerName).newInstance(Utils.getOut()));
        def result = junitcore.run(testclass);
        return result.wasSuccessful();
    }
    
    private Project(pom, result, mvnServer) {
        this.mvnServer = mvnServer
        pomFile = pom
        modStamp = (pom as File).lastModified()
        mavenProject = result.project
        name = mavenProject.name
        description = mavenProject.description

        srcDirectories = mavenProject.compileSourceRoots
        classesDirectory = mavenProject.build.outputDirectory
        resources = mavenProject.resources.collect {
            // TODO: better resource handling
            it.directory
        }

        testSrcDirectories = mavenProject.testCompileSourceRoots
        testSrcDirectories += testSrcDirectories.collect {
            it.replaceFirst('src/test/java', 'src/test/groovy')
        }
        testClassesDirectory = mavenProject.build.testOutputDirectory
        testResources = mavenProject.testResources.collect {
            // TODO: better resource handling
            it.directory
        }

        compileClasspath = new Classpath(mavenProject.compileClasspathElements + resources);
        testClasspath = new Classpath(mavenProject.testClasspathElements + resources + testResources);
        runtimeClasspath = new Classpath(mavenProject.runtimeClasspathElements);

        def compilerConfig = mavenProject.getPlugin("org.apache.maven.plugins:maven-compiler-plugin")?.configuration
        if (compilerConfig) {
            encoding = compilerConfig.getChild("encoding")?.value ?: encoding
            source = compilerConfig.getChild("source")?.value ?: source
            target = compilerConfig.getChild("target")?.value ?: target
        }

        compiler = new Compiler(this);
    }
}
