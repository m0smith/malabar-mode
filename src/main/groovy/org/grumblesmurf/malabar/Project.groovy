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

import org.apache.maven.embedder.MavenEmbedder;
import org.apache.maven.execution.*;

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

    def encoding = "UTF-8";
    def source = "1.3";
    def target = "1.1";

    def mavenProject;

    def compiler;
    
    static Project makeProject(pom) {
        Project p = Projects.get(pom)
        File pomFile = pom as File
        if (p && p.modStamp >= pomFile.lastModified()) {
            return p
        }
        
        MavenEmbedder embedder = MvnServer.embedder
        MavenExecutionRequest req = MvnServer.newRequest()
        req.baseDirectory = pomFile.parentFile
        MavenExecutionResult result = embedder.readProjectWithDependencies(req)
        if (result.hasExceptions()) {
            // handle exceptions
            println '(error "%s" "' + result.exceptions + '")'
            return null;
        } else if (result.artifactResolutionResult.missingArtifacts) {
            println '(error "Missing artifacts: %s" "' + result.artifactResolutionResult.missingArtifacts + '")'
            return null;
        } else {
            Project me = new Project(pom, result);
            Projects.put(pom, me)
            return me;
        }
    }

    def runtest(testname) {
        def run = MvnServer.INSTANCE.run(pomFile, false, "test");
        run.addProperty("test", testname);
        return run.run();
    }

    def runJunit(testname) {
        // Extra care must be taken to ensure class reloadability
        def classloader = testClasspath.newClassLoader()
        def junitcore = classloader.loadClass("org.junit.runner.JUnitCore")
        def testclass = classloader.loadClass(testname)
        def result = junitcore.runClasses(testclass);
        result.failures.each{
            println it
        }
        println "Failures: ${result.failureCount}  Tests run: ${result.runCount}  Ignored: ${result.ignoreCount}"
        def seconds = result.runTime / 1000.0
        println "Took ${seconds} seconds"
        return result.wasSuccessful();
    }
    
    private Project(pom, result) {
        pomFile = pom
        modStamp = (pom as File).lastModified()
        mavenProject = result.project
        name = mavenProject.name
        description = mavenProject.description

        srcDirectories = mavenProject.compileSourceRoots
        classesDirectory = mavenProject.build.outputDirectory
        resources = mavenProject.resources.collect{
            // TODO: better resource handling
            it.directory
        }

        testSrcDirectories = mavenProject.testCompileSourceRoots
        testClassesDirectory = mavenProject.build.testOutputDirectory
        testResources = mavenProject.testResources.collect{
            // TODO: better resource handling
            it.directory
        }

        compileClasspath = new Classpath(mavenProject.compileClasspathElements);
        testClasspath = new Classpath(mavenProject.testClasspathElements);
        runtimeClasspath = new Classpath(mavenProject.runtimeClasspathElements);

        def compilerConfig = mavenProject.getPlugin("org.apache.maven.plugins:maven-compiler-plugin").configuration
        if (compilerConfig) {
            encoding = (compilerConfig.getChild("encoding") ?: encoding).value
            source = (compilerConfig.getChild("source") ?: source).value
            target = (compilerConfig.getChild("target") ?: target).value
        }

        compiler = new Compiler(this);
    }
}
