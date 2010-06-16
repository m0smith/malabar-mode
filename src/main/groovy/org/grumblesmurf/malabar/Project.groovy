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

import org.apache.maven.artifact.Artifact;
import org.apache.maven.artifact.resolver.filter.CumulativeScopeArtifactFilter;

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
    /**
     * In Maven terminology a project's coordinate uniquely identifies that
     * project in time.
     */
    def coordinate;

    def modStamp;

    def srcDirectories;
    def classesDirectory;

    def testSrcDirectories;
    def testClassesDirectory;

    def activeProfiles;
    def requestedProfiles;
    def availableProfiles;

    def encoding = "UTF-8" as String;
    def source = "1.3" as String;
    def target = "1.1" as String;

    def mavenProject;
    def request;

    def compiler;

    def mvnServer;
    
    def runtest(testname) {
        return run(["test"], [], [ test: testname ]);
    }

    def run(String... goals) {
        return run(goals, [], [:]);
    }

    def run(goals, profiles, properties) {
        def run = mvnServer.run(pomFile, false, goals as String[]);
        run.setProfiles(profiles as String[]);
        properties.each {
            run.addProperty(it.key, it.value);
        }
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

    def sourceJarForClass(String name) {
        def artifact = testClasspath.artifactForClass(name);
        if (artifact == null) {
            println "nil";
            return
        }
        
        def af = GroovyServer.mvnServer.plexus.lookup(org.apache.maven.artifact.factory.ArtifactFactory);
        def sourceArtifact =
            af.createArtifactWithClassifier(artifact.groupId,
                                            artifact.artifactId,
                                            artifact.version,
                                            artifact.type,
                                            "sources");
        def ar = mvnServer.plexus.lookup(org.apache.maven.artifact.resolver.ArtifactResolver);
        try {
            def localRepo = request.localRepository
            ar.resolve(sourceArtifact, request.remoteRepositories, localRepo);
            def f = new File(localRepo.basedir, localRepo.pathOf(sourceArtifact))
            println "\"${f}\""
        } catch (Exception e) {
            // TODO log
            println "nil"
        } 
    }

    def successfulCompilation() {
        [ compileClasspath, testClasspath, runtimeClasspath ].each {
            it.extraClassMap.clear()
        }
    }

    private Project(pom, profiles, request, mavenProject, mvnServer) {
        this.mvnServer = mvnServer
        this.request = request
        pomFile = pom
        requestedProfiles = profiles
        modStamp = (pom as File).lastModified()
        this.mavenProject = mavenProject
        name = mavenProject.name
        description = mavenProject.description
        coordinate = mavenProject.artifact.id

        activeProfiles = mavenProject.activeProfiles.collect { it.id }
        availableProfiles = mavenProject.model.profiles.collect { it.id }
        
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

        def filter = new CumulativeScopeArtifactFilter([Artifact.SCOPE_COMPILE])
        compileClasspath =
            new Classpath([classesDirectory] + resources,
                          mavenProject.artifacts.findAll {
                              it.artifactHandler.addedToClasspath && filter.include(it)
                          })

        filter = new CumulativeScopeArtifactFilter([Artifact.SCOPE_TEST])
        testClasspath =
            new Classpath([testClassesDirectory, classesDirectory] + resources + testResources,
                          mavenProject.artifacts.findAll {
                              it.artifactHandler.addedToClasspath && filter.include(it)
                          })

        filter = new CumulativeScopeArtifactFilter([Artifact.SCOPE_RUNTIME])
        runtimeClasspath =
            new Classpath([classesDirectory] + resources,
                          mavenProject.artifacts.findAll {
                              it.artifactHandler.addedToClasspath && filter.include(it)
                          })

        def compilerConfig = mavenProject.getPlugin("org.apache.maven.plugins:maven-compiler-plugin")?.configuration
        if (compilerConfig) {
            encoding = compilerConfig.getChild("encoding")?.value ?: encoding
            source = compilerConfig.getChild("source")?.value ?: source
            target = compilerConfig.getChild("target")?.value ?: target
        }

        compiler = new Compiler(this);
    }
}
