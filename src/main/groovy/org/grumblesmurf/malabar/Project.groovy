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
    }
}
