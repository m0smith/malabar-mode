package org.grumblesmurf.malabar

abstract class Project
{
    def compileClasspath;
    def runtimeClasspath;
    def testClasspath;

    def name;
    def pomFile;

    def srcDirectory;
    def classesDirectory;

    def testSrcDirectory;
    def testClassesDirectory;

    def encoding = "UTF-8";
    def source = "1.3";
    def target = "1.1";

}
