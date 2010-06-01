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

import org.codehaus.groovy.tools.RootLoader;
import java.util.jar.*;

class Classpath 
{
    def artifacts = [];
    
    def extraEntries = [];
    
    def bootUrls = [];

    def extUrls = [];
    
    def urls = [];
    
    Classpath() {
        System.getProperty("sun.boot.class.path").split(File.pathSeparator).each() {
            bootUrls << "file:" + it
        }

        System.getProperty("java.ext.dirs").split(File.pathSeparator).each() { dir ->
            try {
                (dir as File).eachFileMatch(~/.*.jar/) {
                    extUrls << "file:" + it
                }
            } catch (FileNotFoundException e) {
                // EAT IT
            }
        }
    }

    Classpath(extraEntries, artifacts) {
        this()
        this.artifacts = artifacts;
        
        this.extraEntries = extraEntries.collect {
            if ((it as File).isDirectory() && !it.endsWith("/"))
              it = it + "/"
            this.urls << "file:" + it
            "file:" + it
        }
        
        artifacts.each {
            if (it.file)
                this.urls << "file:" + it.file.path
        }
    }

    def asClassPath() {
        return urls.collect{it.substring(5)}.join(':')
    }
    
    private classloader;

    def classMap = [:]
    def extraClassMap = [:]
    def classToArtifact = [:]

    // TODO: This is not good
    def currentArtifact
        
    def classnamecollector = { fileName, path, map ->
        def classbinaryname = fileName[0..-7];
        def simplename = classbinaryname[classbinaryname.lastIndexOf('$') + 1..-1]
        def pkgname = path.replace('/', '.')
                        
        if (!map.containsKey(simplename)) {
            map[simplename] = []
        }
        map[simplename] << pkgname + "." + classbinaryname
        if (currentArtifact)
            classToArtifact[pkgname + "." + classbinaryname] = currentArtifact
    }
            
    def classcollector = { it, map ->
        URI uri = new URI(it);
        File file = new File(uri);
        String absolutePath = file.absolutePath
        if (file.exists()) {
            if (file.isFile()) {
                new JarFile(file).entries().each{ entry ->
                    if (entry.name.endsWith(".class")) {
                        def entryname = entry.name
                        def filename = entryname[entryname.lastIndexOf('/') + 1..-1]
                        def path = ""
                        if (entryname.lastIndexOf('/') > -1) {
                            path = entryname.substring(0, entryname.lastIndexOf('/'))
                        }
                        classnamecollector(filename, path, map)
                    }
                }
            } else {
                file.eachFileRecurse{ classFile ->
                    if (classFile.name.endsWith(".class")) {
                        classnamecollector(classFile.name,
                                           classFile.parent[absolutePath.length() + 1 .. -1],
                                           map)
                    }
                }
            }
        }
    }
            
    def getClasses(String name) {
        if (name.contains(".")) {
            try {
                getClassLoader().loadClass(name);
                Utils.printAsLispList([name]);
            } catch (ClassNotFoundException e) {
                Utils.printAsLispList([]);
            }
            return;
        }
        
        if (classMap.isEmpty()) {
            populateClassMap();
        }
        populateExtraClassMap();

        Utils.printAsLispList classMapEntry(name);
    }

    def classMapEntry(name) {
        if (extraClassMap.containsKey(name))
            extraClassMap[name];
        else
            classMap[name];
    }

    def populateExtraClassMap() {
        extraEntries.each {
            classcollector(it, extraClassMap)
        }
    }

    def populateClassMap() {
        bootUrls.each{
            classcollector(it, classMap)
        }
        extUrls.each{
            classcollector(it, classMap)
        }
        artifacts.each {
            currentArtifact = it
            if (it.file)
                classcollector("file:" + it.file.path, classMap)
        }
    }

    def artifactForClass(String name) {
        if (classMap.isEmpty()) {
            populateClassMap()
        }
        return classToArtifact[name];
    }
    
    def newClassLoader() {
        def realUrls = urls.collect { new URL(it) }
        return new RootLoader(realUrls as URL[],
                              ClassLoader.systemClassLoader.parent)
    }
        
    def getClassLoader() {
        if (classloader == null) {
            classloader = newClassLoader();
        }
        return classloader
    }

    def getClassInfo(className) {
        try {
            Class c = this.classLoader.loadClass(className)
            println new SemanticReflector().asSemanticTag(c)
        } catch (ClassNotFoundException e) {
            Utils.printAsLispList([]);
        }
    }
}
