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
package org.grumblesmurf.malabar;

import org.apache.maven.Maven;

import org.apache.maven.cli.Configuration;
import org.apache.maven.cli.DefaultConfiguration;
import org.apache.maven.cli.MavenCli;
import org.apache.maven.cli.MavenLoggerManager;

import org.apache.maven.execution.ExecutionListener;
import org.apache.maven.execution.DefaultMavenExecutionRequest;
import org.apache.maven.execution.MavenExecutionRequest;
import org.apache.maven.execution.MavenExecutionRequestPopulationException;
import org.apache.maven.execution.MavenExecutionRequestPopulator;
import org.apache.maven.execution.MavenExecutionResult;

import org.apache.maven.repository.ArtifactTransferListener;

import org.apache.maven.settings.MavenSettingsBuilder;

import org.codehaus.plexus.ContainerConfiguration;
import org.codehaus.plexus.DefaultContainerConfiguration;
import org.codehaus.plexus.DefaultPlexusContainer;

import org.codehaus.plexus.classworlds.ClassWorld;

import org.codehaus.plexus.logging.Logger;

public class MvnServer
{
    final Configuration configuration;
    final Logger logger;
    final ArtifactTransferListener transferListener;
    final ExecutionListener executionListener;
    final Maven maven;

    final def plexus

    MvnServer() {
        ClassWorld classWorld = new ClassWorld("plexus.core",
                                               Thread.currentThread().getContextClassLoader());
        ContainerConfiguration cc =
            new DefaultContainerConfiguration(classWorld:classWorld, name:"embedder");

        plexus = new DefaultPlexusContainer(cc);
        logger = new MvnServerLogger();
        plexus.loggerManager = new MavenLoggerManager(logger);

        maven = plexus.lookup(Maven.class);
        
        configuration = buildEmbedderConfiguration();
        transferListener = new MvnServerTransferListener();
        executionListener = new ExecutionEventLogger(logger);
    }
    
    public MavenExecutionRequest newRequest(basedir, profiles) {
        return new DefaultMavenExecutionRequest(
            userSettingsFile:configuration.userSettingsFile,
            globalSettingsFile:configuration.globalSettingsFile,
            baseDirectory:basedir,
            transferListener:transferListener,
            executionListener:executionListener).with { req ->
            def settings = withComponent(MavenSettingsBuilder.class) {
                it.buildSettings(req);
            }
            
            withComponent(MavenExecutionRequestPopulator.class) {
                it.populateDefaults(req);
                it.populateFromSettings(req, settings);
            }
            
            profiles.each {
                req.addActiveProfile(it);
            }
            req
        }
    }

    def withComponent(c, clos) {
        def comp = plexus.lookup(c);
        try {
            return clos(comp);
        } finally {
            plexus.release(comp);
        }
    }

    Configuration buildEmbedderConfiguration() {
        return new DefaultConfiguration(
            userSettingsFile:MavenCli.DEFAULT_USER_SETTINGS_FILE,
            globalSettingsFile:MavenCli.DEFAULT_GLOBAL_SETTINGS_FILE)
    }

    public boolean run(String pomFileName, String... goals) {
        return run(pomFileName, false, goals).run();
    }

    public RunDescriptor run(String pomFileName, boolean recursive, String... goals) {
        return new RunDescriptor(
            mvnServer:this,
            pom:pomFileName as File,
            recursive:recursive,
            goals:goals);
    }
}

class RunDescriptor 
{
    File pom;
    boolean recursive;
    String[] goals;
    String[] profiles = new String[0];
    Properties properties = new Properties();
    
    MvnServer mvnServer;

    public RunDescriptor addProperty(String key, String value) {
        properties.put(key, value);
        return this;
    }
    
    public boolean run() {
        PrintStream oldOut = System.out;
        PrintStream oldErr = System.err;
        try {
            if (Utils._io.get()) {
                System.setOut(new PrintStream(Utils._io.get().outputStream));
                System.setErr(new PrintStream(Utils._io.get().errorStream));
            }
                    
            mvnServer.newRequest(pom.parentFile, profiles).with {
                goals = Arrays.asList(owner.goals)
                recursive = owner.recursive
                userProperties = owner.properties
                
                mvnServer.maven.execute(delegate).with {
                    !hasExceptions();
                }
            }
        } finally {
            System.setOut(oldOut);
            System.setErr(oldErr);
        }
    }
}
