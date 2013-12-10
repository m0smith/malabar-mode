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
package org.grumblesmurf.malabar;

import org.apache.maven.execution.*;
import org.apache.maven.project.ProjectBuildingRequest;
import org.apache.maven.project.DefaultProjectBuildingRequest;
import org.apache.maven.project.MavenProject;
import org.apache.maven.project.ProjectBuilder;

class Projects
{
    static def projects = [:];

    static Project get(pom, profiles) {
        Project p = projects[pom]
        File pomFile = pom as File
        if (p && p.requestedProfiles == profiles && p.modStamp >= pomFile.lastModified()) {
            return p
        }

        MvnServer mvnServer = GroovyServer.mvnServer;
        MavenExecutionRequest req = mvnServer.newRequest(pomFile.parentFile, profiles)

        ProjectBuildingRequest config = req.getProjectBuildingRequest()
            .setProcessPlugins(true)
            .setResolveDependencies(true);

        MavenProject mavenProject = mvnServer.withComponent(ProjectBuilder.class) {
            it.build(pomFile, config).project
        }
        
        // TODO: Error handling!
        Project me = new Project(pom, profiles, req, mavenProject, mvnServer);
        projects[pom] = me
        return me;
    }

    static getProjectsCoordinateMap() {
        def map = [:];
        projects.each {
            def pom = it.key;
            def pomFile = pom as File;
            def p = it.value;
            if (pomFile.exists()) {
                if (p.modStamp < pomFile.lastModified()) {
                    p = get(pom, p.requestedProfiles);
                }
                map[p.coordinate] = pom;
            }
        }
        return map;
    }
}
