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

import org.junit.Test;
import org.junit.Before;

import static org.junit.Assert.*;
import static org.junit.matchers.JUnitMatchers.*;
import static org.hamcrest.CoreMatchers.*;

class ProjectsTest 
{
    Project p;
    
    @Before
    void instantiateProject() {
        p = Projects.get(System.getProperty("basedir") + "/pom.xml", []);
    }
    
    @Test
    void projectPopulated() {
        assertThat(p.name, is("Malabar: A better Java mode for Emacs"));
    }

    @Test
    void coordinatePopulated() {
        assertThat(p.coordinate, containsString("org.grumblesmurf"));
    }

    @Test
    void coordinateMapPopulated() {
        assertThat(Projects.getProjectsCoordinateMap().get(p.coordinate), is(p.pomFile));
    }

    @Test
    void transitiveCompileClasspath() {
        def cp = p.compileClasspath
        assertThat(cp.asClassPath(), containsString("groovy"))
    }

    @Test
    void transitiveTestClasspath() {
        def cp = p.testClasspath
        assertThat(cp.asClassPath(), containsString("gmaven-runtime"))
    }

    @Test
    void runAGoal() {
        assertThat(p.mvnServer, is(not(nullValue())));
        assertThat(p.run("validate"), is(true));
    }
}
