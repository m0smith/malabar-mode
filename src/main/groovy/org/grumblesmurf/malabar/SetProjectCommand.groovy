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

import org.codehaus.groovy.tools.RootLoader;
import org.codehaus.groovy.tools.shell.CommandSupport;
import org.codehaus.groovy.tools.shell.Shell;
import org.codehaus.groovy.tools.shell.util.SimpleCompletor;

class SetProjectCommand
    extends CommandSupport
{
    SetProjectCommand(final Shell shell) {
        super(shell, 'set-project', '\\sp');
    }

    protected List createCompletors() {
        def loader = {
            def list = [];
            def keys = Projects.projects.keySet();
            keys.each { list << it };
            return list;
        }
        
        return [
                new SimpleCompletor(loader),
                null
                ];
    }

    Object execute(final List args) {
        if (args.size() < 1) {
            fail("Command '$name' requires argument: <pom>");
        }

        def pom = args[0];
        Project p = Projects.get(pom, []);
        def classpathUrls = p.runtimeClasspath.urls.collect { new URL(it) };

        ClassLoader c = new RootLoader(classpathUrls as URL[], shell.interp.classLoader);
        
        println "Type 'exit' to leave this project'";
        new Groovysh(p.name, c, new Binding(), shell.io).run();
    }
}
