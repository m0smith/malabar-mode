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

import org.codehaus.groovy.tools.shell.IO;
    
class Utils 
{
    static ThreadLocal<IO> _io = new ThreadLocal<IO>();

    static PrintWriter getOut() {
        return _io.get()?.out ?: new PrintWriter(System.out)
    }

    static setIO(IO io) {
        _io.set(io);
    }
    
    static print(Object v) {
        getOut().print(v);
    }
    
    static println(Object v) {
        getOut().println(v);
    }

    static printAsLispList(List list) {
        print asLispList(list)
    }

    static asLispList(List list) {
        def result = new StringBuilder("(");
        result << list.collect {
            if (it instanceof String) {
                "\"${it}\""
            } else if (it instanceof GString) {
                "\"${it}\""
            } else if (it instanceof List) {
                asLispList(it)
            } else {
                it
            }
        }.join(" ");
        result << ")";
        return result as String;
    }
}
