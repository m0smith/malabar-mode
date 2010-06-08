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

    static printAsLisp(Object o) {
        print asLisp(o)
    }

    static printAsLispList(List list) {
        print asLispList(list)
    }

    static printAsLispHashTable(Map map) {
        print asLispHashTable(map)
    }

    static asLisp(Object o) {
        if (o instanceof String) {
            "\"${o}\""
        } else if (o instanceof GString) {
            "\"${o}\""
        } else if (o instanceof List) {
            asLispList(o)
        } else if (o instanceof Map) {
            asLispHashTable(o)
        } else {
            o.toString()
        }
    }

    static asLispList(List list) {
        def result = new StringBuilder("(");
        result << list.collect {asLisp(it)}.join(" ");
        result << ")";
        return result as String;
    }

    static asLispHashTable(Map map) {
        def result = new StringBuilder("#s(hash-table test equal size ${map.size()} data (");
        result << map.collect {
            "${asLisp(it.key)} ${asLisp(it.value)}"
        }.join(" ");
        result << "))";
        return result as String;
    }
}
