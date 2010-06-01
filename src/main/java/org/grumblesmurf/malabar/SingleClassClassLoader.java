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

public class SingleClassClassLoader
    extends ClassLoader 
{
    private String className;
    private byte[] classBytes;

    public SingleClassClassLoader(String className, byte[] classBytes, ClassLoader parent) {
        super(parent);
        this.className = className;
        this.classBytes = classBytes;
    }

    @Override
    public Class<?> findClass(String name)
        throws ClassNotFoundException {
        if (name.equals(className)) {
            return defineClass(name, classBytes, 0, classBytes.length);
        }
        throw new ClassNotFoundException(name);
    }
}
