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
package org.grumblesmurf.malabar

import java.lang.reflect.*;

class SemanticReflector 
{
    def variable = new Symbol("variable");
    def typemodifiers = new Symbol(":typemodifiers");
    def type = new Symbol(":type");
    def function = new Symbol("function");
    def constructorFlag = new Symbol(":constructor-flag");
    def t = new Symbol("t");
    def arguments = new Symbol(":arguments");

    // helper
    def classpath = new Classpath();
    
    def asSemanticTag(Field f) {
        Utils.asLispList([ f.name, variable,
                           [ typemodifiers, Modifier.toString(f.modifiers).tokenize(),
                             type, classpath.typeString(f.getType(), true) ] ])
    }

    def asSemanticTag(Constructor c) {
        int counter = -1;
        Utils.asLispList([ c.declaringClass.simpleName, function,
                           [ constructorFlag, t,
                             typemodifiers, Modifier.toString(c.modifiers).tokenize(),
                             arguments, c.parameterTypes.collect {
                                 counter++;
                                 [ "arg${counter}", variable,
                                   [ type, classpath.typeString(it, true) ] ]
                             } ] ])
    }
}
