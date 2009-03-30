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
    def templateSpecifier = new Symbol(":template-specifier");
    def throwsSym = new Symbol(":throws");
    def typeSym = new Symbol("type");
    def superclasses = new Symbol(":superclasses");
    def members = new Symbol(":members");
    def interfaces = new Symbol(":interfaces");
    def dereference = new Symbol(":dereference");

    def modifierSpec(modifiers) {
        modifiers ? [ typemodifiers, Modifier.toString(modifiers).tokenize() ] : []
    }
    
    def typeSpec(type, variable=false) {
        if (!type) {
            return []
        }

        def baseType = type;
        def extra = []
        
        if (variable && type instanceof GenericArrayType) {
            int dim = 0;
            while (baseType instanceof GenericArrayType) {
                dim++;
                baseType = baseType.genericComponentType
            }
            if (dim) {
                extra = [ dereference, dim ]
            }
        }

        [ this.type, typeString(baseType , true) ] + extra
    }
    
    def templateSpec(typeParameters) {
        typeParameters ? [ this.templateSpecifier, "<" + typeParameters.join(",") + ">" ] : []
    }
    
    def argumentSpec(arguments) {
        int counter = -1;
        [ this.arguments, arguments.collect {
                counter++;
                variable("arg${counter}", it)
            } ]
    }
    
    def throwSpec(exceptions) {
        exceptions ? [ this.throwsSym, exceptions.collect {
                typeString(it, true)
            } ] : []
    }
    
    def variable(name, type, modifiers=null) {
        [ name, variable,
          modifierSpec(modifiers) +
          typeSpec(type, true) ]
    }
    
    def function(name, parameterTypes,
                 modifiers=null, type=null, typeParameters=null, exceptions=null,
                 constructor=false) {
        [ name, function,
          (constructor ? [ constructorFlag, t ] : []) +
          modifierSpec(modifiers) +
          argumentSpec(parameterTypes) +
          typeSpec(type) +
          templateSpec(typeParameters) +
          throwSpec(exceptions) ]
    }

    def asSemanticTag(f) {
        Utils.asLispList(asSemanticTagList(f))
    }

    def asSemanticTagList(Field f) {
        variable(f.name, f.type, f.modifiers);
    }

    def asSemanticTagList(Constructor c) {
        function(c.declaringClass.simpleName, c.genericParameterTypes,
                 c.modifiers, null, c.typeParameters,
                 c.genericExceptionTypes,
                 true)
    }

    def asSemanticTagList(Method m) {
        function(m.name, m.genericParameterTypes,
                 m.modifiers, m.genericReturnType,
                 m.typeParameters, m.genericExceptionTypes)
    }

    def asSemanticTagList(Class c) {
        String tag = "class";
        if (c.isInterface()) {
            tag = "interface";
        }
        if (c.isEnum()) {
            tag = "enum";
        }

        def classMembers = []
        classMembers += c.declaredFields as List
        classMembers += c.declaredClasses as List
        classMembers += c.declaredConstructors as List
        classMembers += c.declaredMethods as List
        
        [ c.name, typeSym,
          modifierSpec(c.modifiers) +
          (c.superclass ? [ superclasses, typeString(c.genericSuperclass, true) ] : []) +
          (c.interfaces ? [ interfaces, c.genericInterfaces.collect {
                  typeString(it, true)
              } ] : []) +
          templateSpec(c.typeParameters) +
          [ members, classMembers.collect { asSemanticTagList(it) } ] +
          [ type, tag ] ]
    }

    def typeString(type, qualify=false) {
        def str;

        if (type instanceof Class) {
            if (type.isArray()) {
                str = typeString(type.componentType, qualify) + "[]"
            } else {
                str = type.name;

                if (type.typeParameters) {
                    str += "<"
                    str += type.typeParameters.join(", ")
                    str += ">"
                }
            }
        } else if (type instanceof TypeVariable) {
            str = type.name;
            if (type.bounds.length > 1 ||
                type.bounds[0] != Object) {
                str += " extends "
                str += type.bounds.collect{ typeString(it, qualify) }.join(" & ")
            }
        } else {
            str = type.toString();
        }

        if (qualify) {
            return str;
        }
        def brackpos = str.indexOf("<")
        if (brackpos < 0)
            brackpos = str.length()

        return str.substring(str.lastIndexOf(".", brackpos) + 1)
    }
}