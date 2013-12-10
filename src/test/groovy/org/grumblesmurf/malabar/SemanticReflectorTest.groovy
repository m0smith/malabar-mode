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

import java.lang.reflect.*;

import test.*;

class SemanticReflectorTest
{
    def sr;

    @Before
    void setup() {
        sr = new SemanticReflector();
    }

    @Test
    void noFields() {
        assertThat(sr.collectFields(EmptyClass), is([]));
    }
    
    @Test
    void oneField() {
        assertThat(sr.collectFields(OneField).size(), is(1));
    }
    
    @Test
    void twoFields() {
        assertThat(sr.collectFields(TwoFields).size(), is(2));
    }

    @Test
    void noInnerClass() {
        assertThat(sr.collectClasses(EmptyClass), is([]));
    }
    
    @Test
    void innerClass() {
        assertThat(sr.collectClasses(InnerClass).size(), is(1));
    }
    
    @Test
    void innerClassIsInherited() {
        assertThat(sr.collectClasses(InheritedInnerClass).size(), is(1));
    }
    
    @Test
    void fieldGivesSemanticTag() throws Exception {
        Field f = TwoFields.getDeclaredField("field2");
        assertThat(sr.asSemanticTag(f), is('("field2" variable (:typemodifiers ("public") :type "java.lang.Object" :declaring-class "test.TwoFields"))'));
    }

    @Test
    void fieldGivesSemanticTagPrimitive() throws Exception {
        Field f = OneField.getDeclaredField("field");
        assertThat(sr.asSemanticTag(f), is('("field" variable (:typemodifiers ("public") :type "int" :declaring-class "test.OneField"))'));
    }

    @Test
    void fieldGivesSemanticTagStaticFinal() throws Exception {
        Field f = StaticField.getDeclaredField("STATIC_STRING");
        assertThat(sr.asSemanticTag(f), is('("STATIC_STRING" variable (:typemodifiers ("public" "static" "final") :type "java.lang.String" :declaring-class "test.StaticField"))'));
    }

    @Test
    void simpleConstructor() throws Exception {
        Constructor c = EmptyClass.getDeclaredConstructor();
        assertThat(sr.asSemanticTag(c), is('("EmptyClass" function (:constructor-flag t :typemodifiers ("public") :arguments () :declaring-class "test.EmptyClass"))'));
    }

    @Test
    void complexConstructor() throws Exception {
        Constructor c = ComplexConstructor.getDeclaredConstructor(Integer.TYPE);
        assertThat(sr.asSemanticTag(c), is('("ComplexConstructor" function (:constructor-flag t :typemodifiers ("public") :arguments (("arg0" variable (:type "int"))) :declaring-class "test.ComplexConstructor"))'));
    }

    @Test
    void simpleMethod() throws Exception {
        Method m = InterfaceWithMethods.getDeclaredMethod("mu");
        assertThat(sr.asSemanticTag(m), is('("mu" function (:typemodifiers ("public" "abstract") :arguments () :type "void" :declaring-class "test.InterfaceWithMethods"))'));
    }

    @Test
    void methodWithOneArgument() throws Exception {
        Method m = InterfaceWithMethods.getDeclaredMethod("hasBuddhaNature", Object);
        assertThat(sr.asSemanticTag(m), is('("hasBuddhaNature" function (:typemodifiers ("public" "abstract") :arguments (("arg0" variable (:type "java.lang.Object"))) :type "boolean" :declaring-class "test.InterfaceWithMethods"))'));
    }

    @Test
    void methodWithGenericArgument() throws Exception {
        Method m = InterfaceWithMethods.getDeclaredMethod("meditate", Long.TYPE, Object);
        assertThat(sr.asSemanticTag(m), is('("meditate" function (:typemodifiers ("public" "abstract") :arguments (("arg0" variable (:type "long")) ("arg1" variable (:type "E"))) :type "void" :declaring-class "test.InterfaceWithMethods"))'));
    }

    @Test
    void methodWithComplicatedGenericArgument() throws Exception {
        Method m = InterfaceWithMethods.getDeclaredMethod("meditateAll", Collection);
        assertThat(sr.asSemanticTag(m), is('("meditateAll" function (:typemodifiers ("public" "abstract") :arguments (("arg0" variable (:type "java.util.Collection<? extends E>"))) :type "boolean" :declaring-class "test.InterfaceWithMethods"))'));
    }

    @Test
    void methodWithGenericReturnAndTypeArgument() throws Exception {
        Method m = InterfaceWithMethods.getDeclaredMethod("joshu", Object[]);
        assertThat(sr.asSemanticTag(m), is('("joshu" function (:typemodifiers ("public" "abstract") :arguments (("arg0" variable (:type "T" :dereference 1))) :type "T[]" :template-specifier "<T>" :declaring-class "test.InterfaceWithMethods"))'));
    }

    @Test
    void staticMethodWithException() throws Exception {
        Method m = ClassWithMethods.getDeclaredMethod("snore", Long.TYPE);
        assertThat(sr.asSemanticTag(m), is('("snore" function (:typemodifiers ("public" "static") :arguments (("arg0" variable (:type "long"))) :type "void" :throws ("java.lang.InterruptedException") :declaring-class "test.ClassWithMethods"))'));
    }

    @Test
    void simpleInterface() throws Exception {
        assertThat(sr.asSemanticTag(SimpleInterface), is('("test.SimpleInterface" type (:typemodifiers ("public" "abstract" "interface") :members () :type "interface"))'))
    }

    @Test
    void anotherSimpleInterface() throws Exception {
        assertThat(sr.asSemanticTag(SimpleGenericInterface), is('("test.SimpleGenericInterface" type (:typemodifiers ("public" "abstract" "interface") :template-specifier "<E>" :members (("iterator" function (:typemodifiers ("public" "abstract") :arguments () :type "java.util.Iterator<E>" :declaring-class "test.SimpleGenericInterface"))) :type "interface"))'))
    }

    @Test
    void subInterfaceWithMembers() throws Exception {
        assertThat(sr.asSemanticTag(SubInterface), is('("test.SubInterface" type (:typemodifiers ("public" "abstract" "interface") :interfaces ("test.SimpleGenericInterface<E>") :template-specifier "<E>" :members (("isEmpty" function (:typemodifiers ("public" "abstract") :arguments () :type "boolean" :declaring-class "test.SubInterface")) ("iterator" function (:typemodifiers ("public" "abstract") :arguments () :type "java.util.Iterator<E>" :declaring-class "test.SimpleGenericInterface"))) :type "interface"))'))
    }

    @Test
    void subClassWithInterface() throws Exception {
        assertThat(sr.asSemanticTag(AbstractClass), is('("test.AbstractClass" type (:typemodifiers ("public" "abstract") :superclasses "test.AbstractParent<E>" :interfaces ("test.SubInterface<E>") :template-specifier "<E>" :members (("AbstractClass" function (:constructor-flag t :typemodifiers ("public") :arguments () :declaring-class "test.AbstractClass")) ("isEmpty" function (:typemodifiers ("public") :arguments () :type "boolean" :declaring-class "test.AbstractClass")) ("iterator" function (:typemodifiers ("public") :arguments () :type "java.util.Iterator<E>" :declaring-class "test.AbstractParent")) ("clone" function (:typemodifiers ("protected" "native") :arguments () :type "java.lang.Object" :throws ("java.lang.CloneNotSupportedException") :declaring-class "java.lang.Object")) ("equals" function (:typemodifiers ("public") :arguments (("arg0" variable (:type "java.lang.Object"))) :type "boolean" :declaring-class "java.lang.Object")) ("finalize" function (:typemodifiers ("protected") :arguments () :type "void" :throws ("java.lang.Throwable") :declaring-class "java.lang.Object")) ("getClass" function (:typemodifiers ("public" "final" "native") :arguments () :type "java.lang.Class<?>" :declaring-class "java.lang.Object")) ("hashCode" function (:typemodifiers ("public" "native") :arguments () :type "int" :declaring-class "java.lang.Object")) ("notify" function (:typemodifiers ("public" "final" "native") :arguments () :type "void" :declaring-class "java.lang.Object")) ("notifyAll" function (:typemodifiers ("public" "final" "native") :arguments () :type "void" :declaring-class "java.lang.Object")) ("toString" function (:typemodifiers ("public") :arguments () :type "java.lang.String" :declaring-class "java.lang.Object")) ("wait" function (:typemodifiers ("public" "final") :arguments () :type "void" :throws ("java.lang.InterruptedException") :declaring-class "java.lang.Object")) ("wait" function (:typemodifiers ("public" "final" "native") :arguments (("arg0" variable (:type "long"))) :type "void" :throws ("java.lang.InterruptedException") :declaring-class "java.lang.Object")) ("wait" function (:typemodifiers ("public" "final") :arguments (("arg0" variable (:type "long")) ("arg1" variable (:type "int"))) :type "void" :throws ("java.lang.InterruptedException") :declaring-class "java.lang.Object"))) :type "class"))'));
    }
}
