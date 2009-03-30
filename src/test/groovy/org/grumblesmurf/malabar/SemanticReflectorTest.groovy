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

import org.junit.Test;
import org.junit.Before;

import static org.junit.Assert.*;
import static org.junit.matchers.JUnitMatchers.*;
import static org.hamcrest.CoreMatchers.*;

import java.lang.reflect.*;

class SemanticReflectorTest
{
    def sr;

    @Before
    void setup() {
        sr = new SemanticReflector();
    }

    @Test
    void fieldGivesSemanticTag() throws Exception {
        Field f = FilterReader.getDeclaredField("in");
        assertThat(sr.asSemanticTag(f), is('("in" variable (:typemodifiers ("protected") :type "java.io.Reader"))'));
    }

    @Test
    void fieldGivesSemanticTagPrimitive() throws Exception {
        Field f = javax.swing.AbstractAction.getDeclaredField("enabled");
        assertThat(sr.asSemanticTag(f), is('("enabled" variable (:typemodifiers ("protected") :type "boolean"))'));
    }

    @Test
    void fieldGivesSemanticTagStaticFinal() throws Exception {
        Field f = javax.swing.AbstractButton.getDeclaredField("BORDER_PAINTED_CHANGED_PROPERTY");
        assertThat(sr.asSemanticTag(f), is('("BORDER_PAINTED_CHANGED_PROPERTY" variable (:typemodifiers ("public" "static" "final") :type "java.lang.String"))'));
    }

    @Test
    void simpleConstructor() throws Exception {
        Constructor c = ArrayList.getDeclaredConstructor();
        assertThat(sr.asSemanticTag(c), is('("ArrayList" function (:constructor-flag t :typemodifiers ("public") :arguments ()))'));
    }

    @Test
    void complexConstructor() throws Exception {
        Constructor c = ArrayList.getDeclaredConstructor(Integer.TYPE);
        assertThat(sr.asSemanticTag(c), is('("ArrayList" function (:constructor-flag t :typemodifiers ("public") :arguments (("arg0" variable (:type "int")))))'));
    }
}
