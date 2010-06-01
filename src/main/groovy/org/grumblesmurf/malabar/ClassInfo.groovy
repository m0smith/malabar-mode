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
package org.grumblesmurf.malabar

import org.objectweb.asm.*;

class ClassInfo implements ClassVisitor, MethodVisitor
{
    def methodParameters = [:];

    def currentMethod;
        
    public void visit(int version, int access, String name, String signature, String supername, String[] interfaces) {
    }
    public AnnotationVisitor visitAnnotation(String desc, boolean visible) {
        return null
    }
    public void visitAttribute(Attribute attr) {
    }
    public void visitEnd() {
    }
    public FieldVisitor visitField(int access, String name, String desc, String signature, Object value) {
        return null
    }
    public void visitInnerClass(String name, String outername, String innername, int access) {
    }
    public MethodVisitor visitMethod(int access, String name, String desc, String signature, String[] exceptions) {
        currentMethod = "${name}${desc}"
        methodParameters[currentMethod] = []
        return this
    }
    public void visitOuterClass(String owner, String name, String desc) {
    }
    public void visitSource(String source, String debug) {
    }

    public AnnotationVisitor visitAnnotationDefault() {
        return null
    }

    public void visitLineNumber(int a, Label b) {}
    public void visitVarInsn(int a, int b) {}
    public void visitFieldInsn(int a, String b, String c, String d) {}
    public void visitLabel(Label a) {}
    public void visitTableSwitchInsn(int a, int b, Label c, Label[] d) {}
    public void visitMethodInsn(int a, String b, String c, String d) {}
    public void visitInsn(int a) {}
    public void visitLookupSwitchInsn(Label a, int[] b, Label[] c) {}
    public void visitTypeInsn(int a, String b) {}
    public void visitMaxs(int a, int b) {}
    public void visitIincInsn(int a, int b) {}
    public void visitIntInsn(int a, int b) {}
    public void visitCode() {}
    public void visitTryCatchBlock(Label a, Label b, Label c, String d) {}
    public void visitLocalVariable(String name, String desc, String signature, Label start, Label end, int index) {
        methodParameters[currentMethod] << [
            name: name,
            desc: desc,
            signature: signature
            ];
    }
    public void visitLdcInsn(Object a) {}
    public AnnotationVisitor visitParameterAnnotation(int a, String b, boolean c) {}
    public void visitJumpInsn(int a, Label b) {}
    public void visitMultiANewArrayInsn(String a, int b) {}
}
