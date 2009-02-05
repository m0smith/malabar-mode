import org.codehaus.groovy.tools.RootLoader;
import org.objectweb.asm.*;
import java.lang.reflect.*;

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

class Classpath 
{
    def bootUrls = [];

    def extUrls = [];
    
    def urls = [];
    
    Classpath() {
        System.getProperty("sun.boot.class.path").split(File.pathSeparator).each() {
            bootUrls << "file:" + it
        }

        System.getProperty("java.ext.dirs").split(File.pathSeparator).each() { dir ->
            try {
                (dir as File).eachFileMatch(~/.*.jar/) {
                    extUrls << "file:" + it
                }
            } catch (FileNotFoundException e) {
                // EAT IT
            }
        }
    }

    Classpath(entries) {
        this()
        entries.each{
            if ((it as File).isDirectory() && !it.endsWith("/"))
              it = it + "/"
            this.urls << "file:" + it
        }
    }
    
    private classloader;
    
    def getClassLoader() {
        if (classloader == null) {
            URL[] realUrls = new URL[urls.size()];
            urls.eachWithIndex() { it, i ->
                realUrls[i] = new URL(it)
            }
            classloader = new RootLoader(realUrls,
                                         ClassLoader.systemClassLoader.parent)
        }
        return classloader
    }

    def getClassReader(Class c) {
        return getClassReader(c.getName())
    }

    def getClassReader(String className) {
        InputStream input =
            this.classLoader.getResourceAsStream(className.replace('.', '/') + ".class")
        if (input == null) {
            throw new RuntimeException("Couldn't find class file for " + className)
        }
        
        return new ClassReader(input);
    }

    def getMembers(String className) {
        Class c = this.classLoader.loadClass(className)
        print "("
        getMembersInternal(c, [:] as Set)
        println ")"
    }
    
    def getMembersInternal(Class c, Set seenMethods) {
        ClassReader cr = getClassReader(c.name);
        ClassInfo cir = new ClassInfo();
        cr.accept(cir, false);

        def methodPrinter = {
            def desc = it.name + Type.getMethodDescriptor(it)
            if (seenMethods.contains(desc))
                return

            seenMethods << desc
            
            def methodParameters = cir.methodParameters[desc];

            print "(method"
            print " :name \"" + it.name + "\""
            print " :modifiers (" +  Modifier.toString(it.modifiers) + ")"
            print " :returntype \"" + typeString(it.genericReturnType) + "\""
            print " :arguments ("
            it.genericParameterTypes.eachWithIndex{ pt, i ->
                print "(:type \"" + typeString(pt) + "\""
                if (methodParameters  && methodParameters[++i]) {
                    print " :name \"" + methodParameters[i].name + "\""
                }
                print ") "
            }
            print ")"
            if (it.genericExceptionTypes.length > 0) {
                print " :throws (";
                it.genericExceptionTypes.eachWithIndex{ ex, i ->
                    print "\"" + typeString(ex) + "\""
                }
                print ") "
            }
            print " :declaring-class \"" + it.declaringClass.name + "\""
            println ")"
        }

        def constructorPrinter = {
            def desc = "<init>" + methodDescriptor(it)
            if (seenMethods.contains(desc))
                return

            seenMethods << desc
            def methodParameters = cir.methodParameters[desc];
            
            print "(constructor"
            print " :modifiers (" + Modifier.toString(it.getModifiers()) + ")"
            print " :arguments"
            print " ("
            it.genericParameterTypes.eachWithIndex{ pt, i ->
                print "(:type \"" + typeString(pt) + "\""
                if (methodParameters && methodParameters[++i]) {
                    print " :name \"" + methodParameters[i].name + "\""
                }
                print ") "
            }
            print ") " 
            if (it.genericExceptionTypes.length > 0) {
                print " :throws (";
                it.genericExceptionTypes.eachWithIndex{ ex, i ->
                    print "\"" + typeString(ex) + "\""
                }
                print ") "
            }
            print " :declaring-class \"" + it.declaringClass.name + "\""
            println ")"
        }

        def fieldPrinter = {
            print "(field :name \"" + it.name + "\""
            print " :type \"" + typeString(it.genericType) + "\""
            print " :modifiers (" + Modifier.toString(it.getModifiers()) + ")"
            print " :declaring-class \"" + it.declaringClass.name + "\""
            println ")"
        }
        
        c.declaredFields.each{
            if (!Modifier.isPrivate(it.modifiers))
                fieldPrinter(it)
        }
        
        c.declaredConstructors.each{
            if (!Modifier.isPrivate(it.modifiers))
                constructorPrinter(it)
        }
        c.declaredMethods.each{
            if (!Modifier.isPrivate(it.modifiers))
                methodPrinter(it)
        }
        if (c.isInterface()) {
            c.interfaces.each{
                getMembersInternal(it, seenMethods)
            }
        }
        if (c.superclass != null) {
            getMembersInternal(c.superclass, seenMethods)
        }
    }

    def typeString(type) {
        if (type instanceof Class) {
            if (type.enclosingClass) {
                return typeString(type.enclosingClass) + "." + type.simpleName
            }
            return type.simpleName
        }
        if (type instanceof GenericArrayType) {
            return typeString(type.genericComponentType) + "[]"
        }
        if (type instanceof ParameterizedType) {
            def str = typeString(type.rawType)
            str += "<";
            type.actualTypeArguments.eachWithIndex{ it, i ->
                if (i > 0) {
                    str += ", "
                }
                str += typeString(it)
            }
            str += ">"
            return str;
        }
        
        return type.toString()
    }

    def methodDescriptor(Constructor cons) {
        def str = "(";
        cons.parameterTypes.eachWithIndex{ it, i ->
            if (!it.isArray() && !it.isPrimitive()) {
                str += "L";
            }
            if (!it.isPrimitive()) {
                str += Type.getInternalName(it)
            } else {
                if (it == Boolean.TYPE) {
                    str += "Z"
                } else if (it == Long.TYPE) {
                    str += "J"
                } else {
                    str += it.name.toUpperCase().charAt(0)
                }
            }
            if (!it.isArray() && !it.isPrimitive()) {
                str += ";"
            }
        }
        str += ")V"
        return str
    }
}
