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
        methodParameters[currentMethod] = [
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

    def getMethodSignatures(String className, String methodName) {
        Class c = this.classLoader.loadClass(className)

        ClassReader cr = getClassReader(className);
        ClassInfo cir = new ClassInfo();
        cr.accept(cir, false);

        def methodPrinter = {
            def methodParameters = cir.methodParameters[methodName + Type.getMethodDescriptor(it)];
            
            print Modifier.toString(it.getModifiers())
            print " "
            print typeString(it.genericReturnType)
            print " ${methodName}("
            it.genericParameterTypes.eachWithIndex{ pt, i ->
                if (i > 0) {
                    print ", "
                }
                print typeString(pt)
                print " "
                if (methodParameters && methodParameters[i]) {
                    print methodParameters[i].name
                } else {
                    print "arg${i}"
                }
            }
            print ")"
            // Exceptions
            println ""
        }

        def constructorPrinter = {
            print Modifier.toString(it.getModifiers())
            print " " + it.declaringClass.simpleName
            print "("
            print ")"
            // Exceptions
            println ""
        }

        if (methodName == "<init>") {
            c.constructors.each{
                constructorPrinter(it)
            }
        } else {
            c.methods.each{
                if (it.name == methodName) {
                    methodPrinter(it)
                }
            }
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
}
