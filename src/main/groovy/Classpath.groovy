import org.codehaus.groovy.tools.RootLoader;

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

    def getBytes(Class c) {
        ByteArrayOutputStream baos = new ByteArrayOutputStream()
        
        InputStream input =
            this.classLoader.getResourceAsStream(c.getName().replace('.', '/') + ".class")
        if (input == null) {
            throw new RuntimeException("Couldn't find class file for " + c)
        }
        
        byte[] bytes = new byte[1024];
        int read;
        while ((read = input.read(bytes)) != -1) {
            baos.write(bytes, 0, read)
        }
        input.close()
        return baos.toByteArray()
    }
}
