package org.grumblesmurf.malabar;

import org.codehaus.plexus.logging.AbstractLogger;
import org.codehaus.plexus.logging.Logger;

public class MvnServerLogger
    extends AbstractLogger
{
    public MvnServerLogger() {
        super(Logger.LEVEL_INFO, MvnServer.class.getName());
    }
    
    private void log(String level, String message, Throwable t) {
        Utils.println(String.format("[%s] %s", level, message));
        if (t != null)
            t.printStackTrace(Utils.getOut());
    }
    
    public void debug(String message, Throwable t) {
        if (isDebugEnabled()) {
            log("DEBUG", message, t);
        }
    }

    public void info(String message, Throwable t) {
        if (isInfoEnabled()) {
            log("INFO", message, t);
        }
    }

    public void warn(String message, Throwable t) {
        if (isWarnEnabled()) {
            log("WARNING", message, t);
        }
    }

    public void error(String message, Throwable t) {
        if (isErrorEnabled()) {
            log("ERROR", message, t);
        }
    }

    public void fatalError(String message, Throwable t) {
        error(message, t);
    }

    public void close() {
    }

    public Logger getChildLogger(String arg0) {
        return this;
    }
}
