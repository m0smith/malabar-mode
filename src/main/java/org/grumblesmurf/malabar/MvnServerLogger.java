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
