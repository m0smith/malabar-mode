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
package org.grumblesmurf.malabar;

import java.net.*;
import java.util.concurrent.*;

import org.junit.Test;
import org.junit.Before;
import static org.junit.Assert.*;
import static org.junit.matchers.JUnitMatchers.*;
import static org.hamcrest.CoreMatchers.*;

import org.codehaus.groovy.tools.shell.util.ANSI;

class GroovyServerTest
{
    int compilerPort = 5555;

    def compilerReady = new CountDownLatch(1);
    def done = new CountDownLatch(1);
    
    def startGroovyCompiler = {
        GroovyServer.startServer(compilerPort, compilerReady);
        done.await();
    } as Runnable;
        
    @Before
    void disableANSI() {
        ANSI.enabled = false;
    }
    
    @Test(timeout=2000L)
    void singleServer() {
        def thread = new Thread(startGroovyCompiler);
        thread.start();
        compilerReady.await();
        withSocket { s ->
            s.connect(localPort(compilerPort), 1000);
        }
        done.countDown();
    }

    def withSocket (Closure closure) {
        Socket s = new Socket();
        try {
            closure.call(s);
        } catch (SocketTimeoutException e) {
            fail("GroovyServer didn't answer: " + e.getMessage());
        } catch (IOException e) {
            fail("GroovyServer errored: " + e.getMessage());
        } finally {
            s.close();
        }
    }

    def localPort (port) {
        return new InetSocketAddress(InetAddress.getByName(null), port);
    }
}
