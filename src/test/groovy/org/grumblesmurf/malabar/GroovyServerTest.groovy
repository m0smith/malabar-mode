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

import java.net.*;
import java.util.concurrent.*;

import org.junit.Test;
import org.junit.Before;
import org.junit.AfterClass;
import org.junit.After;
import static org.junit.Assert.*;
import static org.junit.matchers.JUnitMatchers.*;
import static org.hamcrest.CoreMatchers.*;

import org.codehaus.groovy.tools.shell.util.ANSI;

class GroovyServerTest
{
    def servers = [];

    static exceptionHandler = { 
    } as Thread.UncaughtExceptionHandler;
    
    @Before
    void disableANSI() {
        ANSI.enabled = false;
    }

    @After
    void shutdownServers() {
        servers.each {
            it.socket.close();
        }
    }

    @AfterClass
    static void setExceptionHandler() {
        Thread[] threads = new Thread[Thread.activeCount() * 2];
        Thread.enumerate(threads);
        threads.each {
            if (it) {
                it.setUncaughtExceptionHandler(exceptionHandler);
            }
        }
    }
    
    @Test(timeout=3000L)
    void singleServer() {
        def ready = new CountDownLatch(1);
        def server = GroovyServer.startServer("single-server",  0, ready);
        servers << server;
        ready.await();
        withSocket { s ->
            s.connect(localPort(server.socket.localPort), 1000);
        }
    }

    @Test(timeout=3000L)
    void dualServer() {
        def serversReady = new CountDownLatch(2);
        servers << GroovyServer.startServer("dual-server-1", 0, serversReady);
        servers << GroovyServer.startServer("dual-server-2", 0, serversReady);
        serversReady.await();

        servers.each { server ->
            withSocket { s ->
                s.connect(localPort(server.socket.localPort), 1000);
            }
        }
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
