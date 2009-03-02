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
import static org.junit.Assert.*;
import static org.junit.matchers.JUnitMatchers.*;
import static org.hamcrest.CoreMatchers.*;

import org.codehaus.groovy.tools.shell.util.ANSI;

class GroovyServerTest
{
    int compilerPort = 5555;
    int evalPort = 6666;

    def ready = new CountDownLatch(1);
    def done = new CountDownLatch(1);
    
    def startGroovyServer = {
        GroovyServer.startServer(compilerPort, ready);
        done.await();
    } as Runnable;
        
    @Test(timeout=2000L)
    void socketServer() {
        ANSI.enabled = false;

        def thread = new Thread(startGroovyServer);
        thread.start();
        ready.await();
        Socket s = new Socket();
        try {
            s.connect(new InetSocketAddress(InetAddress.getByName(null), compilerPort),
                      1000);
        } catch (SocketTimeoutException e) {
            fail("GroovyServer didn't answer: " + e.getMessage());
        } catch (IOException e) {
            fail("GroovyServer errored: " + e.getMessage());
        } finally {
            s.close();
            done.countDown();
        }
    }
}
