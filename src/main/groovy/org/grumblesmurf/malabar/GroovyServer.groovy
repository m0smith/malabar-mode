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

import org.codehaus.groovy.tools.shell.Groovysh;
import org.codehaus.groovy.tools.shell.util.ANSI;
import org.codehaus.groovy.tools.shell.IO;
import org.codehaus.groovy.tools.shell.util.Logger;

import java.net.*;

import java.util.concurrent.CountDownLatch;

class GroovyServer
{
    static ready = new CountDownLatch(2);
    static mvnServer = new MvnServer();

    static void main(String[] args) {
        ExpandoMetaClass.enableGlobally();
        
        Object.metaClass.println = Utils.&println; 
        Object.metaClass.print = Utils.&print;
        
        def cli = new CliBuilder();
        cli.c(longOpt: 'compilerPort', args: 1, required: true, 'compiler port');
        cli.e(longOpt: 'evalPort', args: 1, required: true, 'evaluator port');
        
        ANSI.enabled = false;

        def options = cli.parse(args);

        if (options.c && options.e) {
            def compileServer = startServer(Integer.valueOf(options.getOptionValue('c')),
                                            ready);
            def evalServer = startServer(Integer.valueOf(options.getOptionValue('e')),
                                         ready);
            ready.await();
            startConsole();
            System.exit(0);
        } else {
            System.exit(1);
        }
    }

    static startServer(int port, CountDownLatch latch) {
        def s = new GroovySocketServer(port, latch);
        new Thread(s, "GroovyServer on " + port).start();
        return s;
    }
    
    static startConsole() {
        IO io = new IO();
        Utils.setIO(io);
        Binding binding = new Binding();
        binding['mvnServer'] = mvnServer;
        new Groovysh(binding, io).run();
    }
}

class GroovySocketServer
    implements Runnable 
{
    private final port;
    private final CountDownLatch latch;

    private final ServerSocket serverSocket;

    GroovySocketServer(int port, CountDownLatch latch) {
        this.port = port;
        this.latch = latch;
    }

    public synchronized ServerSocket getSocket() {
        return serverSocket;
    }

    private synchronized void setSocket(ServerSocket socket) {
        serverSocket = socket;
    }

    void run() {
        ServerSocket server = new ServerSocket();
        setSocket(server);

        server.reuseAddress = true;

        server.bind(new InetSocketAddress(InetAddress.getByName(null), port));
        latch.countDown();
        try {
            Socket client = server.accept();
            try {
                IO io = new IO(client.inputStream, client.outputStream, client.outputStream);
                Utils.setIO(io);
                Binding binding = new Binding();
                binding['mvnServer'] = GroovyServer.mvnServer;
                new Groovysh(binding, io).run();
            } finally {
                client.close();
            }
        } catch (SocketException e) {
            // Do nothing
        } finally {
            server.close();
        }
    }
}
