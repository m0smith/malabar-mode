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

import org.codehaus.groovy.tools.shell.Groovysh;
import org.codehaus.groovy.tools.shell.util.ANSI;
import org.codehaus.groovy.tools.shell.IO;
import org.codehaus.groovy.tools.shell.util.Logger;

import java.net.*;

import java.nio.channels.ServerSocketChannel;
import java.nio.channels.SocketChannel;

class GroovyServer
{
    static void main(String[] args) {
        def cli = new CliBuilder();
        cli.c(longOpt: 'compilerPort', args: 1, required: true, 'compiler port');
        cli.e(longOpt: 'evalPort', args: 1, required: true, 'evaluator port');
        
        ANSI.enabled = false;

        def options = cli.parse(args);

        if (options.c && options.e) {
            //def compileThread = startServer(Integer.valueOf(options.getOptionValue('c')));
            //def evalThread = startServer(Integer.valueOf(options.getOptionValue('e')));
            startConsole();
            //compileThread.interrupt();
            //evalThread.interrupt();
        } else {
            System.exit(1);
        }
    }

    static startServer(int port) {
        def t = new Thread(new GroovySocketServer(port));
        t.start();
        return t;
    }
    
    static startConsole() {
        println "Starting console..."
        new Groovysh(new IO()).run();
    }
}

class GroovySocketServer
    implements Runnable 
{
    private final port;

    GroovySocketServer(int port) {
        this.port = port;
    }
    
    void run() {
        ServerSocketChannel serverChannel = ServerSocketChannel.open();
        ServerSocket server = serverChannel.socket();
        server.bind(new InetSocketAddress(InetAddress.getByName(null), port));
        SocketChannel clientChannel = serverChannel.accept();
        Socket client = clientChannel.socket();
        try {
            new Groovysh(new IO(client.inputStream, client.outputStream, client.outputStream)).run();
        } finally {
            client.close();
            server.close();
        }
    }
}
