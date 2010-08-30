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

import java.text.DecimalFormat;
import java.text.DecimalFormatSymbols;

import java.util.Locale;

import org.sonatype.aether.transfer.TransferEvent;
import org.sonatype.aether.transfer.TransferResource;
import org.sonatype.aether.util.listener.AbstractTransferListener;

public class MvnServerTransferListener
    extends AbstractTransferListener
{
    public void transferInitiated(TransferEvent event) {
        String type = eventType(event);        

        Utils.println(String.format("%sing: %s", type, descriptor(event.getResource())));
    }

    private String descriptor(TransferResource resource) {
        return resource.getRepositoryUrl() + resource.getResourceName();
    }

    public void transferSucceeded(TransferEvent event) {
        TransferResource artifact = event.getResource();
        long contentLength = event.getTransferredBytes();
        if (contentLength >= 0) {
            String type = eventType(event);
            String len = contentLength >= 1024 ?
                toKB(contentLength) + " KB" :
                contentLength + " B";

            String throughput = "";
            long duration = System.currentTimeMillis() - artifact.getTransferStartTime();
            if (duration > 0) {
                DecimalFormat format =
                    new DecimalFormat("0.0",
                                      new DecimalFormatSymbols(Locale.ENGLISH));
                double kbPerSec = (contentLength / 1024.0) / (duration / 1000.0);
                throughput = " at " + format.format(kbPerSec) + " KB/sec";
            }

            Utils.println(String.format("%sed: %s (%s%s)",
                                        type, descriptor(artifact),
                                        len, throughput));
        }
    }

    private String eventType(TransferEvent event) {
        return event.getRequestType() == TransferEvent.RequestType.PUT ?
            "Upload" : "Download";        
    }
    
    protected long toKB(long bytes) {
        return ( bytes + 1023 ) / 1024;
    }
}
