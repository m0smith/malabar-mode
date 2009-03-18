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

import org.codehaus.plexus.logging.AbstractLogEnabled;
import org.apache.maven.MavenTransferListener;
import org.apache.maven.wagon.events.TransferEvent;

public class MvnServerTransferListener
    extends AbstractLogEnabled
    implements MavenTransferListener
{
    public void transferInitiated(TransferEvent transferEvent) {
    }

    public void transferStarted(TransferEvent transferEvent) {
    }

    public void transferProgress(TransferEvent transferEvent, byte[] buffer, int length) {
    }

    public void transferCompleted(TransferEvent transferEvent) {
    }

    public void transferError(TransferEvent event) {
        Utils.println(event.getException().getMessage());
    }

    public void debug(String message) {
    }

    public boolean isShowChecksumEvents() {
        return false;
    }

    public void setShowChecksumEvents(boolean showChecksumEvents) {
    }
}
