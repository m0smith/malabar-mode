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

import org.apache.maven.repository.ArtifactTransferListener;
import org.apache.maven.repository.ArtifactTransferEvent;

import org.codehaus.plexus.logging.AbstractLogEnabled;

public class MvnServerTransferListener
    extends AbstractLogEnabled
    implements ArtifactTransferListener
{
    public void transferInitiated(ArtifactTransferEvent transferEvent) {
    }

    public void transferStarted(ArtifactTransferEvent transferEvent) {
    }

    public void transferProgress(ArtifactTransferEvent transferEvent, byte[] buffer, int length) {
    }

    public void transferCompleted(ArtifactTransferEvent transferEvent) {
    }

    public void transferError(ArtifactTransferEvent event) {
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
