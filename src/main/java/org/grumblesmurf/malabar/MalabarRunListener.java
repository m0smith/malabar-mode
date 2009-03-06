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

import java.io.PrintWriter;

import org.junit.runner.notification.RunListener;
import org.junit.runner.Description;
import org.junit.runner.Result;
import org.junit.runner.notification.Failure;
import java.util.regex.Pattern;
import java.util.regex.Matcher;

public class MalabarRunListener
    extends RunListener
{
    private PrintWriter out;

    private String currentClass = "";

    public MalabarRunListener(PrintWriter out) {
        this.out = out;
    }

    private Pattern displayNamePattern = Pattern.compile("(.*)\\((.*)\\)");
    
    private String getClassName(String description) {
	Matcher matcher= displayNamePattern.matcher(description);
	return matcher.matches() ? matcher.group(2) : "";
    }

    private String getMethodName(String description) {
	Matcher matcher= displayNamePattern.matcher(description);
	return (matcher.matches()) ? matcher.group(1) : "";
    }

    private String simpleName(String classname) {
        return classname.substring(classname.lastIndexOf(".") + 1);
    }

    private void printAndUpdateCurrentClass(Description description) {
        String thisClass = simpleName(getClassName(description.getDisplayName()));
        if (!thisClass.equals(currentClass)) {
            out.println("\n" + thisClass + ":");
        }
        currentClass = thisClass;
    }

    private void printAndUpdateCurrentClass(Failure failure) {
        printAndUpdateCurrentClass(failure.getDescription());
    }

    private void printMethod(Description description) {
        out.print("  " + getMethodName(description.getDisplayName()) + ":  ");
    }
    
    private void printMethod(Failure failure) {
        printMethod(failure.getDescription());
    }
    
    @Override
    public void testRunFinished(Result result) throws Exception {
        out.println(String.format("\nResults:\n\nTests run %s  Failures: %s  Ignored: %s",
                                  result.getRunCount(),
                                  result.getFailureCount(),
                                  result.getIgnoreCount()));
        double seconds = result.getRunTime() / 1000.0;
        out.println(String.format("\nTook %s seconds", seconds));
    }

    @Override
    public void testAssumptionFailure(Failure failure) {
        printAndUpdateCurrentClass(failure);
        printMethod(failure);
        out.println("AssumptionFailure:");
        out.println(failure.getMessage());
    }

    @Override
    public void testFailure(Failure failure) throws Exception {
        printAndUpdateCurrentClass(failure);
        printMethod(failure);
        out.println("FAIL");
        out.println(failure.getMessage());
        String qualifiedClassName = getClassName(failure.getDescription().getDisplayName());
        for (StackTraceElement ste : failure.getException().getStackTrace()) {
            if (ste.getClassName().equals(qualifiedClassName)) {
                out.println("  Failure point:  " +
                            ste.getFileName() + ":" + ste.getLineNumber() + "\n");
                break;
            }
        }
    }

    @Override
    public void testIgnored(Description description) throws Exception {
        printAndUpdateCurrentClass(description);
        printMethod(description);
        out.println("ignored");
    }
}
