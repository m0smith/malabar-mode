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

import java.io.PrintWriter;

import org.junit.runner.notification.RunListener;
import org.junit.runner.Description;
import org.junit.runner.Result;
import org.junit.runner.notification.Failure;
import java.util.regex.Pattern;
import java.util.regex.Matcher;
import java.util.List;
import java.util.LinkedList;

public class MalabarRunListener
    extends RunListener
{
    private PrintWriter out;
    private String currentClass = "";
    private boolean reported;

    private static final Pattern displayNamePattern = Pattern.compile("(.*)\\((.*)\\)");
    
    public MalabarRunListener(PrintWriter out) {
        this.out = out;
    }

    @Override
    public void testStarted(Description description) throws Exception {
        reported = false;
    }

    @Override
    public void testFinished(Description description) throws Exception {
        printAndUpdateCurrentClass(description);
        if (!reported)
            report(".");
    }

    @Override
    public void testIgnored(Description description) throws Exception {
        printAndUpdateCurrentClass(description);
        report("I");
    }

    @Override
    public void testFailure(Failure failure) throws Exception {
        printAndUpdateCurrentClass(failure);
        if (isError(failure))
            report("E");
        else
            report("F");
    }

    @Override
    public void testRunFinished(Result result) throws Exception {
        List<Failure> failures = new LinkedList<Failure>();
        List<Failure> errors = new LinkedList<Failure>();

        for (Failure failure : result.getFailures()) {
            if (isError(failure))
                errors.add(failure);
            else
                failures.add(failure);
        }
        
        out.println(String.format("\nResults:\n\nTests run %s  Failures: %s  Errors: %s  Ignored: %s",
                                  result.getRunCount(),
                                  failures.size(),
                                  errors.size(),
                                  result.getIgnoreCount()));

        if (result.wasSuccessful()) {
            out.println("OK");
        } else {
            out.println("FAILURES!!!");

            reportFailures(failures);
            
            reportErrors(errors);
        }
            
        double seconds = result.getRunTime() / 1000.0;
        out.println(String.format("\nTook %s seconds", seconds));
    }

    private void report(String indicator) {
        out.print(indicator);
        reported = true;
    }

    private void reportFailures(List<Failure> failures) {
        reportFailures("Failed tests:", failures);
    }

    private void reportErrors(List<Failure> errors) {
        reportFailures("Tests in error:", errors);
    }

    private void reportFailures(String header, List<Failure> failures) {
        if (failures.isEmpty())
            return;

        out.println(header);
        out.println();

        for (Failure f : failures) {
            if (isError(f))
                reportError(f);
            else
                reportFailure(f);
        }
    }

    private void reportError(Failure error) {
        printQualifiedMethodName(error.getDescription());

        printSanitizedStackTrace(error.getException());

        out.println();
    }

    private void printSanitizedStackTrace(Throwable t) {
        out.println(t.getClass().getName() + ": " + t.getMessage());
        for (StackTraceElement ste : t.getStackTrace()) {
            if (ste.getClassName().startsWith("sun.reflect")) 
                continue;
            
            out.println("\tat " + ste);

            if (ste.getClassName().startsWith("org.junit")) 
                break;
        }
        
        if (t.getCause() != null) {
            out.print("Caused by: ");
            printSanitizedStackTrace(t.getCause());
        }
    }

    private void reportFailure(Failure failure) {
        printQualifiedMethodName(failure.getDescription());
        out.print(failure.getMessage());

        String qualifiedClassName = getClassName(failure.getDescription().getDisplayName());
        for (StackTraceElement ste : failure.getException().getStackTrace()) {
            if (ste.getClassName().equals(qualifiedClassName)) {
                if (!failure.getMessage().endsWith("\n"))
                    out.println();
                
                out.println("  Failure point:  " +
                            ste.getFileName() + ":" + ste.getLineNumber() + "\n");
                
                break;
            }
        }

        out.println();
    }

    private void printQualifiedMethodName(Description description) {
        String className = simpleName(getClassName(description.getDisplayName()));
        out.print(className);
        out.print(".");
        String methodName = getMethodName(description.getDisplayName());
        out.println(methodName);
    }
    
    private boolean isError(Failure failure) {
        return !(failure.getException() instanceof AssertionError);
    }

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
}
