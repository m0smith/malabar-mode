package org.grumblesmurf.malabar;

import org.apache.maven.MavenTransferListener;
import org.apache.maven.cli.CLIReportingUtils;
import org.apache.maven.cli.ConsoleDownloadMonitor;
import org.apache.maven.embedder.Configuration;
import org.apache.maven.embedder.ConfigurationValidationResult;
import org.apache.maven.embedder.DefaultConfiguration;
import org.apache.maven.embedder.MavenEmbedder;
import org.apache.maven.embedder.MavenEmbedderConsoleLogger;
import org.apache.maven.embedder.MavenEmbedderException;
import org.apache.maven.embedder.MavenEmbedderLogger;
import org.apache.maven.errors.CoreErrorReporter;
import org.apache.maven.errors.DefaultCoreErrorReporter;
import org.apache.maven.execution.DefaultMavenExecutionRequest;
import org.apache.maven.execution.MavenExecutionRequest;
import org.apache.maven.execution.MavenExecutionResult;
import org.apache.maven.wagon.events.TransferEvent;

import java.util.Arrays;
import java.util.Properties;

import java.io.File;

public enum MvnServer
{
    INSTANCE;
    
    private Configuration configuration;
    private MavenEmbedder mavenEmbedder;
    private CoreErrorReporter errorReporter;
    private MavenEmbedderLogger logger;
    private MavenTransferListener transferListener;

    private MvnServer() {
        configuration = buildEmbedderConfiguration();
        errorReporter = new DefaultCoreErrorReporter();
        logger = new MavenEmbedderConsoleLogger();
        transferListener = new MvnServerTransferListener();
        
        if (validateConfiguration()) {
            try {
                mavenEmbedder = new MavenEmbedder(configuration);
                mavenEmbedder.setLogger(logger);
            } catch (MavenEmbedderException e) {
                CLIReportingUtils.showError("Unable to start the embedder: ", e, false, errorReporter, logger);
                throw new RuntimeException("Unabled to start the embedder", e);
            }
        }
    }

    public static MavenEmbedder getEmbedder() {
        return INSTANCE.mavenEmbedder;
    }

    public static MavenExecutionRequest newRequest() {
        MavenExecutionRequest req = new DefaultMavenExecutionRequest();
        req.setErrorReporter(INSTANCE.errorReporter);
        req.setTransferListener(INSTANCE.transferListener);
        return req;
    }

    private Configuration buildEmbedderConfiguration() {
        Configuration configuration = new DefaultConfiguration()
            .setErrorReporter(errorReporter)
            .setUserSettingsFile(MavenEmbedder.DEFAULT_USER_SETTINGS_FILE)
            .setMavenEmbedderLogger(logger);
        return configuration;
    }

    private boolean validateConfiguration() {
        ConfigurationValidationResult cvr =
            MavenEmbedder.validateConfiguration(configuration);
        if (!cvr.isValid()) {
            if (cvr.getUserSettingsException() != null) { 
                CLIReportingUtils.showError("Error reading user settings: ",
                                            cvr.getUserSettingsException(),
                                            false,
                                            errorReporter,
                                            logger);
            }
            if (cvr.getGlobalSettingsException() != null) { 
                CLIReportingUtils.showError("Error reading global settings: ",
                                            cvr.getGlobalSettingsException(),
                                            false,
                                            errorReporter,
                                            logger);
            }
            return false;
        }
        return true;
    }
    
    public boolean run(String pomFileName, String... goals) {
        return run(pomFileName, false, goals).run();
    }

    public RunDescriptor run(String pomFileName, boolean recursive, String... goals) {
        RunDescriptor run = new RunDescriptor();
        run.setPom(new File(pomFileName));
        run.setRecursive(recursive);
        run.setGoals(goals);
        return run;
    }

    private static class MvnServerTransferListener
        extends ConsoleDownloadMonitor
    {
        public void transferError(TransferEvent event) {
            System.out.println(event.getException().getMessage());
        }
    }

    public static class RunDescriptor 
    {
        File pom;
        boolean recursive;
        String[] goals;
        Properties properties = new Properties();
        
        public void setPom(File pom) {
            this.pom = pom;
        }
        public void setRecursive(boolean recursive) {
            this.recursive = recursive;
        }
        public void setGoals(String[] goals) {
            this.goals = goals;
        }
        public RunDescriptor addProperty(String key, String value) {
            properties.put(key, value);
            return this;
        }
        public boolean run() {
            MavenExecutionRequest request = new DefaultMavenExecutionRequest()
                .setBaseDirectory(pom.getParentFile())
                .setGoals(Arrays.asList(goals))
                .setTransferListener(INSTANCE.transferListener)
                .setRecursive(recursive)
                .setProperties(properties);
            
            MavenExecutionResult result = INSTANCE.mavenEmbedder.execute(request);
            CLIReportingUtils.logResult(request, result, INSTANCE.logger);
            if (result.hasExceptions())
                System.out.println("1");
            else
                System.out.println("0");
            return !result.hasExceptions();
        }
    }
    
    public static void main(String[] args) {
        INSTANCE.run(args[0], false, "test").addProperty("maven.test.skip", "true").run();
    }
}
