package org.grumblesmurf.malabar;

class Projects
{
    static def projects = [:];

    static void put(pom, project) {
        projects[pom] = project
    }

    static Project get(pom) {
        return projects[pom]
    }
}
