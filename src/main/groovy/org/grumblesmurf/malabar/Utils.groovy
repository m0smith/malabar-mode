package org.grumblesmurf.malabar

class Utils 
{
    static printAsLispList(List list) {
        print "("
        list.each {
            if (it instanceof String) {
                print '"' + it + '"'
            } else if (it instanceof List) {
                printAsLispList(it)
            } else {
                print it
            }
            print " "
        }
        print ")"
    }
}
