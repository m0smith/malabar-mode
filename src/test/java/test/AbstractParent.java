package test;

import java.util.Iterator;

public abstract class AbstractParent<E>
    implements SimpleGenericInterface<E>
{
    public Iterator<E> iterator() {
        return null;
    }
}
