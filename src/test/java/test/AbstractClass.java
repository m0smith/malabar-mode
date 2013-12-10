package test;

public abstract class AbstractClass<E>
    extends AbstractParent<E>
    implements SubInterface<E>
{
    public boolean isEmpty() {
        return false;
    }
}
