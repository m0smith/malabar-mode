package test;

import java.util.Collection;

public interface InterfaceWithMethods<E>
{
    void mu();
    boolean hasBuddhaNature(Object dog);
    void meditate(long millis, E koan);
    boolean meditateAll(Collection<? extends E> koans);
    <T> T[] joshu(T[] dogs);
}
