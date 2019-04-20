package com.github.unchama.util.failable;

import com.github.unchama.util.ActionStatus;

import java.util.Objects;
import java.util.function.Supplier;

public final class FailableAction<F> {
    final F failValue;
    final Supplier<ActionStatus> action;

    public FailableAction(F failValue, Supplier<ActionStatus> action) {
        this.failValue = failValue;
        this.action = action;
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;
        FailableAction<?> that = (FailableAction<?>) o;
        return Objects.equals(failValue, that.failValue) &&
                Objects.equals(action, that.action);
    }

    @Override
    public int hashCode() {
        return Objects.hash(failValue, action);
    }
}
