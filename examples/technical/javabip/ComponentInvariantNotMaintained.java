package org.javabip.spec.deviation;

import org.javabip.annotations.*;
import org.javabip.api.PortType;

import java.time.LocalDateTime;
import java.util.List;

@Port(name = GO)
@ComponentType(initial = INIT, name = NAME)
@Invariant(expr = "x >= 0")
public class ComponentInvariantNotMaintained {
    public static final String INIT = "initialState";
    public static final String DONE = "doneState";
    public static final String NAME = "oneComponentOneTransition";
    public static final String GO = "go";

    OneComponentOneTransition() {
        x = 0;
    }

    private int x;

    @Transition(name = GO, source = INIT, target = DONE, pre = "x >= 0", post = "x < 3")
    public void goTransition() {
        x = -1;
    }
}

