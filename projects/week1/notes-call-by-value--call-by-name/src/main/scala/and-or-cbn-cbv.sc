
// => means call by name (so, fun expands first before evaluating args)

def and(x:Boolean, y: => Boolean): Boolean = if (x) y else false

def or(x:Boolean, y: => Boolean): Boolean = if (!x) y else true

def loop(): Boolean = loop()

and(true, false)
and(true, true)
and(false, loop)

or(false, true)
or(false, false)
or(true, loop)
