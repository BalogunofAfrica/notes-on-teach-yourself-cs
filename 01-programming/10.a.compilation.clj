"Interpretations is leveling up the machine language up to the application langauge. The apply eval loop is an example of 
 an interpreter. On the other hand, compilations is the lowering down of the application language down to the machine language.
 
 The interpreter is inherentenly pessimistic because it is a general construct which is not aware of the code it is about to run,
 so it tries to be safe by saving up values it think it needs in register like continue, env, unev etc... Whereas the compiler is 
 aware of what it wants to run, and therefore can avoid most of these extra safety steps by only using registers that are actually needed.
 "