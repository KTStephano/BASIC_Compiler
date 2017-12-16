# BASIC Compiler
Bytecode compiler for the BASIC programming language (written in Haskell).

The source is split into three modules: Parselib (not written by us), BasicVirtualMachine and BasicCompiler.

Parselib -> Monadic parser we use to parse the source code

BasicVirtualMachine -> Stack-based virtual machine which runs the compiled program

BasicCompiler -> Compiles source code to bytecode to be run by the vm

# Authors
Justin Hall @ https://github.com/KTStephano

Tommy Manzanares @ https://github.com/tmanzanares

# Running the Project

To run the code, you will need to install the Glasgow Haskell Compiler (GHC - https://www.haskell.org/ghc/) or equivalent Haskell compiler.

Next, compile the code with the following command:

    ghc -O2 BasicVirtualMachine.hs -o vm
    
This will compile the virtual machine & compiler to an executable with -O2 level optimizations enabled.

Now you can run BASIC programs by typing:

    ./vm [source_file]
    
For example, "./vm Fib.bas" will compile the code contained in the Fib.bas file and run it in the vm.
