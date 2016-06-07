# Machine types

The various instructions included in the instruction set are somewhat complicated by
the interplay between the different instruction sets (RV32I/RV32E/RV64I), and priviledge
levels (Base/Machine/User/Hyper/Super).

Nomenclature:

Name    | Machine modes
-----------------------
Machine | M
User    | M + U
Super   | M + S + U
Hyper   | M + H + S + U

# Base level 

* csrrc (limited form)
  * rdcycle
  * rdcycle
  * rdtime
  * rdtimeh
  * rdinstret
  * rdinstreth

* ecall (though not so useful without an environment)

* ebreak (requires a debugging environment)

# Machine + User

* csrrc (full form)
* csrrs
* csrrw
* csrrci 
* csrrsi
* csrrwi
* eret
* wfi

It looks as though a minimal implementation should also include Mbare, Mbb and Mbbid VM
modes.

# Machine + Super + User

* sfence.vm
* mrts [1]

[1] mrts is defined in machine mode (and executed there) but can only be included
when the supervisor mode is enabled.

# Hypervisor

Spec undefined.

* mrth [1]
* hrst [1]

[1] these are defined in machine mode (and executed there) but can only be included
when the hypervisor mode is enabled.

