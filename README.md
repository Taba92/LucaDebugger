# LucaCauDEr

A branch of the causal-consistent debugger for Erlang Cauder

For general information about Cauder see the link: https://github.com/mistupv/cauder

Extension of the debugger:
-Spawn_link
-Error/exit.
-Error/exit signals propagation.
-Unlink.
-Link: The links have been implemented not entirely correct as signals would also be used in the case of link setting, instead in Cauder they are implemented in an atomic way 