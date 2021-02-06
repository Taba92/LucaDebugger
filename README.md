# LucaCauDEr

A branch of the causal-consistent debugger for Erlang Cauder

For general information about Cauder see the link: https://github.com/mistupv/cauder

Extension of the debugger:
-Spawn_link
-Error/exit.
-Error/exit signals propagation.
-Unlink.
-Link: the links use signals for creation e errors 'noproc'. But if the process receiver die during the travelling of the signal link, the signal remain in the GS. In the documentation, this signal should be converted in a erro signal with noproc reason.