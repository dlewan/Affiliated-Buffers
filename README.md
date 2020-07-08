# Affiliated-Buffers

The idea of affiliated buffers is that you've got a primary buffer
and then it can have other buffers associated with it.
Those buffer can, of course, have affiliated buffers too.
(There can be no loops of affiliations.)

Another aspect of affiliated buffers is
that they may "share [buffer-]local variables.
This is implemented by a wrapper function for (setq)
that is aware of the buffers affiliated with the current one.


