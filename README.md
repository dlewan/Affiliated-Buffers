# Affiliated-Buffers

The idea of affiliated buffers is that you've got a primary buffer
and then it can have other buffers associated with it.
Those buffers can, of course, have affiliated buffers too.
(There can be no loops of affiliations.)

Another aspect of affiliated buffers is
that they may "share [buffer-]local variables".
This is implemented by a wrapper function for (setq)
that is aware of the buffers affiliated with the current one.

There is a small minor mode that includes some basic navigation and editing.
You probably don't want to use it though; it's too rigid for any interesting application.
Write your own application instead.
