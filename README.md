# passman

Copyright (C) 2018 Jonathan Lamothe
<jlamothe1980@gmail.com>

This program is free software: you can redistribute it and/or modify
it under the terms of the GNU Lesser General Public License as
published by the Free Software Foundation, either version 3 of the
License, or (at your option) any later version.

This program is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
Lesser General Public License for more details.

You should have received a copy of the GNU Lesser General Public
License along with this program.  If not, see
<https://www.gnu.org/licenses/>.

## Executive Summary

This package is a very paranoid password manager.  It consists of two
parts: a front-end command-line interface, and a back-end library
(which can be used to create prettier interfaces).

It is important to note that there is no password recovery service.
If you enter an incorrect master password, you will simply get
incorrect passwords from the database.  This is by design.  It makes
the password database much more resistant to a brute-force attack.
DON'T FORGET YOUR MASTER PASSWORD.

Once installed, the program can be run by typing `passman` in your
terminal.

## Installing

This package uses [Haskell Stack](https://haskellstack.org).  Please
refer to [their
website](https://docs.haskellstack.org/en/stable/README/#how-to-install)
for instructions on installing Haskell Stack.  Once you have done
this, you can simply enter the command `stack install passman` in the
terminal to install passman.

## GitHub

The most recent version of passman can be found on GitHub at
<https://github.com/jlamothe/passman>.

## Pull Requests

Pull requests are welcome, but should be made to the `dev` branch.

## Donations

Bitcoin donations are accepted (but not required) at:
18hqEsXCinyauDp6smPUEVuscjDdasTKvr
