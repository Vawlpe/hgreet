# Hagreety
## A simple example greeter made with hgreet based on agreety / agetty
#### This is only an example, hagreety is not a good greeter, in fact it's almost certainly buggy.
#### It is only meant to show you the basics of how to implement your own greeter in haskell using the [hgreet](https://github.com/Vawlpe/hgreet) package.
### Installation
- [Install greetd](https://sr.ht/~kennylevinsen/greetd/)
- Make sure your `greeter` user has a proper home directory:
  ```shell
  sudo mkdir /home/greeter
  sudo usermod -d /home/greeter greeter
  ```
- **As the** ***greeter*** **user**, [Install GHCup](https://www.haskell.org/ghcup/)
- Still ***as the greeter user***, Clone this repository:
  ```shell
  git clone https://github.com/Vawlpe/hagreety.git
  ```
- And (still as the ***greeter*** user), build and install hagreety:
  ```shell
  cd hagreety
  cabal install
  ```
- Finally, edit and save the `/etc/greetd/config.toml` file as ***root*** to replace the `default_session` section with the following:
  ```toml
  [default_session]
  command = "~/hagreety startx"
  user = "greeter"
  ```
- Reboot and hagreety will be set as your greeter

## License
Hagreety, a simple example greeter made with hgreet and based on agreety / agetty.
Copyright (C) 2022  Hazel (Vawple)

This program is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.
