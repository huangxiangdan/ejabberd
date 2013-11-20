sudo apt-get install libyaml-dev
make clean
./configure --enable-mysql --enable-tools
make
sudo make install
#/lib/ejabberd/ebin or /var/lib/ejabberd/ebin/
sudo cp ebin/configure.beam /lib/ejabberd/ebin/
