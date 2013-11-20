make clean
./configure --enable-mysql --enable-tools
make
sudo make install
#/lib/ejabberd/ebin or /var/lib/ejabberd/ebin/
cp ebin/configure.beam /lib/ejabberd/ebin/
