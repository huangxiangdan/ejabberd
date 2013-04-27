cd src
make clean
./configure  --enable-odbc
make
sudo make install
cd ..
#/lib/ejabberd/ebin or /var/lib/ejabberd/ebin/
sudo cp ebin/* /lib/ejabberd/ebin/
