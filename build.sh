cd src
make clean
./configure  --enable-odbc
make
sudo make install
cd ..
sudo cp ebin/* /lib/ejabberd/ebin/
