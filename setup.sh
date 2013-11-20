echo "install dependence"
sudo apt-get install git
sudo apt-get install mysql-common mysql-client-5.5 libmysqlclient-dev
sudo apt-get install make libncurses5-dev libssl-dev libexpat-dev

echo "install erlang"
tar xvzf otp_src_R16B02.tar.gz
cd otp_src_R16B02/ && ./configure
make
sudo make install

echo "install ejabberd"
cd ../ejabberd && chmod +x build.sh
./build.sh

echo "configure ejabberd"
cd ..
sudo cp server.pem /etc/ejabberd/
sudo cp ejabberd.yml /etc/ejabberd/
sudo cp ejabberdctl.cfg /etc/ejabberd/

echo "done"


