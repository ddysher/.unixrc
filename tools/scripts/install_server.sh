#!/bin/bash
set +x

# Installation script for ubuntu15.04 remote host.


function InstallAll() {
  InstallBasicPackages
  InstallShadowsocks
  InstallOwncloud
  InstallZnc

  if false; then
    ChangeHostname "vultr.guest" "pitaya" # Only Used for vultr cloudprovider
    CreateUser "deyuan"                   # Optionlly create a user
  fi
}


function InstallBasicPackages() {
  sudo apt-get install -y git curl vim
}


function InstallShadowsocks() {
  sudo apt-get update
  sudo apt-get install -y python-pip curl
  sudo pip install shadowsocks

  local -r public_ip="$(curl icanhazip.com)"
  local -r password=$(dd if=/dev/urandom bs=128 count=1 2>/dev/null | base64 | tr -d "=+/" | dd bs=16 count=1 2>/dev/null)
  sudo cat <<EOF > /etc/shadowsocks.json
{
  "server": "${public_ip}",
  "server_port": 8388,
  "password": "${password}",
  "timeout":300,
  "method":"aes-256-cfb",
  "fast_open": false
}
EOF

  sudo cat <<EOF > /etc/systemd/system/shadowsocks-server.service
[Unit]
Description=Shadowsocks Server
After=network.target

[Service]
PermissionsStartOnly=true
ExecStart=/usr/local/bin/ssserver -c /etc/shadowsocks.json
Restart=on-abort
User=nobody
Group=nogroup
UMask=0027

[Install]
WantedBy=multi-user.target
EOF

  systemctl enable shadowsocks-server.service
  systemctl start shadowsocks-server.service
}


function InstallOwncloud() {
  sudo sh -c "echo 'deb http://download.opensuse.org/repositories/isv:/ownCloud:/community/xUbuntu_15.04/ /' >> /etc/apt/sources.list.d/owncloud.list"
  sudo apt-get update
  DEBIAN_FRONTEND=noninteractive apt-get -q -y install mysql-server
  sudo apt-get install -y --force-yes owncloud

  # Create a config file for apache to serve 'owncloud.deyuan.me'. If the domain
  # is not attached to the machine, then it will have no effect. Owncloud can be
  # accessed from $public_ip/owncloud.
  sudo mkdir /etc/ssl/apache
  sudo cp ~/.unixrc/scripts/ssl/apache/* /etc/ssl/apache
  sudo cat <<EOF > /etc/apache2/sites-available/owncloud.conf
<VirtualHost owncloud.deyuan.me:80>
  ServerName owncloud.deyuan.me
  # Redirect all requests to https. Note the root "/" is relative to domain
  # 'owncloud.deyuan.me'. E.g. if we change "/" to "abc", we'll be redirected
  # to https only if we request 'http://owncloud.deyuan.me/abc'
  Redirect "/" "https://owncloud.deyuan.me"
</VirtualHost>

<VirtualHost owncloud.deyuan.me:443>
  ServerName owncloud.deyuan.me
  ServerAdmin webmaster@localhost
  DocumentRoot /var/www/owncloud

  ErrorLog ${APACHE_LOG_DIR}/error.log
  CustomLog ${APACHE_LOG_DIR}/access.log combined

  SSLEngine on

  SSLCertificateFile /etc/ssl/apache/deyuan.me.crt
  SSLCertificateKeyFile /etc/ssl/apache/deyuan.me.key

  <FilesMatch "\.(cgi|shtml|phtml|php)$">
    SSLOptions +StdEnvVars
  </FilesMatch>
  <Directory /usr/lib/cgi-bin>
    SSLOptions +StdEnvVars
  </Directory>

  BrowserMatch "MSIE [2-6]" \
  nokeepalive ssl-unclean-shutdown \
  downgrade-1.0 force-response-1.0
  # MSIE 7 and newer should be able to use keepalive
  BrowserMatch "MSIE [17-9]" ssl-unclean-shutdown
</VirtualHost>
EOF

  sudo a2ensite owncloud.conf
  sudo a2enmod ssl
  sudo systemctl restart apache2.service
}


# Install Znc for IRC chat (running as daemon). Example configuration file:
# Version = 1.6.0
# <Listener l>
#   Port = 5000
#   IPv4 = true
#   IPv6 = false
#   SSL = true
# </Listener>
# LoadModule = webadmin
#
# <User ddysher>
#   Pass       = sha256#5ff062957ecdab93248024c5e5140a113b9215665de830f51f5e808102f7adb4#W_FZ-2/zxhieih:K.Byy#
#   Admin      = true
#   Nick       = ddysher
#   AltNick    = deyuan
#   Ident      = ddysher
#   RealName   = Deyuan Deng
#   LoadModule = chansaver
#   LoadModule = controlpanel
#
#   <Network freenode>
#     LoadModule = simple_away
#     Server     = irc.freenode.net 6667
#   </Network>
# </User>
#
# Notes:
# [ ** ] To connect to this ZNC you need to connect to it as your IRC server
# [ ** ] using the port that you supplied.  You have to supply your login info
# [ ** ] as the IRC server password like this: user/network:pass or user:pass.
function InstallZnc() {
  sudo apt-get update
  sudo apt-get install -y python-software-properties
  sudo add-apt-repository -y ppa:teward/znc
  sudo apt-get update
  sudo apt-get install -y znc znc-dbg znc-dev znc-perl znc-python znc-tcl
  sudo apt-get install -y libapache2-mod-proxy-html libxml2-dev # For apache proxy

  sudo useradd --create-home -d /var/lib/znc --system --shell /sbin/nologin --comment "Account to run ZNC daemon" --user-group znc
  sudo mkdir /var/lib/znc
  sudo chown znc:znc /var/lib/znc
  # Do not start ZNC when creating conf; otherwise, systemctl will fail.
  sudo -u znc /usr/bin/znc --datadir=/var/lib/znc --makeconf

  # Create a config file for apache to serve 'znc.deyuan.me'. Note we assume
  # znc will listen on port 5000.
  sudo mkdir /etc/ssl/apache
  sudo cp ~/.unixrc/scripts/ssl/apache/* /etc/ssl/apache
  sudo cat <<EOF > /etc/apache2/sites-available/znc.conf
<VirtualHost znc.deyuan.me:80>
  ServerName znc.deyuan.me
  Redirect "/" "https://znc.deyuan.me"
</VirtualHost>

<VirtualHost znc.deyuan.me:443>
  ServerName znc.deyuan.me

  ProxyPreserveHost On
  SSLEngine on

  SSLCertificateFile /etc/ssl/apache/deyuan.me.crt
  SSLCertificateKeyFile /etc/ssl/apache/deyuan.me.key

  ProxyPass / http://localhost:5000/
  ProxyPassReverse / http://localhost:5000/
</VirtualHost>
EOF

  sudo cat <<EOF > /etc/systemd/system/znc.service
[Unit]
Description=ZNC, an advanced IRC bouncer
After=network.target

[Service]
ExecStart=/usr/bin/znc -f --datadir=/var/lib/znc
User=znc

[Install]
WantedBy=multi-user.target
EOF

  sudo systemctl start znc

  sudo a2ensite znc.conf
  sudo a2enmod proxy
  sudo a2enmod proxy_http
  sudo a2enmod proxy_ajp
  sudo a2enmod rewrite
  sudo a2enmod deflate
  sudo a2enmod headers
  sudo a2enmod proxy_balancer
  sudo a2enmod proxy_connect
  sudo a2enmod proxy_html
  sudo a2enmod xml2enc
  sudo systemctl restart apache2.service
}


function ChangeHostname() {
  if grep -Fxq "$1" /etc/hostname
  then
    sudo hostname $2
    sudo sed -i "s/$1/$2/g" /etc/hostname
  fi

  if grep -Fxq "$1" /etc/hosts
  then
    sudo sed -i "s/$1/$2/g" /etc/hosts
  else
    echo "" >> /etc/hosts
    echo "127.0.0.1  $2" >> /etc/hosts
  fi
}


function CreateUser() {
  sudo useradd $1 -m -s /bin/bash
  sudo passwd $1
  sudo echo "$1 ALL=(ALL) ALL" >> /etc/sudoers
}
