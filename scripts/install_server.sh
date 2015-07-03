#!/bin/bash
set +x

# Installation script for ubuntu15.04 remote host.


function InstallAll() {
  InstallBasicPackages

  InstallShadowsocks
  InstallOwncloud

  # Optional based on cloudprovider.
  ChangeHostname "vultr.guest" "pitaya"
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

  sudo cat <<EOF > /etc/apache2/sites-available/owncloud.conf
<VirtualHost *:80>
  ServerName owncloud.deyuan.me
  # Redirect all requests to https. Note the root "/" is relative to domain
  # 'owncloud.deyuan.me'. E.g. if we change "/" to "abc", we'll be redirected
  # to https only if we request 'http://owncloud.deyuan.me/abc'
  Redirect "/" "https://owncloud.deyuan.me"
</VirtualHost>

<VirtualHost *:443>
  ServerName owncloud.deyuan.me
  ServerAdmin webmaster@localhost
  DocumentRoot /var/www/owncloud

  ErrorLog ${APACHE_LOG_DIR}/error.log
  CustomLog ${APACHE_LOG_DIR}/access.log combined

  SSLEngine on

  SSLCertificateFile  /etc/ssl/certs/ssl-cert-snakeoil.pem
  SSLCertificateKeyFile /etc/ssl/private/ssl-cert-snakeoil.key

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
