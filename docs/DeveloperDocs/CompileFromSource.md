## Compile from source

Other operating systems of FreePascal Compiler here https://www.freepascal.org/download.html

### Linux

Install Git and FreePascal, compile and run:

```
sudo apt-get -y install git fpc

git clone https://github.com/wekan/wami

cd wami

fpc wekan.pas

./wekan.pas
```
Then it's visible at http://localhost:5500/ and http://YOUR-IP-ADDRESS:5500/
