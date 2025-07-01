## Making Pull Request

Other operating systems here: https://www.freepascal.org/download.html

Windows Git https://git-scm.com/downloads or with https://chocolatey.org/install `choco -y install git`


1. Install Git, FreePascal and nano

### Linux:

Install git and FreePascal compiler.


```
sudo apt-get -y install git fpc

git config --global user.name "YOUR-FIRST-NAME YOUR-LAST-NAME"

git config --global user.email YOUR-EMAIL-ADDRSS@YOUR-EMAIL.COM

git config --global push.default simple

nano .ssh/config
```
There add your User (GitHub username) and IdentityFile (Your ssh private key. Not public key that has .pub).
For indentation, use one tab.
```
Host *
        IdentitiesOnly=yes

Host github.com
        Hostname github.com
        User YOUR-GITHUB-USERNAME
        IdentityFile ~/.ssh/id_YOUR-SSH-PRIVATE-KEY
```
Save and Exit with Ctrl-o Enter Ctrl-x Enter

If you do not have ssh key, create it:
```
ssh-keygen
```
And press Enter about 3 times, until you have private key at `~/.ssh/id_rsa` and public key at `~/.ssh/id_rsa.pub`

Add public key `.pub` to your github account web interface.

### 2. Create fork of `https://github.com/wekan/wami` at GitHub web page

```
mkdir repos

cd repos

git clone git@github.com:YOUR-GITHUB-USERNAME/wami.git

cd wami
```
### 3. Select option 1 to install dependencies, and then Enter.
```
./build.sh
```
And then it's visible at http://localhost:5500/ and http://YOUR-IP-ADDRESS:5500/

### 4. Test

Test does that new plugin syntax work, for example in card title, card description etc on other input fields.

### 5. If it works, create pull request

If normal markdown, emoji, and your new added plugin syntax all work, commit your changes.
```
git add --all

git commit -m "Fixes DESCRIPTIO-ABOUT-WHAT-YOU-FIXED"

git push
```
And then at your GitHub fork `https://github.com/YOUR-GITHUB-USERNAME/wami` click `Create pull request`.
