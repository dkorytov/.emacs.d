git config --global user.email "d_korytov@apple.com"
git config --global user.name  "Dan Korytov"
git config --global push.default "simple"
git config --global core.editor "emacs"
git config --global core.pager "cat"
git config --global alias.ll "!git log --pretty=format:'%C(auto)%h%d (%cr) %s' --graph --decorate -n"
git config --global alias.sl "!git status && git ll 10 && echo \"\""
git config --global alias.ds "!git diff --staged"
git config --global init.defaultBranch main
