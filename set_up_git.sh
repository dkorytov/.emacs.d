git config --global user.email "d_korytov@apple.com"
git config --global user.name  "Dan Korytov"
git config --global push.default "simple"
git config --global core.editor "emacs"
git config --global core.pager "cat"
git config --global color.ui "always"
git config --global alias.ll "!git log --pretty=format:'%C(auto)%h%d (%cr) %s' --graph --decorate -n"
git config --global alias.lll "!git log --pretty=format:'%C(auto)%h%d (%cr) [%an %ae] %s' --graph --decorate -n"
git config --global alias.llll "!git log --pretty=format:'%C(auto)%h%d (%cr) [%an %ae] %s' --graph --decorate -n"
git config --global alias.sl "!git status && git ll 10 && echo \"\""
git config --global alias.ds "!git diff --staged"
git config --global init.defaultBranch main
git config filter.strip-notebook-output.clean 'jupyter nbconvert --ClearOutputPreprocessor.enabled=True --ClearMetadataPreprocessor.enabled=True --to=notebook --stdin --stdout --log-level=ERROR'
