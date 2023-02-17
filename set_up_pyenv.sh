
brew install python3
python -m venv ~/.venv/work

# Add to .zprofile/.bashrc
alias work_activate="source ~/.venv/work/bin/activate"
alias vemacs="work_activate; emacs; deactivate"

export PYPI_PROD="https://pypi.apple.com/simple"

# pip3 install venv -i $pypi_prod --user

pip3 install model-reporting -i $PYPI_PROD
pip3 install falconlib -i $PYPI_PROD
