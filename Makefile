LOCAL_FILE=cheatsheet
REMOTE_FILE=probstat
WEB=login.eecs.berkeley.edu:public_html/dl/$(REMOTE_FILE).pdf

www: pdf
	scp $(LOCAL_FILE).pdf $(WEB)

pdf:
	pdflatex $(LOCAL_FILE)

FORCE:
