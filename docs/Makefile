FILE = index

all:
	pandoc -t revealjs -s $(FILE).md -o $(FILE).html \
		--katex \
		--citeproc \
		--highlight-style syntax.theme \
		--slide-level=2

clean:
	rm *.html
