.PHONY: preview render pdf

preview:
	quarto preview

render:
	quarto render

pdf:
	quarto render --to pdf
