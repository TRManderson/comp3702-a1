python := $(shell which python3 || which python)
all:
	$(python) setup.py install

clean:
	find . -type f -name "*.pyc" -delete
