from setuptools import setup, find_packages

description = ""
long_description = ""
requires = [
    'mypy-lang'
]

setup(
    name='comp3702-a1',

    # Versions should comply with PEP440.  For a discussion on single-sourcing
    # the version across setup.py and the project code, see
    # https://packaging.python.org/en/latest/single_source_version.html
    version='0.1',

    description=description,
    long_description=long_description,
    url='https://github.com/TRManderson/comp3702-a1',
    author='Tom Manderson & Tristan Roberts',
    packages=find_packages(exclude=['report']),
    install_requires=requires,
    entry_points={
        'console_scripts': [
            'a1-3702-Tomstan=a1:main',
        ],
    },
)
