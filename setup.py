from setuptools import setup, find_packages


with open('README.md') as file:
    long_description = file.read()
with open('CHANGES.md') as file:
    long_description += file.read()

setup(
    name="specification",
    use_scm_version=True,
    setup_requires=['setuptools_scm'],
    install_requires=[],
    packages=find_packages(),
    include_package_data=True,
    author="Michael Amrhein",
    author_email="michael@adrhinum.de",
    url="https://github.com/mamrhein/specification",
    description="Python implementaion of the `Specification` pattern",
    long_description=long_description,
    license='BSD',
    keywords='specification specification-pattern predicate selection '
             'validation',
    platforms='all',
    classifiers=[
        "Development Status :: 4 - Beta",
        "Intended Audience :: Developers",
        "License :: OSI Approved :: BSD License",
        "Operating System :: OS Independent",
        "Programming Language :: Python",
        "Programming Language :: Python :: 3",
        "Programming Language :: Python :: 3.6",
        "Programming Language :: Python :: Implementation :: CPython",
        "Programming Language :: Python :: Implementation :: PyPy",
        "Topic :: Software Development",
        "Topic :: Software Development :: Libraries",
        "Topic :: Software Development :: Libraries :: Python Modules"
    ]
)
