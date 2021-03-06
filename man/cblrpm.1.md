% CBLRPM(1)
% This manpage was written by Jens Petersen
% 2013-01-21

# NAME
cblrpm - a RPM packaging tool for Haskell Cabal packages

# SYNOPSIS
cblrpm [*options*] spec [*path-or-pkg*]
cblrpm [*options*] local [*path-or-pkg*]
cblrpm [*options*] srpm [*path-or-pkg*]
cblrpm [*options*] prep [*path-or-pkg*]
cblrpm [*options*] builddep [*path-or-pkg*]
cblrpm [*options*] install [*path-or-pkg*]
cblrpm [*options*] depends [*path-or-pkg*]
cblrpm [*options*] requires [*path-or-pkg*]
cblrpm [*options*] missingdeps [*path-or-pkg*]
cblrpm [*options*] diff [*path-or-pkg*]

# DESCRIPTION
cblrpm generates RPM packages and .spec files from Haskell Cabal package.

If no *path-or-pkg* is specified, cblrpm looks for a .cabal or .spec file
in the current directory.  Otherwise, it will look for *path-or-pkg*. If
the argument is a directory then it will look there for a .cabal or .spec file.
If the argument is a path to a .cabal, .spec, or .tar.gz file then it will
use it. Otherwise if there is no '/' in the argument and it does not exist
then cblrpm will try to unpack the package and use its .cabal file.
cblrpm uses a temporary directory for unpackaging tarballs or packages.

cblrpm then parses the above specified .cabal file and
uses it to generate a .spec file that can be built.

If a <PKG>.spec already exists, cblrpm output to <PKG>.spec.cblrpm instead.

# OPTIONS
-h, --help
:   Show the help text.

-l, --library
:   Force a BinLib package with executable(s) to be considered a Lib package.

-f *FLAGS*, --flags=*FLAGS*
: Override one or more Cabal build configuration flags.

--release=*RELEASE*
: Override the release number in the .spec file.

-v *N*, --verbose=*N*
: Set verbosity to *N*.

--version=*VERSION*
: Override the version number in the .spec file.

# EXAMPLES
Below CMD can be one of: spec, srpm, prep, local, install, and depends.

Do CMD for the package in current directory:

    cblrpm CMD

Do CMD for package (directory or package name):
 
    cblrpm CMD [package]

Do CMD for package-version (directory or package name):

    cblrpm CMD [package-version]

Do CMD on a .cabal file:

    cblrpm CMD path/to/some.cabal

Do CMD for a tarball:

    cblrpm CMD path/to/pkg-ver.tar.gz

# HISTORY
Cabal-rpm was originally written by Bryan O'Sullivan in 2007-2008
and resurrected by Jens Petersen in 2012 to replace cabal2spec.

# SEE ALSO
<http://github.com/juhp/cabal-rpm/>
