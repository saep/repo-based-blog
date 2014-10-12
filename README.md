![buid-status](https://travis-ci.org/saep/saeplog.svg?branch=master)

# saeplog

Toy blogging website that I am occasionally working on.

# Get this thing running

On a clean system, that is one without any packages other than ghc and
cabal-install, it should pretty much just work if you follow the following
steps:

> mkdir -p ~/git

> git clone git@github.com:saep/saeplog.git ~/git/saeplog

> mkdir -p ~git/myblog

> cd ~/git/myblog

> git init

Write some blog entries and commit them. If the entries are not commited, 
they are not found due to the way the crawler is implemented.

> cd ~/git/saeplog

> cabal sandbox init

> cabal install --dependencies-only

> cabal run saeplog -- -e ~/git/myblog -r ~/git/saeplog/resources

Open your browser with the following url:
http://127.0.0.1:8000


# Contributing

This is essentially just a reminder for myself as to how I want
contributions to be made. I want to keep the feature-set small for now and
get the prototype running first.

## Guidelines

* Please write documentation!
* Please try to write tests!
* Please annotate commits with [Test],[Style] or [Doc] to indicate their domain,
  leave it empty for commits targeted at the core functionality. This is
  very helpful to spot commits that are interesting.
* If an issue exists, reference it!
* Similar to neovim's development, use [WIP], [RFC] and [RDY] as tags in
  pull requests. Especially the [WIP] annotation has the potential to avoid
  unnecessary duplicate work if the pull request is created immediately
  after starting to work at the issue.

