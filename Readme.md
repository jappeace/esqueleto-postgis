[![https://jappieklooster.nl](https://img.shields.io/badge/blog-jappieklooster.nl-lightgrey)](https://jappieklooster.nl/tag/haskell.html)
[![Githbu actions build status](https://img.shields.io/github/workflow/status/jappeace/haskell-template-project/Test)](https://github.com/jappeace/haskell-template-project/actions)
[![Jappiejappie](https://img.shields.io/badge/discord-jappiejappie-black?logo=discord)](https://discord.gg/Hp4agqy)
[![Hackage version](https://img.shields.io/hackage/v/esqueleto-postgis.svg?label=Hackage)](https://hackage.haskell.org/package/esqueleto-postgis) 

> Show me the place where space is not.

Implement postgis functionality for esqueleto.
https://postgis.net/

Haskell project template.

Set up cabal within a nix shell.
If you like nix this is a good way of doing haskell development.

similar to: https://github.com/monadfix/nix-cabal
except this has a makefile and ghcid.
We also make aggressive use of [pinning](https://nixos.wiki/wiki/FAQ/Pinning_Nixpkgs)
ensuring project builds for ever (theoretically).

Comes with:
+ [GHCID](https://jappieklooster.nl/ghcid-for-multi-package-projects.html)
+ a nix shell, meaning somewhat platform independence.
  + which is pinned by default
+ A couple of handy make commands.
+ Starting haskell files, assuming we put practically all code in library
+ Working test suite, The detection macro will pickup any file ending with Spec.hs
+ functioining CI (pick your favorite or keep both)
  + for various platforms with cabal
  + a nix flake. 


## Usage

### Modifying for your project
Assuming the name of your new project is `new-project`.

```
git clone git@github.com:jappeace/haskell-template-project.git new-project
cd new-project
```

+ [x] Edit template.cabal,
    + [x] find and replace template with `new-project`
    + [x] Update copyright
    + [x] Update github
+ [x] rename template.cabal to new-project.cabal
+ [x] Edit Changelog.md
  + [x] replace template with `new-project`
  + [x] Also describe your version 1.0.0 release.
+ [ ] Edit flake.nix, replace template with `new-project`.
+ [ ] Edit copyright in LICENSE
+ [ ] For automatic bound bumping: In “Settings” → “Actions” → “General” → “Workflow permissions” tick “Allow GitHub Actions to create and approve pull requests”

#### Reconfigure remotes
```
git remote add template git@github.com:jappeace/haskell-template-project.git
git remote set-url origin git@github.com:YOUR-ORG-OR-USER-NAME/new-project.git
```

We can get template updates like this if we want to by doing `git pull template`.
There will be a large amount of conflicts, but the merge commit should solve them permanently.

#### Readme

+ [ ] Select desired badges. 
  + [ ] Point build badges to right project
+ [ ] Give short project description.
+ [ ] Add new quote suited for the project.
  For example for [fakedata-quickcheck](https://github.com/fakedata-haskell/fakedata-quickcheck#readme)
  I used Kant because
  he dealt with the question "what is truth" a lot.
+ [ ] Truncate this checklist
+ [ ] Truncate motivation for using  this template

### Tools
Enter the nix shell.
```
nix develop
```
You can checkout the makefile to see what's available:
```
cat makefile
```

### Running
```
make run
```

### Fast filewatch which runs tests
```
make ghcid
```