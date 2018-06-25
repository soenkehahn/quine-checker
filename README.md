# `quine-checker`

This is a program that checks whether executables are quines or not.

## installing

1. Install [stack](https://haskellstack.org).
2. Put `~/.local/bin` in your `$PATH`.
2. `git clone https://github.com/soenkehahn/quine-checker.git`
3. `cd quine-checker`
4. `stack install`

## running

If you have a quine in a directory `./foo` in the file `quine`, you can
check it by running `quine-checker foo`. You can pass multiple
directories to `quine-checker`.

## docker base image

This repo also provides a docker image that can be used to run the
`quine-checker`. It aims to support a range of languages for running and
checking quines, independently of the host platform. However, the
`quine-checker` can be used without that image.

You can build and test the base image by running

```
$ ./base-image/build-base-image.hs
```

from the root of the repo.

Adding support for another language requires some familiarity with Dockerfiles.
Here's what you need to do:

1. Add a test file in `./base-image/tests`. This should be a simple program
  written in the language you want to add. It should just write the exact string
  `Hello, World!` to stdout. See `./base-image/tests/python.py` for an example.
2. Modify `./base-image/Dockerfile` to make that language available in the
  image.
3. Run `./base-image/build-base-image.hs` to make sure it works.
