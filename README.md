# lazytensor

### Build Status
|                 | Build           | Dev             |
|-----------------|-----------------|-----------------|
| Linux x86_64    | [![Build Status](https://travis-ci.org/cdeterman/lazytensor.png?branch=master)](https://travis-ci.org/cdeterman/lazytensor)      | [![Build Status](https://travis-ci.org/cdeterman/lazytensor.png?branch=develop)](https://travis-ci.org/cdeterman/lazytensor) |
| Windows x86     | [![Appveyor Build Status](https://ci.appveyor.com/api/projects/status/github/cdeterman/lazytensor?branch=master&svg=true)](https://ci.appveyor.com/project/cdeterman/lazytensor)     | [![Appveyor Build Status](https://ci.appveyor.com/api/projects/status/github/cdeterman/lazytensor?branch=develop&svg=true)](https://ci.appveyor.com/project/cdeterman/lazytensor) |

Test coverage: [![Coverage Status](https://coveralls.io/repos/cdeterman/lazytensor/badge.svg)](https://coveralls.io/r/cdeterman/lazytensor?branch=master)

Community Use: [![Downloads](http://cranlogs.r-pkg.org/badges/lazytensor?color=brightgreen)](http://www.r-pkg.org/pkg/lazytensor)

This package is intended to provide a symbolic tensor library similar to the
basic functionality found within python libraries such as Theano and Tensorflow.
The 'Tensor' objects within this package allow the user to chain functions
that will be lazily evaluated.
