# hoard Readme


## Description

hoard's aim is to be a collections framework for Bigloo. It provides a number of protocols (i.e., collections of generic methods) that define the interfaces required by commmon collection types and multiple implementations of each protocol.

It  currently defines the following protocols:

### Protocols

* collection
* indexable
* extendable
* mutable
* enumerator
* enumerable
* collector
* comparator
* bag
* set
* queue
* stack
* priority-queue
* dictionary
* dictionary-enumerator
* dictionary-enumerable

And it currently provides the following concrete collection implementations and supporting data types:

### Implementations and Supporting Data Types

* association
* range
* sorted-bag
* hash-bag
* sorted-set
* sorted-bag
* stretchy-vector
* linked-queue
* contiguous-queue
* linked-stack
* continguous-stack
* binary-heap
* pairing-heap
* sorted-dictionary

### Building

Both the Bigloo native and jvm backends are supported. To build the native libraries, just execute

     make

To build only the jvm libraries, execute

     make jvm

To build only the native libraries, execute

     make c

### Installation
To Install the libraries, execute

	make install

This by default installs the libraries into the directory /usr/lib/bigloo/<bigloo-version>. If you have Bigloo installed to a different prefix, execute

	make install DESTDIR=/path/prefix

This will result in the libraries being installed to /path/prefix/lib/bigloo/<bigloo-version>.

### Documentation

The current documentation is incomplete but can be found in the [manual](https://github.com/donaldsonjw/hoard/tree/master/manual). The protocols are mostly documented but sustantial work remains for the concrete implementations. There are also number of tests included which can provid examples of using the library (To run the tests, you will need to install [btest](https://github.com/donaldsonjw/btest)).

