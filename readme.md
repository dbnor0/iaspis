
## Iaspis

EVM compatible programming language with support for first-class upgradeable contracts

### Reasoning

Ethereum contracts are immutable by default. This means that once confirmed by the network, the code associated with a given contract's address will stay unchanged. Transparency is one of the forefront selling points of blockchains - however, this seemingly straight-forward aspect has also proved to be a (sometimes costly) painpoint in development. 

Certain web3 ecosystems have adapted to this constraint through something called _proxy contracts_. Leveraging the ability of certain languages to delegate execution to other contracts while still acting upon the storage of the original essentially allows for upgradeable interfaces (functionality) to immutable storage.

One of the most prolific examples of this is OpenZeppelin's implementation of [Upgradeable Proxies](https://docs.openzeppelin.com/upgrades-plugins/1.x/proxies) within the Ethereum space.

This concept can be taken a step further - staying in the realm of EVM-based languages, we can extend the idea to one proxy serving multiple implementation contracts, thus circumventing the code size limitations that most blockchains have by splitting endpoints into multiple _facets_. The terms _diamonds/facets_ have been coined to describe the relationship between these types of contracts, formalized by the [EIP-2535 Standard](https://eips.ethereum.org/EIPS/eip-2535). 

This begs the question - why the need for anything else when we already have these features implemented through well tested libraries?

Although the concepts are readily available, they still expose the end user to a fair share of intricate implementation details - function call forwarding is done through low level instructions, function call delegation involved bitwise manipulation of selectors, and the process of upgrading must be done in a backwards compatible manner with respect to the existing storage layout.  

Iaspis aims to ease development using upgradeable contracts by abstracting over the scaffolding provided by proxy libraries & restricting potentially vulnerable design decisions as much as possible, while still offering most of the features & syntax Solidity developers are used to.

### Language features

In addition to the usual contracts & interfaces Ethereum developers are used to, Iaspis provides 2 additional contract types:

```
open proxy Base for Facet1, Facet2 {
  @      * public  mut bytes32 b;
  @ Facet1 private mut string s; 
  @ Facet2 public  mut uint256[] a;
  @ Facet2 private mut mapping (uint256 => bytes32) m;
}
```

```
facet Facet1 to Base {
  public fn f1 () -> bytes32 {
    b <- keccak256(s);
    return b;
  }
}
```

```
facet Facet2 to Base {
  private fn f2 (uint256 i) {
    uint256 x := a[0]++;
    m[i] <- b;
  }
}
```

In this example, `Base` is a proxy for facets `Facet1` and `Facet2` declaring 4 fields. `b` is usable across all facets, `s` and `a` are restricted to `Facet1`, while `m` is only usable in `Facet2`.

Access modifiers, like in Solidity, specify whether a getter function will be generated for a given field or not.

Facets also specify the proxy they're interfacing. Each facet comes with a function that accesses the fields it has access to. All functions, except private and internal ones will be callable through the proxy. 

Storage assignment is denoted by the `<-` token, while memory assignment uses `:=`.