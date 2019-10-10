# user-explain

Clojure library for converting Spec error messages (specifically Spec's 
explain-data) into error messages for **users**.

The ideas behind this library are still being worked on so the API may change. This library is used in
production systems.

## Rationale

Clojure Spec provides a printer for Spec explain-data that is super general. Because of this generality, the error 
messages can be difficult to read. [Expound](https://github.com/bhb/expound) does a great job at making error 
messages more friendly for developers. Unfortunately our users may not be technical. Even if our users were
technical, most technical users will not be familiar with the format the Spec or Expound printer will output. We need
a new printer that formats error messages for end users -- technical or otherwise. 

## Usage 

TODO: write this

See test namespace for now

## Related Work

- [phrase](https://github.com/alexanderkiel/phrase)
- [Expound](https://github.com/bhb/expound)

## License 

Copyright © 2019 Provisdom Corp.

Distributed under the MIT License.