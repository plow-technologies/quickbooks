Quickbooks Changelog
----------------------
# Version 1.0.0.0
* Added support for OAuth2 requests to existing QuickBooks API Calls
* Support for OAuth1 remains through the format of the config file (readme)

# Version 0.6.0.0
* Added a QBText type that filters out characters that break the QuickBooks api calls. This is a black list filter and it is not complete. If a character causes the api to act unexpectedly, then the filter function is likely the place to fix it.
* Added Read and Query for bundles
* Item, Category, and Customer responses will now return QuickBooks(type)Response []
* Fixed parsing errors for queries and added versions where necessary
* Removed DocTests
* Added Full Customer Support
* Added Full Item Support
* Switched to using a config file instead of environmental variables

# Version 0.2.0
* Changed Line 'Amount' to a 'Maybe Double'
* Exposed the rest of the modules (Up to user to decide what to use)
* Fixed some of the tests
