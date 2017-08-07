quickbooks
==========

[![Circle CI](https://circleci.com/gh/plow-technologies/quickbooks.svg?style=svg&circle-token=d573550f3617f3cede51f39d9c58125c0644aecc)](https://circleci.com/gh/plow-technologies/quickbooks)

A QuickBooks API binding for Haskell

Test and Development Environment
--------------------------------

Testing and Development should run against an Intuit QuickBooks API sandbox. You need to signup for a sandbox and make a config file with your sandbox credentials. You must have the config file to get the tests to pass.

The following is a sample config file:

quickbooksConfig.yml (for OAuth 2)

companyId           : ""
oauth2ClientId      : ""
oauth2ClientSecret  : ""
oauth2RefreshToken  : ""
consumerToken       : "" Leave empty
consumerSecret      : "" Leave empty
hostname            : "sandbox-quickbooks.api.intuit.com"
loggingEnabled      : "true"

quickbooksConfig.yml (for OAuth 1)

hostname            : "sandbox-quickbooks.api.intuit.com"
consumerToken       : ""
consumerSecret      : ""
oauthToken          : ""
oauthSecret         : ""
loggingEnabled      : "true"

You can gather these pieces of information by following the instructions found on QuickBooks' Developer web site.

Logging
-------

If you would like to enable logging for a given environment. Please set the following config variable as follows:

loggingEnabled : "true"

Setting the value to anything other than true `True` (case insensitive) will disable logging.



