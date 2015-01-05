quickbooks
==========

A QuickBooks API binding for Haskell

Test and Development Environment
--------------------------------

Testing and Development should run against an Intuit QuickBooks API sandbox. You need to signup for a sandbox and export environment variables containing your sandbox credentials. You must set the environment variables to get the tests to pass.

The following environment variable need to be exported:

`INTUIT_COMPANY_ID`
`INTUIT_CONSUMER_KEY` 
`INTUIT_CONSUMER_SECRET`
`INTUIT_TOKEN`
`INTUIT_SECRET`
`INTUIT_HOSTNAME`

You can gather these pieces of information by following the instructions found on QuickBooks web site.

Logging
-------

If you would like to enable logging for a given environment. Please set the following environment variable as follows:

`INTUIT_API_LOGGING_ENABLED=True`

Logging is enabled by default.
Setting the value to anything other than true `True` (case insensitive) will disable logging.



