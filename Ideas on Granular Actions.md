# A granular actions model

## Persistence
The persistence layer records all granular write-actions and doesn't care about transactions. 

## Server
### Possibilities
* The server granularly communicates with client in transaction, thus suffering from overhead and transaction prolongation.
* On the server side the user declares final transactions (events), the client calls them by tags. The communication is reduced to a single request.

