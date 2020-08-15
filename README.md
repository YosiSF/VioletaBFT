# VioletaBFT

VioletaBFT (Spanish: Colour, Name (F); popular figures: First Female Presidentof Nicaragua and Central America.) is a relativistic pbft protocol which represents A mission critical, scalable, OLTP Query planning. In designing VioletaBFT, we applied similar techniques and decomposition in order to preserve the simplicity and understandability of Raft, Paxos, Tendermint, Casper, etc. but we use several modifications and additions that provide Byzantine fault tolerance.

1) Message signatures: VioletaBFT uses digital signatures extensively to authenticate messages and verify their in- tegrity. For example, the leader replicates client messages along with the client signatures. This prevents a Byzantine leader from modifying the message contents or forging messages. Client public keys are kept separate from replica public keys to enforce that only clients can send new valid commands, and only replicas can send valid Raft RPCs.

2) Client intervention: VioletaBFT allows clients to inter- rupt the current leadership if it fails to make progress. This allows VioletaBFT to prevent Byzantine leaders from starving the system.

3) Incremental hashing: Each replica in VioletaBFT Raft computes a cryptographic hash every time it appends a new entry to its log. The hash is computed over the previous hash and the newly appended log entry. A node can sign its last hash to prove that it has replicated the entirety of a log, and other servers can verify this quickly using the signature and the hash. VioletaBFT supports partial rollbacks, flexible buffer management, purely asynchronous leader-election and low latency Query/Update


In an asynchronous network, messages are eventually delivered but no other timing assumption is made. Unlike existing weakly synchronous protocols where parameter tuning can be finicky, VioletaBFT does not care. Regardless of how network conditions fluctuate, VioletaBFT’s throughput always closely tracks the network’s available bandwidth. Imprecisely speaking, VioletaBFT eventually makes progress as long as messages eventually get delivered; moreover, it makes progress as soon as messages are delivered. 

Merkle trees are a particular kind of binary trees of size h. Each nodes’ value lies in the set {0, 1}^n and depends on its childs values b and c: a = H(b ∥ c)where H is a hash function:

https://cdn-images-1.medium.com/max/720/0*uHv9jd3c9IVQ7XBJ


This construct is used to sign a vast amount of messages from Winternitz One-Time signatures described in the previous section. One can build a tree of height h whose leaves represent a WOTS public key each. The public key of the tree is its root value.

Signing a message with the i-th index in the tree requires signing with the $i-th instance of WOTS and broadcasting the signature together with its authentication path in the tree. This path allows checking the WOTS public key that was used. If we trace back the path between the i-th leave and the root of the tree, the authentication path is a graph made from all the neighbours of this path. Those neighbours allow going back up to the root and compare its value to the broadcasted public key.

the optimal selection of a stateful or stateless scheme for embedded systems primarily depends on the time-memory trade-off. For instance, stateful schemes exploit memory to store state infor- mation and have better run-time, hence, are well-tailored for performance-oriented systems while stateless schemes exploit processing power and have better memory utilization, hence, are well-suited for memory-constrained systems. It can be concluded that the stateful versions of HBS schemes offer better performance than the stateless versions, but require careful implementation to thwart an attacker to exploit the vulnerabilities related to state management.