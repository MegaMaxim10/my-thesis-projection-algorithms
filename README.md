# my-thesis-projection-algorithms #
In this repository you will find the Haskell codes of projection algorithms proposed in one of the contributions of my PhD thesis.

## A Context ##
To easily automate their business processes and increase their competitiveness, organisations are increasingly interested in workflow technology. Indeed, in its most widespread approach, the latter reduces the automation of a given business process to its formal specification (modelling) using a ***Workflow Process Specification Language*** (WfPSL).

To automate a given administrative process (*these are variable processes of which we know all the cases; that is, the tasks are predictable and the sequence are simple and clearly defined*), we can conceive it as a set of annotated trees called ***target artifacts***, representing possible execution scenarios of the said process. In these artifacts, each node represents a process task and an annotation between two nodes indicates whether the corresponding tasks should be executed in sequence or not. The task execution precedence relationships are therefore given by these different annotations and by the hierarchical structure of the artifact.

From this set of artifacts, we will derive an equivalent grammar called ***Grammatical Model of Workflow*** ***(GMWf)***, considering that each task of the studied process corresponds to a symbol of the GMWf and that, each hierarchical decomposition (a node and its sons) in the different artifacts induces a production. 

By associating this model with a list (called ***set of accreditations***) informing on the permissions of each process actor for each task, we obtain a complete (ready to be executed) specification of the studied process.

Instances of an administrative process described using a GMWf and a set of accreditations, can be executed in a decentralised mode, by a ***P2P-WfMS-View***; it is a ***Workflow Management System (WfMS)*** whose geographically dispersed instances (peers) communicate in Peer to Peer (P2P) mode by exchanging artifacts (artifact-centric). As a prerequisite to do so, each peer is configured using the model (GMWf + set of accreditations) of the process. From the (global) GMWf, each peer completes its configuration by deriving by ***projection***, a local GMWf according to the accreditations of the local actor (***GMWf projection***).

During the proper execution, actors coordinate by exchanging locally updated artifacts, to incrementally build one of the target artifacts: that's why the workflow execution process is said to be artifact-centric. These exchanged artifacts represent at any given moment, the current execution state of the business process; they highlight the already executed tasks, those ready to be executed and their executors.

On a given peer, when an artifact is received, it is projected (***artifact projection***) in accordance with the accreditations of the local actor in order to meet any confidentiality requirements. The local actor potentially manipulates only a partial replica of the global artifact under execution; his actions on it are guided by his local GMWf. At the end of its local execution, his updates to the manipulated partial replica must be integrated into the global artifact being executed. We then merge these two artifacts that are conform to two different models (we say that we realise the ***expansion-pruning*** of the updated partial replica). When this merging is complete, the process execution continues with the updated global artifact under execution.

We proposed in our thesis, stable projection operations (*GMWf projection*, *artifacts projection* and *expansion-pruning*) for decentralised execution of processes using a P2P-WfMS-View. **In this repository you will find the Haskell codes of the proposed projection algorithms**.

## How to use the Code in this Repository ##
All the projection functions I provide are written in one file in this repository: the file `src/ArtefactProjection.hs`. To use them, simply load the said file using `ghci` and then enter a few commands.

The supplied file contains an example (that of my thesis) that can be immediately experimented with.

You can test the projection of artifact 2 of the example according to the editor in chief's view by entering the following command:

    projection viewEC (mapArtifact untagNode art2)

You can test the projection of the global GMWf according to the editor in chief's view by entering :

    localGMWf viewEC

The expansion-pruning algorithm can be tested with:

    expandPrune explGMWf viewAE "Ag" curArt2 parRepUptd3

Several other tests are defined for the proposed functions. The functions are commented in French: you can translate them as you wish. Of course, it is also possible to define your own examples and test them.

Thank you for your interest and I hope you will enjoy the experience with this script.