# Evidence-Based operations - SAWDAP 2014

Slides for a talk given at the first [Sydney Area Workshop on Data Science and Programming
Languages](https://wiki.mq.edu.au/display/plrg/SAWDAP).

Prebuilt version available
[here](https://tesser.org/doc/slides/2014-12-10-sawdap-evidence-based-operations/).

## Abstract

Failure detection and isolation is a critical component of all
fault-tolerant systems. The operational stability of these systems is
reliant on tools and processes which are frequently fundamentally
unchanged from the time of their inception. In addition, these systems
require a significant degree of human oversight. Since the development
of these tools, the problems faced by systems engineers have grown
significantly in scale; so too have the fields of computer science and
statistics made significant advances in data processing and inference.

This informal talk will focus on preliminary research done by Anchor
Engineering on applying statistical techniques to operational metrics.
This will include the design and development of Vaultaire, a
time-series datastore developed in Haskell for consolidation of
systems metrics from thousands of individual hosts, and a discussion
of our initial investigations into failure detection and predictive
modeling of resource consumption.
