### Concept
Protocol and implementation to store and transfer:
- variable lot of files as one
- their meta-data
  - list of content;
  - checksums (hash, signature);
  - dependencies like schemas and prev versions;
- and their log as CUD operations
  - Create with content;
  - Update with new content and dependency or both old and new data;
  - Delete with dependency or old data;
- custom content in same manner: tables; trees; comments.

Header should be relatively compact. Content should be friendly for streaming and batch/chunk transfer.

### Technological targets
#### git integration
Be able to fully read 100+ of top git repositories, including
- git
- scala library
- go lang
- node.js

Be able to prepare commits and transfer packs in git format (two-way integration).

At first, be able to repeat git-like commands:
- add
- commit
- delete
- pull
- push
- rebase

#### scala integration
Be able to parse 10+ popular scala projects - their structure and evolution by-commit:
- classes (including case classes), traits, objects, enums
- fields, methods and their signatures (including type-parameters)
- locations - modules, packages, files
- nested things (identifiable)

#### jvm integration
Be able to parse jar-file same way as any project.


#### Competitor to excel
Minimal requirements and data:
- semi-free cell space and regions (addressable fields);
- inference of cell types, lazy for single cell, but with a way to constrain a region;
- reactive inference of formulas between cells;
- a way to store non-cellular structures (trees, lists, ie enums);
- a region can be a table, as a strict constraint;
- composite regions with common constraints;
- propagation of constraints from outer regions to the cell;
- reference enforcement;
- unique and order enforcement;
- styles, including computable and relative;
- non-2D spaces, projections to 2D and maps?
- multi-indexes for 2+D?