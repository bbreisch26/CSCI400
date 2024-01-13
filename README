GitHub Setup
============

Joining the course organization
------------------------------

1. You must complete the GitHub username survey on Canvas so that the
   instructor can invite you to join the course GitHub organization.

2. You must accept the invitation to join the course github
   organization.  You may either click the link sent via email or go
   to the course organization page at https://github.com/mines-csci400

SSH Key Authentication
----------------------

If you have previously added your SSH keys to your GitHub account, you
may skip this step.

1. Check whether you have already created SSH keys by running the
   command below.  The keys are typically named `id_rsa` (private key)
   and `id_rsa.pub` (public key).

       ls ~/.ssh

2. If you don't see any SSH keys, create them.  Do not change the
   default filename, or else git and ssh will not find your keys.

       ssh-keygen

3. Display your public key:

       cat ~/.ssh/id_rsa.pub

4. Copy your public key using the GitHub web interface. Click your
   user icon in the upper-right corner and go to Settings->SSH and GPG
   Keys->New SSH Key.  Paste your public key.

Repository Setup
================

1. Work on the labs with your project group.

2. Clone this repo.

3. Create a new, private repo for your team in the course
   organization.  Name your repo `TYY-team-TEAMNAME`, where,
   - `T` is one character for the term: `f` Fall or `s` for Spring,
   - `YY` is the two digit year, e.g. `19` for 2019,
   - `TEAMNAME` is a name for your team.

4. Give all team members access to the team repo in the course github
   organization.

5. Add all team members' names and CWIDs to the `AUTHORS` file.

6. Change the "origin" remote for your cloned repo to your newly
   created team repo and push.

7. Add a new "upstream" remote for the starter code repo.

8. Double-check that:
   1. You have created the repo in the course organization and not
      under your own github account or under another organization.
   2. Your team's github repo is correctly named.
   3. Only your teammates (and the instructor/TA) can access your team's github repo.


Submission
==========

These submission instructions are intended to promote collaboration
with your teammates, make submitting your code easy, and avoid errors
with incorrect submissions.  Please take care to correctly submit your
work by the deadline.

- Push all changes to your team repo created above in the course
  organization before the deadline.

- The TA/instructor will clone the team repos (based on the repository
  name prefix) at the deadline and grade the code in the master
  branch.

- ONLY CODE THAT IS CORRECTLY SUBMITTED BY THE DEADLINE WILL BE
  GRADED:
  - Ensure that your code is pushed to a correctly named repo in the
    course organization.  Incorrectly named repos will not be graded.
  - Ensure that your code is in the master branch.  Code in other
    branches will not be graded.
  - Submit your code by the deadline.  Late work will not be graded.
  - Keep the same directory structure and function names as in the
    stater code.  The grading scripts will test your code based on the
    directory structure and function names in the starter code.  If
    you change either, your code will not pass the tests, and you will
    not receive credit.
  - Code that does not compile will not receive credit.


Updating Starter Code
=====================

For successive projects, you must merge the updated starter code into
your team's repository.  Do not re-clone either the starter code or
your team's repository.  Do not manually copy files between
repositories.  There is an easier way!  Add the starter code
repository as an "upstream" remote as described under setup.  Then,
pull from the "upstream" remote to merge the updated starter code.


Project Instructions
====================

See the `README-lab*.md` files for the instructions for each project.
