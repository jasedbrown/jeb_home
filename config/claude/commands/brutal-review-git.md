---
description: Perform a ruthless, brutal, in-depth, extremely critical code review of commits on the current branch
argument-hint: [base_branch]
---

# Brutal Code Review (Git)

Perform a ruthless, brutal, in-depth, extremely critical code review of all commits on the current branch since it diverged from the base branch. Designed for stacked diff workflows with Gerrit.

## Steps

1. **Retrieve the changes**:
   - Determine base branch: use `$ARGUMENTS` if provided, otherwise default to `main`
   - Use `git log <base>..HEAD --oneline` to list all commits in the stack
   - For each commit, use `git show <commit_hash>` to examine its specific changes
   - Note the chronological order for reviewing oldest to newest

2. **Review each commit individually** in chronological order (oldest first):
   - For each commit, understand what it's trying to accomplish
   - Assess whether the commit is appropriately scoped and focused
   - Examine every aspect of that commit's changes:
     - Logic and algorithm correctness
     - Code style and readability
     - Edge cases and error handling
     - Potential bugs
     - Architecture and design decisions
     - Test coverage and quality
     - Documentation completeness
     - Compliance with best practices

3. **Correlate findings and answer questions** by examining the wider codebase and context:
   - For each commit, identify dependencies and interactions with other parts of the codebase
   - Understand the broader impact of each change
   - Address questions about dependencies and interactions
   - Consider how commits in the stack relate to and build upon each other
   - Identify any cross-commit concerns or inconsistencies

4. **Deliver the review organized by commit**:
   - Present findings for each commit separately, identified by hash and message
   - Within each commit section:
     - Cite specific line numbers and code snippets
     - Provide actionable feedback for each issue
     - Ask concrete questions about unclear decisions
     - Categorize issues by severity (critical, major, minor, nit)
   - After individual commit reviews, add a summary section for:
     - Cross-commit concerns or patterns
     - Overall assessment of the commit stack structure
     - Any broader architectural observations

Be ruthless in identifying problems and critical in assessing solutions. Do not soften feedback or add unnecessary praise.
