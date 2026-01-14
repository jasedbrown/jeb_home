---
description: Perform a ruthless, brutal, in-depth, extremely critical code review of commits on the current branch
argument-hint: [base_branch]
---

# Brutal Code Review (Git)

Perform a ruthless, brutal, in-depth, extremely critical code review of all commits on the current branch since it diverged from the base branch. Designed for stacked diff workflows with Gerrit.

## Steps

1. **Retrieve the changes**:
   - Determine base branch: use `$ARGUMENTS` if provided, otherwise default to `main`
   - Use `git log <base>..HEAD` to view the commit stack
   - Use `git diff <base>...HEAD` to see all changes in the branch

2. **Conduct a thorough code review** examining every aspect:
   - Logic and algorithm correctness
   - Code style and readability
   - Edge cases and error handling
   - Potential bugs
   - Architecture and design decisions
   - Test coverage and quality
   - Documentation completeness
   - Compliance with best practices

3. **Correlate findings and answer questions** by examining the wider codebase and context:
   - Identify dependencies and interactions with other parts of the codebase
   - Understand the broader impact of changes
   - Address questions about dependencies and interactions
   - Examine the commit stack to understand the logical progression
   - Use `git log -p <base>..HEAD` to see detailed changes per commit

4. **Deliver the review** with extreme detail:
   - Cite specific line numbers and code snippets
   - Provide actionable feedback for each issue
   - Ask concrete questions about unclear decisions
   - Categorize issues by severity (critical, major, minor, nit)
   - Consider the stacked diff structure when reviewing

Be ruthless in identifying problems and critical in assessing solutions. Do not soften feedback or add unnecessary praise.
