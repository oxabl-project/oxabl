# Issue tracker: GitHub

Issues and PRDs for this repository live in GitHub Issues under `oxabl-project/oxabl`. Use the `gh` CLI for issue operations.

## Conventions

- Create an issue with `gh issue create --title "..." --body "..."`.
- Read an issue with `gh issue view <number> --comments` and include its labels.
- List issues with `gh issue list`, selecting the fields needed for the task.
- Comment with `gh issue comment <number> --body "..."`.
- Apply or remove labels with `gh issue edit`.
- Close with `gh issue close <number> --comment "..."`.
- Infer the repository from the current clone and its `origin` remote.

## Small fixes

A small defect that is being fixed immediately does not require a separate issue. Create an issue only when work will remain outstanding, needs independent tracking, or the user asks for one.

## Skill terminology

When a skill says to publish to the issue tracker, create a GitHub issue. When it says to fetch a relevant ticket, read the GitHub issue and its comments.
