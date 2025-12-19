# Revision history for pandoc-md2redmine

## 0.1.0.1 -- 2025-12-19

### Fixed

- Fixed code blocks in lists adding extra blank lines that broke list continuity
  - Code blocks inside list items no longer add trailing newlines
  - Code blocks outside lists maintain proper spacing with following elements
