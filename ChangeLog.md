# Changelog for file-shelf
- 0.5.0: 
    - Added expansion of ~ in paths 
    - Added platform specific path escaping with fallback
- 0.6.0
    - No longer parses shelf files unnecarsarily (e.g. for version command)
    - Rewrite of database code and general cleanup. This is a breaking change,
      Databases made with older versions won't work. imports of exports made with older versions will work however


## Unreleased changes
