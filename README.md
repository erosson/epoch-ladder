# [Epoch-Rank](https://epoch-rank.erosson.org)

An alternative view of [Last Epoch's ladder](https://lastepoch.com/ladder) highlighting class and skill popularity.

Inspired by [poe.ninja/builds](https://poe.ninja/challengessf/builds), though with no online armory (yet?), this isn't nearly as good.

### Secrets

- Last Epoch's official ladder doesn't show character death counts, but their API has them!
- [?rank=level](https://epoch-rank.erosson.org?rank=level) shows a leaderboard sorted by character level, including characters who've never visited the arena. This isn't visible on [Last Epoch's official ladder](https://lastepoch.com/ladder), but their API supports it! Unsure how EHG feels about making this widely available, so Epoch-Rank keeps it lightly hidden.
- [?enableExp=1](https://epoch-rank.erosson.org?enableExp=1) shows each character's experience. The API's experience value seems wrong - it doesn't match what I see in-game - so it's disabled by default. Feel free to take a look, though.
