# Quiet HN - Elm

## APIs used

- https://pages.github.com/
- https://github.com/HackerNews/API

## MVP

1. Load the top 30 story IDs
2. Load each story
3. Render them as they arruve

## Better Architecture

1. Load the stories from local storage.
2. If the time updated was more than an hour ago, load the links again and save them.
3. Otherwise, use the links from local storage.
