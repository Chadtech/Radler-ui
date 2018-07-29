# Radler 

This is Radler, software for composing and producing music. The UI is a tracker, which is basically a spreadsheet software where the columns are different musical instruments, the rows are beats, and the cells contain the note and other information like volume or duration.

# Getting Started

```
npm install
gulp
```

# Lessons

The code came out pretty good in the project. But there are a few things I would do differently if I started over again.
1. I use a flex grid technique to position things. That worked out well for the most part, but I could have used `display: grid;` and it would probably have been a lot simpler to position things.
2. The tracker UIs are scrollable, but the top header of each tracker isnt. Theres no visual seam to cue which part is scrollable or not. There isnt a functionality problem, but it does look a big weird when the UI is a little bit scrolled, and some buttons are just cut off.