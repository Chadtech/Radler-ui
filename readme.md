# Radler 

This is Radler, software for composing and producing music. The UI is a tracker, which is basically a spreadsheet software where the columns are different musical instruments, the rows are beats, and the cells contain the note and other information like volume or duration.

This is what it looks like:

![radler](https://i.imgur.com/SOGVQSQ.png)

# Getting Started

```
npm install
gulp
```
then in a separate terminal
```
electron .
```

# UI Organization

For every big important data type `X` in this project, its generally organized in the following way:
```
Ui/X.elm
    - view function
    - update function
    - Msg type
Data/X.elm
    - X type
    - every function meant to modify X
```
Why? Because if I just made `X.elm` for all my data types, and I had a nested project where `B` was nested in `A`, then `B.elm` would not be able to modify the data type `A` since it would not be able to import `A` due to circular dependencies This project does a lot of child-state wanting to update parent-state. (For example, since this is basically just spreadsheet software, the Ui for a row should be able to do things like, add a row below, and the row UI therefore needs access to the state of the whole spread sheet, and not just the state one particular row).

# Engine Organization

For every big important data type `X` in this project, its generally organized as
```
X.hs
    - X data type
    - read function, for parsing an X
    - Error
    - Throw

```

# Ui Lessons

1. The tracker UIs are scrollable, but the top header of each tracker isnt. Theres no visual seam to cue which part is scrollable or not. There isnt a functionality problem, but it does look a big weird when the UI is a little bit scrolled, and some buttons are just cut off.
2. In the course of working on this project, I have become more test driven, so if I were to do this again I would start with the tests first.
3. In the course of working on this project, I went from extreme practice of never using let statements unless it seemed like I would have to repeat many things in the function body, to the practice of putting a function in a let statement if its only used in one place.

# Engine Lessons
Where do I begin? During the course of this project I went from being a total Haskell scrub to being someone who can write a Haskell program. So I would do a lot of things differently.
1. The parsing in this project is really chaotic. Sometimes I use attoparsec, sometimes I use my own `Parse` module, and sometimes I just read a string manually. If I were to do this project again I would standardize from the very start how all data will be structured how I go about parsing it.