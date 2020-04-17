Shells are often considered "not real code". Why? Usually because the shell language
over-optimizes for interactive CLI, which makes them not great for writing a program.

What would a language look like that tried to be both at once, so you could script with
confidence?

fish is the closest thing I know of to this. Let's make a new fish.

What it could look like:

```
ls # Expression returning the list of files

let files = ls
open(files[0])

fn doThing(file) {
    
}

for f in files {
    doThing(file)
}

```