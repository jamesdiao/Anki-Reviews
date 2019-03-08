# Modeling Anki Review Load 
Models how much work it takes for a medical student to keep up with a popular flashcard study schedule. 

### tl;dr 
I built an app to model how much work it takes for a medical student to keep up with a popular flashcard study schedule. 

### Anki as a study tool
Just as a quick intro, Anki is a flashcard app, much like Quizlet- you can make your own cards or download shared decks. The core premise of Anki is the spaced repetition system, which reschedules the next time you see a card based on how many times you've successfully answered it in a row. This system requires a fairly high amount of sustained effort to keep up with reviews, and many students aiming to try out Anki are worried about whether they'll be able to keep up with it.

To help people better understand what they're signing up for, or figure out how intense they want to schedule new cards, I've built an app to model how many cards you'll have to review per day. 

### Methodology
Review counts are computed by forward propagating the expected future reviews generated on each day. To avoid multiple embedded loops, the program only allows a maximum of 2 possible forgetten reviews. Because this constraint underestimates the true number of reviews, each added review count is scaled up by the expected difference. This shortcut greatly decreases the computational cost, but results in some shape distortions away from key points (peak and stable state). A more accurate (but slower and less visual) simulator can be accessed here: [https://repl.it/repls/GlassBiodegradableTriggers](https://repl.it/repls/GlassBiodegradableTriggers). 

For validation data, visit [jamesdiao.com/medical_school/projects/anki-reviews](http://jamesdiao.com/medical_school/projects/anki-reviews)


