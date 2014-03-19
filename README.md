LearningFormalLanguages
=======================

A haskell implementation of Angluin's learnig algorithm with the machine as learner, see http://www.model.in.tum.de/um/courses/auto/ws1314/script/autonotes.pdf#section.2.7

The machine tries to build a minimal deterministic automaton for a regular language known to the user by asking if speficic words are in the language, and occassionally aksing if the automaton accepts the correct language.


The empty word is visualized by "1"
