# Codenames Web App

A real-time multiplayer word-guessing web app built in Scala 3 for EPFL CS-214 (Software Construction), autumn 2025. It is a team project with Aziz Marzougui and Ameur Zaheg.

Players pick a team and a role (spymaster or operative). Spymasters give one-word hints, and operatives flip cards on a shared board until one team finds all its words.

- `apps/shared/`: event/view types and the JSON wire format shared by client and server.
- `apps/jvm/`: server-side game logic (state machine, turns, scoring) and tests.
- `apps/js/`: Scala.js browser UI.
- `mockups/`: UI mockups made before implementation.

Built on the course's `cs214.webapp` framework, which sbt fetches from EPFL GitLab (access may be required). Run with `sbt run`.
