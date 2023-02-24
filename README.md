## About

Learning Scala and OCaml by implementing simple games.

## Bulding and Running

Create and run Docker container with Scala and OCaml installed:
```
    docker-compose up -d
```

Enter Docker container
```
    docker-compose exec app bash
```

Build Scala sources
```
    make build
```

Run Chess
```
    make run_chess
```

Run Tic-Tac-Toe in Scala
```
    make run_tictactoe_scala
```

Run Tic-tac-Toe in OCaml
```
    make run_tictactoe_ocaml
```
