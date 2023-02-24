clean:
	cd Chess && $(MAKE) clean
	cd TicTacToe && $(MAKE) clean

build:
	cd Chess && $(MAKE) build
	cd TicTacToe && $(MAKE) build

run_chess:
	cd Chess && $(MAKE) run

run_tictactoe:
	cd TicTacToe && $(MAKE) run

run_tictactoe_ocaml:
	cd TicTacToe && $(MAKE) run
