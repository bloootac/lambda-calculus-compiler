#!/bin/bash


: '
TODO:

commands to finish:
	- interpret
	- compile
	- run

after the command line tool is done
	- add I/O combinators
	- try out infinite loops
	- try fib w/o Y combinator 
	- check typability of terms in lambda or sk? mark them and only run if nothing else available


'

fileExists() {
	if [ -f "$1" ]; then
		return 0
	else 
		printf "Error: ${1} could not be found.\n"
		return 1
	fi
}

fileGiven() {
	if [ -z "$1" ]; then
		printf "Error: target file not specified.\n"
		return 1
	else
		return 0
	fi
}	

runCommand() {

	if [ "$1" = "help" ]; then 
		printf "help                                        list all commands\n"
		printf "interpret [sourceFile] [targetFile]         store the combinatory representation of [sourceFile] at [targetFile]\n"
		printf "compile [sourceFile] [targetFile]           compile [sourceFile] into an executable program stored at [targetFile]\n"
		printf "run [fileName]                              compile [sourceFile] and run the program\n"
		printf "quit                                        exit command line tool\n"
		
	elif [ "$1" = "interpret" ]; then
		printf "i think you're trying to interpret ${2} into ${3}\n"
		fileExists "$2"
		fileGiven "$3"
		
		
	elif [ "$1" = "compile" ]; then
		printf "i think you're trying to compile ${2} into ${3}\n"
		fileExists "$2"
		fileGiven "$3"
		
	elif [ "$1" = "run" ]; then 
		printf "i think you're trying to run ${2}\n"
		fileExists "$2"
		
		
	else
		printf "Command not recognised. Try running 'help' to see a list of commands.\n"
	fi


	

}

mainLoop() {
	if [ -z "$1" ]; then
		printf "Welcome to the lambda compiler command line tool!\nTry running 'help' to see a list of commands.\n"
		while true; do
			printf ">>> "
			read -r command source target
			
			if [ "$command" = "quit" ]; then
				break
			fi
			
			runCommand "$command" "$source" "$target"
			printf "\n"
		done
	else 
		runCommand "$1" "$2" "$3"
	fi
		
}

mainLoop "$1" "$2" "$3"