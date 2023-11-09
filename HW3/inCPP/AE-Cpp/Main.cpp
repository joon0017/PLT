#include <iostream>
#include "AE.h"
#include "Parser.cpp"
using namespace std;

int main(int argc, char *argv[]){
    string pOption, ConcreteCode;
    AST* ast;
    Parser parser;

    if(argc >= 3){
        if(strcmp(argv[1], "-p") == 0){
            pOption = argv[1];
            ConcreteCode = argv[2];
        }
        else    
            return 0;
    }
    else if(argc == 2){
        pOption = "Interprete";
        ConcreteCode = argv[1];    
    }

    if(pOption == "-p"){
        ast = parser.parse(ConcreteCode);
        cout << ast->getASTCode();
        
        return 0;   
    }
    else if(pOption == "Interprete"){
        return 0;
    }
    else
        cout << "Syntax Error." ; 

    return 0;
}