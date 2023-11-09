#pragma once

#ifndef AE_H
#define AE_H

#include <iostream>
#include <string>
#include <cstring>

using namespace std;
#define NUM 1
#define ADD 2

class AST
{

public:
    int type;
    string num = "0";
    AST *lhs;
    AST *rhs;

    void createNum(string);
    void createAdd(AST *, AST *);
    AST getLhs();
    AST getRhs();

    string getASTCode();
};

void AST::createNum(string stringNum)
{
    num = stringNum;
}

void AST::createAdd(AST *astlhs, AST *astrhs)
{
    lhs = astlhs;
    rhs = astrhs;
}

AST AST::getLhs()
{
    return *lhs;
}

AST AST::getRhs()
{
    return *rhs;
}

string AST::getASTCode()
{
    string astCode = "";
    string numCode = "(num ";
    string addCode = "(add ";

    if (type == NUM)
    {
        astCode += numCode;
        astCode += num;
        astCode += ")";
        return astCode;
    }

    if (type == ADD)
    {
        astCode += addCode;
        astCode += lhs->getASTCode();
        astCode += " ";
        astCode += rhs->getASTCode();
        astCode += ")";
        return astCode;
    }

    return astCode;
}

#endif