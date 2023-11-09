#include <iostream>
#include <regex>
#include "AE.h"

class Parser
{
public:
    // Parser();
    vector<string> splitExpressionAsSubExpressions(string exampleCode);
    vector<string> getSubExpressions(string exampleCode);
    AST *parse(string exampleCode);
    bool isNumeric(string str);
};

vector<string> Parser::splitExpressionAsSubExpressions(string exampleCode)
{
    if ((exampleCode.front() == '{' && exampleCode.back() != '}') || (exampleCode.front() != '{' && exampleCode.back() == '}'))
        exit(0);

    if (exampleCode.front() == '{')
        exampleCode = exampleCode.substr(1, exampleCode.length() - 1);

    return getSubExpressions(exampleCode);
}

vector<string> Parser::getSubExpressions(string exampleCode)
{
    vector<string> sexpressions = vector<string>();
    int openingParenthesisCount = 0;
    string strBuffer = "";
    for (int i = 0; i < exampleCode.length(); i++)
    {
        if (i == 0 || (i == 0 && exampleCode.at(i) == '{'))
        {
            strBuffer = strBuffer + exampleCode.at(i);
            continue;
        }
        else if (exampleCode.at(i) == ' ' && openingParenthesisCount == 0)
        {
            // buffer is ready to be a subexpression
            if (strBuffer.size() > 0)
            {
                sexpressions.push_back(strBuffer);
                strBuffer = ""; // Ready to start a new buffer
            }
            continue;
        }
        else
        {
            if (exampleCode.at(i) == '{' && openingParenthesisCount == 0)
            {
                openingParenthesisCount++;
                // Ready to start a new buffer
                strBuffer = "" + exampleCode.at(i);
                continue;
            }
            else if (exampleCode.at(i) == '{')
            {
                openingParenthesisCount++;
                strBuffer = strBuffer + exampleCode.at(i);
                continue;
            }
            else if (exampleCode.at(i) == '}' && openingParenthesisCount > 0)
            {
                openingParenthesisCount--;
                strBuffer = strBuffer + exampleCode.at(i);
                continue;
            }
            else if (exampleCode.at(i) == '}')
            {
                // buffer is ready to be a subexpression
                sexpressions.push_back(strBuffer);
                continue;
            }
        }
        strBuffer = strBuffer + exampleCode.at(i);
    }

    sexpressions.push_back(strBuffer);

    return sexpressions;
}

AST *Parser::parse(string exampleCode)
{
    vector<string> subExpressions = splitExpressionAsSubExpressions(exampleCode);

    if (subExpressions.size() == 1 && isNumeric(subExpressions.front()))
    {
        // AST* num = new Num();
        AST *num = new AST();
        num->type = NUM;
        cout << subExpressions.front();
        num->createNum(subExpressions.front());
        return num;
    }

    if (subExpressions.front() == "+")
    {
        // AST* num = new Num();
        AST *add = new AST();
        add->type = ADD;
        add->createAdd(parse(subExpressions.at(1)), parse(subExpressions.at(2)));
        return add;
    }
}

bool Parser::isNumeric(string str)
{
    regex pattern("-?\\d+(\\.\\d+)?");
    smatch match;
    return regex_match(str, match, pattern);
}