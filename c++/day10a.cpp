#include <fstream>
#include <iostream>
#include <map>
#include <stack>
#include <string>
#include <vector>

using Input = std::vector<std::string>;

Input readParseInput(std::string fileName) {
    std::ifstream file(fileName);
    Input input;

    do {
        std::string s;
        file >> s;
        input.push_back(s);
    } while (!file.eof());
    file.close();

    return input;
}

static const std::map<char, char> closingChar{
    {'(', ')'},
    {'[', ']'},
    {'{', '}'},
    {'<', '>'},
};

static const std::map<char, int> illegalCharScore{
    {')', 3},
    {']', 57},
    {'}', 1197},
    {'>', 25137},
};

int scoreLine(const std::string& s) {
    std::stack<char> stack;
    int i = -1;
    for (auto c : s) {
        i++;
        switch (c) {
        case '(':
        case '[':
        case '{':
        case '<':
            stack.push(closingChar.at(c));
            break;
        default:
            if (c != stack.top())
                return illegalCharScore.at(c);
            stack.pop();
        } 
    }
    return 0;
}

int answer(const Input &input) {
    int totalScore = 0;
    for (const auto &s : input)
        totalScore += scoreLine(s);
    return totalScore;
}

int main(void) {
    std::cout << answer(readParseInput("../input/day10.txt")) << '\n';
    return 0;
}
