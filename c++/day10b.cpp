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

static const std::map<char, int> missingCharScore{
    {')', 1},
    {']', 2},
    {'}', 3},
    {'>', 4},
};

uint64_t scoreLine(const std::string& s) {
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
                return 0;
            stack.pop();
        } 
    }
    uint64_t score = 0;
    while (!stack.empty()) {
        score *= 5;
        score += missingCharScore.at(stack.top());
        stack.pop();
    }
    return score;
}

uint64_t answer(const Input &input) {
    std::vector<uint64_t> scores;
    for (const auto &s : input) {
        auto score = scoreLine(s);
        if (score)
            scores.push_back(score);
    }
    std::sort(scores.begin(), scores.end());
    return scores[scores.size() / 2];
}

int main(void) {
    std::cout << answer(readParseInput("../input/day10.txt")) << '\n';
    return 0;
}
