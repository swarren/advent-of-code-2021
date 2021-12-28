#include <fstream>
#include <iostream>
#include <map>
#include <stack>
#include <string>
#include <vector>

using Input = std::array<std::array<uint8_t, 10>, 10>;
using PaddedInput = std::array<std::array<uint8_t, 12>, 12>;

Input readParseInput(std::string fileName) {
    std::ifstream file(fileName);
    Input input;

    for (size_t y = 0; y < 10; y++) {
        std::string s;
        file >> s;
        for (size_t x = 0; x < 10; x++) {
            input[y][x] = s[x] - '0';
        }
    }
    file.close();

    return input;
}

uint64_t answer(const Input &inputOrig) {
    PaddedInput input;
    for (size_t y = 1; y <= 10; y++) {
        for (size_t x = 1; x <= 10; x++) {
            input[y][x] = inputOrig[y - 1][x - 1];
        }
    }
    for (size_t y = 0; y <= 11; y++) {
        input[y][0] = 0;
        input[y][11] = 0;
    }
    for (size_t x = 0; x <= 11; x++) {
        input[0][x] = 0;
        input[11][x] = 0;
    }

    uint64_t iter = 0;
    while (true) {
        iter++;

        for (size_t y = 1; y <= 10; y++) {
            for (size_t x = 1; x <= 10; x++) {
                input[y][x]++;
            }
        }

        while (true) {
            uint64_t flashes = 0;

            for (size_t y = 1; y <= 10; y++) {
                for (size_t x = 1; x <= 10; x++) {
                    if (input[y][x] != 255 && input[y][x] > 9) {
                        flashes++;
                        input[y][x] = 255;
                        for (int yofs = -1; yofs <= 1; yofs++) {
                            size_t y2 = y + yofs;
                            for (int xofs = -1; xofs <= 1; xofs++) {
                                size_t x2 = x + xofs;
                                if (input[y2][x2] != 255) {
                                    input[y2][x2]++;
                                }
                            }
                        }
                    }
                }
            }

            if (flashes == 0)
                break;
        }

        uint64_t flashes = 0;
        for (size_t y = 1; y <= 10; y++) {
            for (size_t x = 1; x <= 10; x++) {
                if (input[y][x] == 255) {
                    flashes++;
                    input[y][x] = 0;
                }
            }
        }
        if (flashes == 100) {
            return iter;
        }
    }
}

int main(void) {
    std::cout << answer(readParseInput("../input/day11.txt")) << '\n';
    return 0;
}
