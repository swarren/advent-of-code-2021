#include <fstream>
#include <iostream>
#include <map>
#include <set>
#include <string>
#include <utility>
#include <vector>

using CaveName = std::string;
using Connection = std::pair<CaveName, CaveName>;
using Input = std::vector<Connection>;

Input readParseInput(std::string fileName) {
    std::ifstream file(fileName);
    Input input;

    do {
        std::string s;
        file >> s;
        auto delimPos = s.find('-');
        std::string from = s.substr(0, delimPos);
        std::string to = s.substr(delimPos + 1);
        input.push_back(std::make_pair(from, to));
    } while (!file.eof());
    file.close();

    return input;
}

using Graph = std::map<CaveName, std::set<CaveName>>;

Graph inputToGraph(const Input &input) {
    Graph g;
    for (const auto &connection : input) {
        g[connection.first].insert(connection.second);
        g[connection.second].insert(connection.first);
    }
    return g;
}

using Path = std::vector<CaveName>;
using Paths = std::vector<Path>;
using SeenCaves = std::set<CaveName>;

void findPaths(
    const Graph &g,
    const CaveName &location,
    Paths &paths,
    const Path &path,
    const SeenCaves &seenSmallCaves,
    bool visitedSomeSmallTwice
) {
    if (location == "end") {
        paths.push_back(path);
        return;
    }

    for (const auto &to : g.at(location)) {
        Path subPath{path};
        subPath.push_back(to);

        bool subVisitedSomeSmallTwice{visitedSomeSmallTwice};
        if (seenSmallCaves.find(to) != seenSmallCaves.end()) {
            if (visitedSomeSmallTwice || to == "start") {
                continue;
            }
            subVisitedSomeSmallTwice = true;
        }

        SeenCaves subSeenSmallCaves{seenSmallCaves};
        if (std::islower(to[0])) {
            subSeenSmallCaves.insert(to);
        }

        findPaths(g, to, paths, subPath, subSeenSmallCaves, subVisitedSomeSmallTwice);
    }
}

auto answer(const Input &input) {
    Graph g = inputToGraph(input);
    Paths paths;
    findPaths(g, "start", paths, {"start"}, {"start"}, false);
    return paths.size();
}

int main(void) {
    std::cout << answer(readParseInput("../input/day12.txt")) << '\n';
    return 0;
}
