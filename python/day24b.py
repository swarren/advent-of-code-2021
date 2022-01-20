#!/usr/bin/env python3

import subprocess

instructions = []
with open("../input/day24.txt", "rt") as f:
    instructions = f.read().splitlines()

obin = "day24b.bin"
ofn = "day24b.cpp"
of = open(ofn, "wt")

indent_level = 0
def indent():
    global indent_level
    indent_level += 1
def dedent():
    global indent_level
    indent_level -= 1
def emit(s):
    for l in s.splitlines():
        of.write((indent_level * 4) * " ")
        of.write(l)
        of.write("\n")

emit("""\
#include <condition_variable>
#include <cstdint>
#include <iostream>
#include <mutex>
#include <thread>
#include <vector>

struct CpuState {
    int64_t x;
    int64_t y;
    int64_t z;
    int64_t w;
};

int64_t monad(int64_t digit0v, int64_t digit1v, int64_t digit2v) {
    CpuState cs{0};
    std::vector<CpuState> csSaved{};
""")

def f2c(field):
    if field[0] in "xyzw":
        return "cs." + field
    else:
        return field

indent()
digits = 0
for i in instructions:
    fields = i.split()
    if fields[0] == "inp":
        emit(f"csSaved.push_back(cs);")
        if digits == 0:
            max = "digit0v"
            min = "digit0v"
        elif digits == 1:
            max = "digit1v"
            min = "digit1v"
        elif digits == 2:
            max = "digit2v"
            min = "digit2v"
        else:
            max = 9
            min = 1
        emit(f"for (int64_t digit{digits} = {min}; digit{digits} <= {max}; digit{digits}++) {{")
        indent()
        emit(f"cs = csSaved.back();")
        emit(f"cs.{fields[1]} = digit{digits};")
        digits += 1
    elif fields[0] == "add":
        emit(f"cs.{fields[1]} += {f2c(fields[2])};")
    elif fields[0] == "mul":
        emit(f"cs.{fields[1]} *= {f2c(fields[2])};")
    elif fields[0] == "div":
        emit(f"cs.{fields[1]} /= {f2c(fields[2])};")
    elif fields[0] == "mod":
        emit(f"cs.{fields[1]} %= {f2c(fields[2])};")
    elif fields[0] == "eql":
        emit(f"cs.{fields[1]} = cs.{fields[1]} == {f2c(fields[2])};")
    else:
        raise Exception("???")

emit("if (cs.z == 0) {")
indent()
emit("int64_t ret = 0;")

for digit in range(digits):
    emit(f"ret *= 10;");
    emit(f"ret += digit{digit};")

emit("return ret;")
dedent()
emit("}")

for digit in range(digits):
    dedent()
    emit("}")
    emit("csSaved.pop_back();")

emit("return 0;")
dedent();
emit("""\
}

struct WorkItem {
    int64_t digit0v;
    int64_t digit1v;
    int64_t digit2v;
    bool complete;
    int64_t result;
};

std::mutex m;
std::condition_variable cv;
size_t completed;

void worker(WorkItem& work) {
    int64_t result = monad(work.digit0v, work.digit1v, work.digit2v);
    {
        std::unique_lock<std::mutex> lk(m);
        work.result = result;
        work.complete = true;
        completed++;
    }
    cv.notify_one();
}

int main(int argc, char *argv[]) {
    size_t nprocs = atoi(argv[1]);

    std::vector<WorkItem> work;
    work.reserve(9*9*9);
    for (int64_t digit0v = 1; digit0v <= 9; digit0v++) {
        for (int64_t digit1v = 1; digit1v <= 9; digit1v++) {
            for (int64_t digit2v = 1; digit2v <= 9; digit2v++) {
                work.push_back({digit0v, digit1v, digit2v, false, 0});
            }
        }
    }

    auto check_done = [&work]() {
        for (auto &wi : work) {
            if (!wi.complete)
                break;
            if (wi.result == 0)
                continue;
            std::cout << wi.result << '\\n';
            exit(0);
        }
    };

    size_t running = 0;
    size_t next_work_to_launch = 0;
    while (true) {
        while (true) {
            if (next_work_to_launch >= work.size())
                break;
            if (running >= nprocs)
                break;
            running++;
            new std::thread(worker, std::ref(work[next_work_to_launch]));
            next_work_to_launch++;
        }
        if (running == 0)
            break;
        {
            std::unique_lock<std::mutex> lk(m);
            cv.wait(lk, []{return completed > 0;});
            running -= completed;
            completed = 0;
            check_done();
        }
    }

    check_done();
    std::cout << "No answer?\\n";
    return 0;
}
""")

of.close()
subprocess.run(["g++", "-O3", "-ggdb", "-o", obin, ofn, "-lpthread"], check=True)
#subprocess.run(["./" + obin, "64"], check=True)
