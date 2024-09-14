// MemoryProfiler.h
#ifndef MYPROJECT_MEMORYPROFILER_H
#define MYPROJECT_MEMORYPROFILER_H

#include <cstddef>
#include <cstdlib>
#include <iostream>
#include <cassert>

using namespace std;

// Define the Interval structure
struct Interval {
    size_t low;    // Start of the interval (inclusive)
    size_t high;   // End of the interval (exclusive)
};

// Define the IntervalTreeNode structure
struct IntervalTreeNode {
    Interval interval;             // The interval represented by this node
    size_t max;                    // Maximum high value in the subtree rooted at this node
    IntervalTreeNode* left;        // Left child
    IntervalTreeNode* right;       // Right child

    IntervalTreeNode(size_t low, size_t high) {
        interval.low = low;
        interval.high = high;
        max = high;
        left = nullptr;
        right = nullptr;
    }
    IntervalTreeNode() {
        interval.low = 0;
        interval.high = 0;
        max = 0;
        left = nullptr;
        right = nullptr;
    }
};

// Global variables
extern IntervalTreeNode* IntervalTreeRoot;
extern IntervalTreeNode* CacheNode;
extern long lowerbound_1;
extern long upperbound_1;
extern long lowerbound_2;
extern long upperbound_2;

// Extern "C" functions
extern "C" void CacheUpdateandCheck(void* Address);
extern "C" void CacheUpdateandCheck_2(void* Address, int64_t maxIndex);

// Function declarations
IntervalTreeNode* createIntervalTreeNode(size_t low, size_t high);
IntervalTreeNode* insertInterval(IntervalTreeNode* root, size_t low, size_t high);
IntervalTreeNode* searchInterval(IntervalTreeNode* root, size_t addr);
IntervalTreeNode* deleteInterval(IntervalTreeNode* root, size_t low, size_t high);
IntervalTreeNode* minValueNode(IntervalTreeNode* node);
void deleteTree(IntervalTreeNode* node);
void printTree(IntervalTreeNode* node, int level);

#endif // MYPROJECT_MEMORYPROFILER_H
