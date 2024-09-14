// MemoryProfiler.cpp

#include "MemoryProfiler.h"

// Global variables
IntervalTreeNode* IntervalTreeRoot = nullptr;
IntervalTreeNode* CacheNode = nullptr;
long lowerbound_1 = 0;
long upperbound_1 = 0;
long lowerbound_2 = 0;
long upperbound_2 = 0;

// Create a new interval tree node
IntervalTreeNode* createIntervalTreeNode(size_t low, size_t high) {
    return new IntervalTreeNode(low, high);
}

// Insert an interval into the interval tree
IntervalTreeNode* insertInterval(IntervalTreeNode* root, size_t low, size_t high) {
    if (root == nullptr) {
        return createIntervalTreeNode(low, high);
    }

    if (low < root->interval.low) {
        root->left = insertInterval(root->left, low, high);
    } else {
        root->right = insertInterval(root->right, low, high);
    }

    // Update the max value
    root->max = std::max(root->max, high);

    return root;
}

// Search for an interval containing a given address
IntervalTreeNode* searchInterval(IntervalTreeNode* root, size_t addr) {
    // Check cache first
    if (CacheNode != nullptr && CacheNode->interval.low <= addr && addr < CacheNode->interval.high) {
        return CacheNode;
    }

    if (root == nullptr) {
        return nullptr;
    }

    if (root->interval.low <= addr && addr < root->interval.high) {
        CacheNode = root;  // Update cache
        return root;
    }

    if (root->left != nullptr && addr < root->left->max) {
        return searchInterval(root->left, addr);
    }

    return searchInterval(root->right, addr);
}

// Delete an interval from the interval tree
IntervalTreeNode* deleteInterval(IntervalTreeNode* root, size_t low, size_t high) {
    if (root == nullptr) {
        return nullptr;
    }

    if (low < root->interval.low) {
        root->left = deleteInterval(root->left, low, high);
    } else if (low > root->interval.low) {
        root->right = deleteInterval(root->right, low, high);
    } else if (high == root->interval.high) {
        // Node found, delete it
        if (root->left == nullptr) {
            IntervalTreeNode* temp = root->right;
            if (root == CacheNode) CacheNode = nullptr;
            delete root;
            return temp;
        } else if (root->right == nullptr) {
            IntervalTreeNode* temp = root->left;
            if (root == CacheNode) CacheNode = nullptr;
            delete root;
            return temp;
        } else {
            // Node with two children
            IntervalTreeNode* temp = minValueNode(root->right);
            root->interval = temp->interval;
            root->right = deleteInterval(root->right, temp->interval.low, temp->interval.high);
        }
    } else {
        // Interval not found; handle partial overlaps if necessary
        // For simplicity, we assume exact matches
    }

    // Update the max value
    root->max = root->interval.high;
    if (root->left != nullptr) root->max = std::max(root->max, root->left->max);
    if (root->right != nullptr) root->max = std::max(root->max, root->right->max);

    return root;
}

// Find the node with minimum low value
IntervalTreeNode* minValueNode(IntervalTreeNode* node) {
    IntervalTreeNode* current = node;
    while (current && current->left != nullptr)
        current = current->left;
    return current;
}

// Delete the entire tree
void deleteTree(IntervalTreeNode* node) {
    if (node == nullptr) {
        return;
    }
    deleteTree(node->left);
    deleteTree(node->right);
    delete node;
}

// Print the tree (for debugging)
void printTree(IntervalTreeNode* node, int level) {
    if (node != nullptr) {
        printTree(node->right, level + 1);
        for (int i = 0; i < level; i++) {
            cout << "   ";
        }
        cout << "{ [" << node->interval.low << ", " << node->interval.high << "), max = " << node->max << " }" << endl;
        printTree(node->left, level + 1);
    }
}

// Implementation of CacheUpdateandCheck (unchanged)
extern "C" void CacheUpdateandCheck(void* Address) {
    if (Address == NULL)
        return;

    size_t addr = (size_t)Address;
    IntervalTreeNode* temp = searchInterval(IntervalTreeRoot, addr);

    assert(temp != nullptr && "Tainted Pointer Illegal");

    lowerbound_1 = temp->interval.low;
    upperbound_1 = temp->interval.high;
}

// Implementation of CacheUpdateandCheck_2 (unchanged)
extern "C" void CacheUpdateandCheck_2(void* Address, int64_t maxIndex) {
    if (maxIndex == 0)
        return;

    size_t addr = (size_t)Address;
    IntervalTreeNode* temp = searchInterval(IntervalTreeRoot, addr);

    assert(temp != nullptr && "Tainted Pointer Illegal");

    lowerbound_1 = temp->interval.low;
    upperbound_1 = temp->interval.high;
}