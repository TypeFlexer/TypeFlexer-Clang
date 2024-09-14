#include <iostream>
#include <list>
#include <set>
#include <execinfo.h>
#include <dlfcn.h>

using namespace std;

const int HASH_SIZE = 100;

static list<void*> CallbackFunctions[HASH_SIZE];

static set<void*>TaintedFunctions;


extern "C" void registerCallback(void* callbackFunc)
{
 	printf("::::::::registerCallback on %p", callbackFunc);
	int index = ((const long)callbackFunc)%HASH_SIZE;
	/*DO THIS FROM THE INSTRUMENTATION
	if (CallbackFunction == NULL)
	{
		CallbackFunctions = new list<void*>[HASH_SIZE];
	}
	*/
	CallbackFunctions[index].push_back(callbackFunc);
	return;
}

extern "C" void unregisterCallback(void* callbackFunc)
{
	printf("::::::::unregisterCallback on %p\n", callbackFunc);
	int index = ((const long)callbackFunc)%HASH_SIZE;
	//find the key in the (index)th list
	list<void*>::iterator i; 
	for (i = CallbackFunctions[index].begin(); i != CallbackFunctions[index].end(); i++)
	{
		if (*i == callbackFunc)
			break;

	}
	//if key is found in the hash table, remove it
	if (i!= CallbackFunctions[index].end())
		CallbackFunctions[index].erase(i);
	return;
}


extern "C" void registerTaintedFunction(void* taintedFunc)
{
	printf("::::::::registerTainted Function on %p\n", taintedFunc);
	TaintedFunctions.insert(taintedFunc);		
}


extern "C" bool checkCallStackIntegrityForTaintedFunction(void* IndirectCalleeFunc)
{
	printf("::::::::checkCallStackIntegrity\n");

	auto it_T = TaintedFunctions.find(IndirectCalleeFunc);
	if (it_T != TaintedFunctions.end())
        {
                return true;
        }

	int index = ((const long)IndirectCalleeFunc)%HASH_SIZE;
	list<void*>::iterator it_C;
	for (it_C = CallbackFunctions[index].begin(); it_C != CallbackFunctions[index].end(); it_C++)
	{
		if (*it_C == IndirectCalleeFunc)
                        return true;
	}

	return false;
}
