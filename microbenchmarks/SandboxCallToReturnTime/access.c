#include <checkcbox_extensions.h>
#include <time.h>
extern void c_BasicSbxCallThatJustReturns();
extern void w2c_BasicSbxCallThatJustReturns();

double BasicCheckedCallThatJustReturns()
{
    clock_t start, end;
    double cpu_time_used;
    start = clock();
    for (int i = 0; i < 100000; i++)
    {
        c_BasicSbxCallThatJustReturns(c_fetch_sandbox_address());
    }
    end = clock();
    cpu_time_used = ((double) (end-start)) / CLOCKS_PER_SEC;
    return cpu_time_used;


}
double BasicSbxCallThatJustReturns()
{
    clock_t start, end;
    double cpu_time_used;
    start = clock();
    for (int i = 0; i < 100000; i++)
    {
        w2c_BasicSbxCallThatJustReturns(c_fetch_sandbox_address());
    }
    end = clock();
    cpu_time_used = ((double) (end-start)) / CLOCKS_PER_SEC;
    return cpu_time_used;


}
int main() {

    printf("Time taken to call into checked function that just returns 100k times is %f \n", BasicSbxCallThatJustReturns());
    return 0;

}
