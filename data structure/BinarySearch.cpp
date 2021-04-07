#include <queue>
#include <string>
#include <vector>
#include <iostream>
#include <algorithm>
using namespace std;

int binary_search(vector<int> xs, int val)
{
    // 找到返回索引否则-1
    int left = 0, right = xs.size() - 1;
    int mid;

    while (left <= right)
    {
        mid = (left + right) / 2;

        if (val == xs[mid])
            return mid;
        
        else if (val < xs[mid])
        {
            right = mid - 1;
        }

        else if (val > xs[mid])
        {
            left = mid + 1;
        }

    }

    return -1;

}

int main(int argc, char const *argv[])
{
    vector<int> xs{1, 2, 3, 4, 5, 6};
    auto res = binary_search(xs, 3);

    cout << res << " ";
    return 0;
}
