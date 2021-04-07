#include <string>
#include <vector>
#include <iostream>
#include <algorithm>
using namespace std;

void insertion_sort(vector<int>& xs)
{
    for(int i = 1; i < xs.size(); i++)
        for (int j = i; j > 0 && xs[j-1] > xs[j]; j--)
            swap(xs[j], xs[j-1]);
}

void merge_sort(vector<int>&xs, vector<int>& tmp, int left, int rignt);
void merge(vector<int>& xs, vector<int>& tmp, int leftpos, int rightpos, int rightend);

void merge_sort(vector<int>& xs)
{
    vector<int> tmp(xs.size() - 1);
    merge_sort(xs, tmp, 0, xs.size()-1);
}
// 把xs的left-right 排好序
void merge_sort(vector<int>& xs, vector<int>& tmp, int left, int right)
{
    if (left < right)
    {
        int center = (left + right) / 2;
        merge_sort(xs, tmp, left, center);
        merge_sort(xs, tmp, center+1, right);
        merge(xs, tmp, left, center+1, right);
    }
}

// xs的两个区段分别是已经排序好的, 合并需要一个额外的储存空间
void merge(vector<int>& xs, vector<int>& tmp, int leftpos, int rightpos, int rightend)
{
    int leftend = rightpos - 1;
    int tmppos = leftpos;
    int count = rightend - leftpos + 1;

    while (leftpos <= leftend && rightpos <= rightend)
    {
        if (xs[leftpos] <= xs[rightpos])
            tmp[tmppos++] = xs[leftpos++];
        else
            tmp[tmppos++] = xs[rightpos++];
    }

    while (leftpos <= leftend)
        tmp[tmppos++] = xs[leftpos++];
    while (rightpos <= rightend)
        tmp[tmppos++] = xs[rightpos++];
    
    for(int i = 0; i < count; i++, rightend)
    {
        xs[rightend] = tmp[rightend];
    }
}

void print(vector<int> xs)
{
    for (auto x : xs)
    {
        cout << x << " ";
    }

    cout << endl;
}

void quicksort(vector<int>& xs, int l, int r)
{
    if (l + 1 >= r) return;

    int first = l, last = r - 1;
    int pivot = xs[first];  // 还有其他更好的办法选取

    while (first < last)
    {
        while(xs[first] < pivot) {first++;}
        while(xs[last] > pivot) {last--;}

        swap(xs[first], xs[last]);
    }

    swap(xs[first], pivot);

    quicksort(xs, l, first);
    quicksort(xs, first+1, r);
}

void bubble_sort(vector<int>& xs)
{
    for (int i = 0; i < xs.size(); i++)
        for (int j = 1; j < xs.size() - i + 1; j++)
        {
            if (xs[j] < xs[j-1])
                swap(xs[j], xs[j-1]);
        }
}

void selection_sort(vector<int>& xs)
{
    int n = xs.size();
    int min;

    for (int i = 0; i < n; i++)
    {
        min = i;
        for (int j = i+1; j < n; j++)
        {
            if (xs[j] < xs[min])
                min = j;
        }

        swap(xs[min], xs[i]);

    }
}

int main(int argc, char const *argv[])
{
    vector<int> xs {3,2,5,4,1,6};
    // insertion_sort(xs);
    // print(xs);

    // merge_sort(xs);
    // quicksort(xs, 0, xs.size());
    // bubble_sort(xs);
    selection_sort(xs);
    print(xs);
    return 0;
}
