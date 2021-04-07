#include <iostream>
#include <stdio.h>
#include <string>
using namespace std;

class Queue
{
    private:
        Queue* front;
        Queue* rear;
        Queue* next;
        int data;
    
    public:
        Queue(int data = 0) : data(data) {front = nullptr; rear = nullptr; next = nullptr;}

        void Enqueue(int x)
        {
            Queue* tmp = new Queue(x);
            if (front == nullptr)
            {
                front = tmp;
                rear = tmp;
                return;
            }

            rear->next = tmp;
            rear = tmp;
            return;
        }

        void Dequeue()
        {
            auto tmp = front;
            if (front == nullptr)
            {
                printf("error dequeue!");
                return;
            }
            if (front == rear)
            {
                front = nullptr; rear = nullptr;
            }
            else
            {
                front = front->next;
            }

            delete tmp;
        }

        void print()
        {
            auto tmp = front;
            while (tmp != nullptr)
            {
                cout << tmp->data << " ";
                tmp = tmp->next;
            }
            cout << endl;
        }
};


int main(int argc, char const *argv[])
{
    Queue q = Queue();
    q.Enqueue(1);
    q.Enqueue(2);
    q.Enqueue(3);
    q.Dequeue();
    q.print();
    return 0;
}
