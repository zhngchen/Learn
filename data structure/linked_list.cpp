#include <stdio.h>
#include <stdlib.h>
#include <stack>
using namespace std;
struct Node
{
    int data;
    Node* next;

    Node() {data = 0; next = nullptr;}

};

Node *head;

void Insert(int x);
void Print(Node *head);
void Insert(int data, int pos);
void Delete(int pos);
void Reverse();
void ReversePrint(Node *head)
{
    if (head == nullptr)
        return;
    
    ReversePrint(head->next);
    printf("%d ", head->data);
}
void Reverse(Node *p)
{
    if (p->next == nullptr)
    {
        head = p;
        return;
    }

    Reverse(p->next);
    Node *q = p->next;
    q->next = p;
    p->next = nullptr;

}

Node* reverse(Node *p)
{
    if (p == nullptr) return nullptr;
    stack<Node *> s;
    while(p != nullptr)
    {
        s.push(p);
        p = p->next;
    }

    auto tmp = s.top();
    auto head = tmp;
    s.pop();
    while(!s.empty())
    {
        tmp->next = s.top();
        s.pop();
        tmp = tmp->next;
    }

    tmp->next = nullptr;
    return head;
}
int main(int argc, char const *argv[])
{
    head = nullptr;
    Insert(2, 1);
    Insert(3, 2);
    Insert(4, 3);
    Insert(5, 4);
    Print(head);
    head = reverse(head);
    Print(head);
    return 0;
}

void Insert(int x)
{
    // Node *tmp = (struct Node*)malloc(sizeof(Node));
    Node *tmp = new Node();
    // Node *tmp = (Node *)Node();
    tmp->next = head;
    tmp->data = x;
    head = tmp;
}

void Print(Node *head)
{
    Node *tmp = head;
    printf("the linked list is \n");
    while (tmp != nullptr)
    {
        printf("%d ", tmp->data);
        tmp = tmp->next;
    }

    printf("\nprint ok\n");
}

void Insert(int data, int pos)
{
    // 暂时不处理异常
    auto *tmp1 = new Node();
    tmp1->data = data;
    tmp1->next = nullptr;

    if (pos == 1)
    {
        tmp1->next = head;
        head = tmp1;
        return;
    }

    auto tmp2 = new Node();
    tmp2 = head;

    for (int i = 0 ; i < pos-2; i++)
    {
        tmp2 = tmp2->next;
    } 

    tmp1->next = tmp2->next;
    tmp2->next = tmp1;

}

void Delete(int pos)
{
    auto tmp1 = head;
    if (pos == 1)
    {
        head = tmp1->next;
        delete tmp1;
        return;
    }

    for(int i = 0; i < pos-2; i++)
    {
        tmp1 = tmp1->next;
    }
    auto tmp2 = tmp1->next;
    tmp1->next = tmp2->next;
    delete tmp2;

}

void Reverse()
{
    Node* current, *prev, *next;
    current = head; 
    prev = nullptr;

    while(current != nullptr)
    {
        next = current->next;
        current->next = prev;
        prev = current;
        current = next;
    }

    head = prev;
}

