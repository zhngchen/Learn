#include <queue>
#include <string>
#include <vector>
#include <iostream>
#include <algorithm>
using namespace std;

class Bst
{
    public:
        Bst() {root = nullptr;}
        ~Bst() {make_empty(root);}
        bool search(const int &x) const;
        void insert(const int &x);
        void print();
        int find_min() const;
        int get_height() const;
        void level_order() const;
        void inorder() const;
        bool isBst() const;
        void remove(int x);
    
    private:
        struct BstNode
        {
            int data;
            BstNode* left;
            BstNode* right;

            BstNode(const int &x, BstNode* lt, BstNode* rt)
                : data(x), left(lt), right(rt) {}
        };

        BstNode* root;

        void insert(const int &x, BstNode* &t);
        bool search(const int &x, BstNode* t) const;
        void make_empty(BstNode* &t);
        int get_height(BstNode* t) const;   // 想声明为const BstNode* &t, 不行
        void inorder(BstNode* t) const;
        bool isBst(BstNode* root, int min, int max) const;
        int find_min(BstNode* t) const;
        BstNode* remove(BstNode* t, int x);
};

bool Bst::search(const int & x) const
{
    return search(x, root);
}

bool Bst::search(const int & x, BstNode* t) const
{
    if (t == nullptr)
        return false;
    else if (x < t->data)
        return search(x, t->left);
    else if (x > t->data)
        return search(x, t->right);
    else
        return true;
}

void Bst::insert(const int &x)
{
    insert(x, root);
}

void Bst::insert(const int &x, BstNode* &t)
{
    if (t == nullptr)
    {
        BstNode* tmp = new BstNode(x, nullptr, nullptr);
        t = tmp;
    }
    else if (x < t->data)
        insert(x, t->left);
    else if (x > t->data)
        insert(x, t->right);
    else
        return;
}

int Bst::find_min() const
{
    // auto current = root;
    // if (current == nullptr)
    // {
    //     cout << "error : no data!" << endl;
    //     return -1;
    // }

    // while (current->left != nullptr)
    //     current = current->left;
    
    // return current->data;
    return find_min(root);
}

int Bst::find_min(BstNode* t) const
{
    if (t == nullptr)
    {
        cout << "error : no data!" << endl;
        return -1;
    }

    while (t->left != nullptr)
        t = t->left;
    
    return t->data; 
}

int Bst::get_height() const
{
    return get_height(root);
}

int Bst::get_height(BstNode* t) const
{
    if (t == nullptr)
        return -1;  // height for edges
    
    auto left_height = get_height(t->left);
    auto right_height = get_height(t->right);
    auto max_height = (left_height > right_height ? left_height : right_height) + 1;
    return max_height;
}

void Bst::level_order() const
{
    if (root == nullptr) return;
    
    queue<BstNode*> q;
    q.push(root);

    while(!q.empty())
    {
        auto current = q.front();
        cout << current->data << " ";
        if (current->left != nullptr) q.push(current->left);
        if (current->right != nullptr) q.push(current->right);
        q.pop();
    }
    cout << endl;
}

void Bst::inorder() const
{
    inorder(root);
    cout << endl;
}

void Bst::inorder(BstNode* t) const
{
    if (t == nullptr)
        return;
    inorder(t->left);
    cout << t->data << " ";
    inorder(t->right);
}

bool Bst::isBst() const
{
    isBst(root, INT32_MIN, INT32_MAX);
}

bool Bst::isBst(BstNode* root, int min, int max) const
{
    if (root == nullptr)
        return true;
    
    if (root->data > min && root->data < max
        && isBst(root->left, min, root->data)
        && isBst(root->right, root->data, max))
        return true;
    else 
        return false;
}

void Bst::remove(int x)
{
    root = remove(root, x);
}

Bst::BstNode* Bst::remove(BstNode* t, int x)
{
    if (t == nullptr)
        return nullptr;
    
    if (x < t->data)
        t->left = remove(t->left, x);
    else if (x > t->data)
        t->right = remove(t->right, x);
    else
    {
        if (t->left == nullptr && t->right == nullptr)
        {
            delete t;
            t = nullptr;
        }

        else if(t->left == nullptr)
        {
            auto tmp = t->right;
            delete t;
            t = tmp;
        }

        else if(t->right == nullptr)
        {
            auto tmp = t->left;
            delete t;
            t = tmp;
        }
        
        else 
        {
            auto value = find_min(t->right);
            t->data = value;
            t->right = remove(t->right, value);
        }

    }

    return t;
}
void Bst::make_empty(BstNode* &t)
{
    if (t != nullptr)
    {
        make_empty(t->left);
        make_empty(t->right);
        delete t;
    }

    t = nullptr;
}
int main(int argc, char const *argv[])
{
    Bst t = Bst();
    t.insert(3);
    t.insert(1);
    t.insert(0);
    t.insert(2);
    t.insert(4);
    t.insert(8);
    t.insert(5);
    t.insert(10);

    // cout << t.search(2) << endl;

    cout << t.find_min() << endl;
    cout << t.get_height() << endl;
    t.level_order();
    t.inorder();

    cout << t.isBst() << endl;
    t.remove(1);
    t.inorder();
    return 0;
}
