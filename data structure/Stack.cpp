#include <iostream>
#include <stdio.h>
#include <stack>
#include <string>
using namespace std;
#define MAX_SIZE 101

class StackMine
{
    private:
        int A[MAX_SIZE];
        int top;

    public:
        StackMine()
        {
            top = -1;
        }
        void push(int x)
        {
            if (top == MAX_SIZE-1)
            {
                printf("Error: stack is overflow");
            }
            A[++top] = x;
        }

        void pop()
        {
            if (top == -1)
            {
                printf("Error: No element to pop");
            }
            top--;
        }

        int top_element()
        {
            return A[top];
        }

        bool is_empty()
        {
            return top == -1;
        }

        // just for test
        void print()
        {
            printf("the stack is : ");
            for (int i = 0; i <= top; i++)
            {
                printf("%d ", A[i]);
            }
            printf("\n");
        }

};

bool check_balenced_parenthesis(string exp)
{
    auto match = [](char c1, char c2)
    {
        switch(c1) 
        {
            case '(' : return c2 == ')';
            break;

            case '[' : return c2 == ']';
            break;

            case '{' : return c2 == '}';
            break;

            default:
                throw "something unkownn happen";

        }

    };

    stack<char> ss;
    for (auto s : exp)
    {
        if (s == '(' || s == '[' || s == '{')
        {
            ss.push(s);
        }

        else if (s == ')' || s == ']' || s == '}')
        {
            if (ss.empty() || !match(ss.top(), s))
            {
                return false;
            }

            ss.pop();
        }
    }

    return ss.empty();
}

// help function, 尽管写了double但是并没有处理小数点
bool is_digit(char c)
{
    if (c <= '9' && c >= '0')
        return true;
    
    return false;
}
bool is_oprator(char c)
{
    if (c == '+' || c == '-' || c == '*' || c == '/')
        return true;
    
    return false;
}
double evaluate(char operation, double operand1, double operand2)
{
    switch(operation)
    {
        case '+': return operand1 + operand2; break;
        case '-': return operand1 - operand2; break;
        case '*': return operand1 * operand2; break;
        case '/': return operand1 / operand2; break;
        default:
            throw("invalid input!");
    }
}
double evaluate_postfix(string exp)
{
    stack<double> ss;

    for (size_t i = 0; i < exp.length(); i++)
    {
        if (exp[i] == ',' || exp[i] == ' ')
            continue;

        if (is_digit(exp[i]))
        {
            double res = 0;
            while (i < exp.length() && is_digit(exp[i]))
            {
                res = 10*res + (exp[i] - '0');
                i++;
            }
            i--;  // 这个很重要，不然operator可能没有录入
            ss.push(res);
        }

        else if (is_oprator(exp[i]))
        {
            auto op2 = ss.top(); ss.pop();
            auto op1 = ss.top(); ss.pop();
            auto res = evaluate(exp[i], op1, op2);

            ss.push(res);
        }

        else 
        {
            throw "invalid input!";
        }
    }

    auto res = ss.top(); ss.pop();
    
    // invalid input : 34+34
    if (ss.empty())
        return res;
    else 
        throw "invalid input";
}
// 对于前缀，只需要反向遍历即可
double evaluate_prefix(string exp)
{
    stack<double> ss;

    for (int i = exp.length() - 1; i >= 0; i--)
    {
        if (exp[i] == ',' || exp[i] == ' ')
            continue;

        if (is_digit(exp[i]))
        {
            double res = 0;
            while (i >= 0 && is_digit(exp[i]))
            {
                res = 10*res + (exp[i] - '0');
                i--;
            }
            i++;  // 这个很重要，不然operator可能没有录入
            ss.push(res);
        }

        else if (is_oprator(exp[i]))
        {
            auto op1 = ss.top(); ss.pop();
            auto op2 = ss.top(); ss.pop();
            auto res = evaluate(exp[i], op1, op2);

            ss.push(res);
        }

        else 
        {
            throw "invalid input!";
        }
    }

    auto res = ss.top(); ss.pop();
    
    // invalid input : 34+34
    if (ss.empty())
        return res;
    else 
        throw "invalid input";
}

// 只考虑单个符号
string infix2postfix(string exp)
{
    stack<char> ss;
    string res = "";

    auto is_high_weight = [](char operation1, char operation2)
    {
        auto get_operator_weight = [](char c)
        {
            switch(c)
            {
                case '+' : 
                case '-' : return 1; 
                case '*' : 
                case '/' : return 2; 
                default:
                    printf("invalid input");
            }
        };
        auto op1weight = get_operator_weight(operation1);
        auto op2weight = get_operator_weight(operation2);

        return op1weight >= op2weight;
    };

    auto is_openning_parenth = [](char c)
    {
        if (c == '(' || c == '[' || c == '{')
            return true;
        return false;
    };
    auto is_closing_parenth = [](char c)
    {
        if (c == ')' || c == ']' || c == '}')
            return true;
        return false;
    };
    for (size_t i = 0; i < exp.length(); i++)
    {
        if (exp[i] == ' ')
            continue;
        else if (is_digit(exp[i]))
        {
            res += exp[i];
        }
        else if (is_oprator(exp[i]))
        {
            while (!ss.empty() && !is_openning_parenth(ss.top()) && is_high_weight(ss.top(), exp[i]))
            {
                res += ss.top();
                ss.pop();
            }

            ss.push(exp[i]);
        }

        else if (is_openning_parenth(exp[i]))
            ss.push(exp[i]);
        // 其实还应该检查匹配
        else if (is_closing_parenth(exp[i]))
        {
            while (!ss.empty() && !is_openning_parenth(ss.top()))
            {
                res += ss.top();
                ss.pop();
            }

            ss.pop();  // 弹出左括号
        }
    }

    while (!ss.empty())
    {
        res += ss.top();
        ss.pop();
    }

    return res;
}

int main(int argc, char const *argv[])
{
    string exp = "(1+2)*3 - 5*2";
    string res = infix2postfix(exp);
    cout << res << endl;



    return 0;
}
