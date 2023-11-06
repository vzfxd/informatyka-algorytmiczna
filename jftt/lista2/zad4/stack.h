typedef struct StackElement{
    struct StackElement* prevElement;
    int data;
} StackElement;

typedef struct Stack{
    struct StackElement* top;
} Stack;

int top(Stack* stack){
    if(stack->top) return stack->top->data;
}

int isEmpty(Stack* stack){
    if(stack->top) return 0;
    return 1;
}

int pop(Stack* stack, int* error){
    if(stack->top){
        struct StackElement* temp = stack->top;
        int val = temp->data;
        stack->top = stack->top->prevElement;
        free(temp);
        return val;
    }else{
        *error = 1;
        return -1;
    }
}

void push(Stack* stack, int data){
    struct StackElement* stackElement = malloc(sizeof(StackElement));
    stackElement->data = data;
    stackElement->prevElement = stack->top;
    stack->top = stackElement;  
}