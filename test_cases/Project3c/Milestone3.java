class Test {
    public static void main(String[] args) {
        //the { } is needed in order for there to be more than one statement
        {
            System.out.println(new Test2().Start(new Test2().Start(9)));
            System.out.println(new Plus().add(1, 2, 3));
        }
    }
}

class Test2 {
    public int Start(int y) {
        return y;
    }
}

class Plus {
    public int add(int x, int y, int z) {
        return 1 + 2 + 3;
    }
}