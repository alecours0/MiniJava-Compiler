class Test {
    public static void main(String[] args) {
        System.out.println(new Test2().Start(1, 2, 3, 4, 5));
    }
}

class Test2 {
    public int Start(int i1, int i2, int i3, int i4, int i5) {
        return (1 * i1) + (2 * i2) + (3 * i3) + (4 * i4) + (5 * i5);
    }
}