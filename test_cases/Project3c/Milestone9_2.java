class Factorial {
    public static void main(String[] a){
	System.out.println(new Array().print());
    }
}
class Array {
    int[] numbers;
    public int print() {
        int j;
        numbers = new int[3];
        numbers[0] = 1;
        numbers[1] = 2;
        numbers[2] = 3;
        j = this.print_array();
        j = numbers[1];
        System.out.println(j);
        j = numbers[j];
        System.out.println(j);
        return 0;
    }
    public int print_array() {
        int i;
        i = 0;
        while(i < numbers.length) {
            System.out.println(numbers[i]);
            i = i + 1;
        }
        return 0;
    }
}