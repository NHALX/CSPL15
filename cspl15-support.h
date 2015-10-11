#define __S1__(x) #x
#define __S2__(x) __S1__(x)
#define __FILE_LINE__ __FILE__ " : " __S2__(__LINE__)
