// http://stackoverflow.com/questions/5052211/changing-value-type-of-a-given-stl-container
#include <cstddef>

template <class Container, class NewType>
struct rebind;

template <class ValueType, class... Args,
          template <class...>
          class Container, 
          class NewType>

struct rebind<Container<ValueType, Args...>, NewType>
{
    typedef Container<NewType,
                      typename rebind<Args, NewType>::type...> type;
};



// std:array support

template <class ValueType, 
          std::size_t N, 
          template <class, std::size_t> class Container, 
          class NewType>

struct rebind<Container<ValueType, N>, NewType>
{
    typedef Container<NewType, N> type;
};
