#ifndef __DUMMY_TEST_HH__
#define __DUMMY_TEST_HH__
/**-----------------------------------------------
  * @Copyright (C) 2021 All rights reserved.
  * @date: 2021
  * @file: dummy_test.hh
  * @version: v0.0.1
  * @author: aliben.develop@gmail.com
  * @create_date: 2021-12-04 20:23:59
  * @last_modified_date: NO_LAST_MODIFIED_DATE
  * @brief: TODO
  * @details: TODO
  *-----------------------------------------------*/

// Header include
#include <iostream>

// Declaration
namespace Demo
{
  class Obj
  {
    public:
      Obj() = default;
      virtual ~Obj() = default;

    public:
      int func_return1();
      int func_return2();
      void setValue(int value);
      int getValue() const;

    private:
      int value_;
  };

  double func_return10();
  double func_return20();
}
#endif // __DUMMY_TEST_HH__
