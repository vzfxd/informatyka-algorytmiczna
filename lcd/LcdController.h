#ifndef LcdController_h
#define LcdController_h

#include <LiquidCrystal_I2C.h>
#include "enums.h"

class LcdController{
    public:
        LcdController(LiquidCrystal_I2C*);
        void write_speed(enums::SIDE, int);
        void write_state(enums::SIDE, enums::STATE);
    private:
        LiquidCrystal_I2C* lcd;
        int calc_padding(int);
};


#endif
