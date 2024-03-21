#include <Arduino.h>
#include "enums.h"

#ifndef Wheels_h
#define Wheels_h

#define DEFINED_SPEED 80

class Wheels {
    public: 
        Wheels();
        
        void attachRight(int pinForward, int pinBack, int pinSpeed);
        void attachLeft(int pinForward, int pinBack, int pinSpeed);
        void attach(int pinRightForward, int pinRightBack, int pinRightSpeed,
                    int pinLeftForward, int pinLeftBack, int pinLeftSpeed);
        
        void forward();
        void forwardLeft();
        void forwardRight();
        void back();
        void backLeft();
        void backRight();
        void stop();
        void stopLeft();
        void stopRight();

        void setSpeed(uint8_t);
        void setSpeedRight(uint8_t);
        void setSpeedLeft(uint8_t);

        int getSpeedLeft();
        int getSpeedRight();

        void goForward(int cm);
        void goBack(int cm);

        enums::STATE getStateLeft();
        enums::STATE getStateRight();

    private: 
        int pinsRight[3];
        int pinsLeft[3];
        enums::STATE left_state;
        enums::STATE right_state;
        int curr_ls;
        int curr_rs;
};



#endif
