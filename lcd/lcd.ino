#include <Wire.h> 
#include <LiquidCrystal_I2C.h>
#include "LcdController.h"
#include "Wheels.h"

#define pF 3
#define pB 4
#define lB 5
#define lF 6
#define lS 9
#define pS 10

#define SET_SIDE_SPEED_STATE(side,_speed,state)\
                               lcd_controller.write_speed(side, _speed);\
                               lcd_controller.write_state(side, state)

LiquidCrystal_I2C lcd(0x27,16,2);
LcdController lcd_controller(&lcd);
Wheels wheels;
volatile char serialInput;
volatile int delta;

void setup() {
  Serial.begin(9600);

  lcd.init();
  lcd.backlight();

  delta = 5;

  wheels.attach(pF,pB,pS,lF,lB,lS);
  wheels.setSpeed(DEFINED_SPEED);
}



void loop() {
  while(Serial.available())
  {
    uint8_t cs;
    serialInput = Serial.read();
    switch(serialInput)
    {
      //Przyspieszenie lewego koła
      case 'q' : cs = wheels.getSpeedLeft() + delta; break;

       //Zwolnienie lewego koła
      case 'a' : cs = wheels.getSpeedLeft() - delta; break; 

      //Przyspieszenie prawego koła
      case 'p' : cs = wheels.getSpeedRight() + delta; break;

      //Zwolnienie prawego koła
      case 'l' : cs = wheels.getSpeedRight() - delta; break; 

      //Lewe koło do przodu
      case 'w' : wheels.forwardLeft(); SET_SIDE_SPEED_STATE(enums::SIDE::LEFT,wheels.getSpeedLeft(),enums::STATE::FORWARD); break;

      //Lewe koło do tyłu
      case 'x' : wheels.backLeft(); SET_SIDE_SPEED_STATE(enums::SIDE::LEFT,-wheels.getSpeedLeft(),enums::STATE::BACK); break;

       //Lewe koło stop
      case 's' : wheels.stopLeft(); SET_SIDE_SPEED_STATE(enums::SIDE::LEFT,wheels.getSpeedLeft(),enums::STATE::STOP); break; 

      //Prawe koło do przodu
      case 'o' :wheels.forwardRight();SET_SIDE_SPEED_STATE(enums::SIDE::RIGHT,wheels.getSpeedLeft(),enums::STATE::FORWARD); break; 
      
      //Prawe koło do tyłu
      case 'm' :wheels.backRight(); SET_SIDE_SPEED_STATE(enums::SIDE::RIGHT,-wheels.getSpeedLeft(),enums::STATE::BACK); break; 
      
      //Prawe koło stop
      case 'k' : wheels.stopRight(); SET_SIDE_SPEED_STATE(enums::SIDE::RIGHT,wheels.getSpeedLeft(),enums::STATE::STOP);  break; 
    }

    if(serialInput == 'p' || serialInput == 'q')
    {
      wheels.setSpeedRight(cs);
        
        if(wheels.getStateRight() == enums::STATE::BACK)
          cs = -cs;

        lcd_controller.write_speed(enums::SIDE::RIGHT, cs);
    }
    else if(serialInput == 'q' || serialInput == 'a')
    {
      wheels.setSpeedLeft(cs);
        
        if(wheels.getStateLeft() == enums::STATE::BACK)
          cs = -cs;

        lcd_controller.write_speed(enums::SIDE::LEFT, cs);
    }
  }
}
