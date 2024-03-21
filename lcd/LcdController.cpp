#include "LcdController.h"

LcdController::LcdController(LiquidCrystal_I2C* lcd) 
{
  this->lcd = lcd;
}

int LcdController::calc_padding(int s)
{
  int pad = 0;

  if(s<0)
    pad += 1;
  
  while((s/10)!=0)
  {
   pad += 1;
   s = s/10; 
  }
  
  return pad;
}

void LcdController::write_speed(enums::SIDE side, int s)
{
  uint8_t row = 0;
  uint8_t col;
  char cstr[16];

  if(side == enums::SIDE::RIGHT)
    col = 15 - this->calc_padding(s);
  else
    col = 0;

  this->lcd->setCursor(col,row);
  this->lcd->printstr(itoa(s, cstr, 10));
}

void LcdController::write_state(enums::SIDE side, enums::STATE state)
{
  uint8_t row = 1;
  uint8_t col;
  char char_state;
  char cstr[1];

  if(side == enums::SIDE::RIGHT)
    col = 15;
  else
    col = 0;
  
  switch(state)
  {
    case enums::STATE::FORWARD : char_state = 'F'; break;
    case enums::STATE::STOP : char_state = 'S'; break;
    case enums::STATE::BACK : char_state = 'B'; break;
  }
  cstr[0] = char_state;
  
  this->lcd->setCursor(col,row);
  this->lcd->printstr(cstr);
}
