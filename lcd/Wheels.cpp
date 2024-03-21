#include <Arduino.h>

#include "Wheels.h"
#include "enums.h"

#define SET_MOVEMENT(side,f,b) digitalWrite( side[0], f);\
                               digitalWrite( side[1], b)

Wheels::Wheels() 
{ }

void Wheels::attachRight(int pF, int pB, int pS)
{
    pinMode(pF, OUTPUT);
    pinMode(pB, OUTPUT);
    pinMode(pS, OUTPUT);
    this->pinsRight[0] = pF;
    this->pinsRight[1] = pB;
    this->pinsRight[2] = pS;
}


void Wheels::attachLeft(int pF, int pB, int pS)
{
    pinMode(pF, OUTPUT);
    pinMode(pB, OUTPUT);
    pinMode(pS, OUTPUT);
    this->pinsLeft[0] = pF;
    this->pinsLeft[1] = pB;
    this->pinsLeft[2] = pS;
}

void Wheels::setSpeedRight(uint8_t s)
{
    this->curr_rs = s;
    analogWrite(this->pinsRight[2], s);
}

void Wheels::setSpeedLeft(uint8_t s)
{
    this->curr_ls;
    analogWrite(this->pinsLeft[2], s);
}

void Wheels::setSpeed(uint8_t s)
{
    setSpeedLeft(s);
    setSpeedRight(s);
}

void Wheels::attach(int pRF, int pRB, int pRS, int pLF, int pLB, int pLS)
{
    this->attachRight(pRF, pRB, pRS);
    this->attachLeft(pLF, pLB, pLS);
}

void Wheels::forwardLeft() 
{
    this->left_state = enums::STATE::FORWARD;
    SET_MOVEMENT(pinsLeft, HIGH, LOW);
}

void Wheels::forwardRight() 
{
    this->right_state = enums::STATE::FORWARD;
    SET_MOVEMENT(pinsRight, HIGH, LOW);
}

void Wheels::backLeft()
{
    this->left_state = enums::STATE::BACK;
    SET_MOVEMENT(pinsLeft, LOW, HIGH);
}

void Wheels::backRight()
{
    this->right_state = enums::STATE::BACK;
    SET_MOVEMENT(pinsRight, LOW, HIGH);
}

void Wheels::forward()
{
    this->forwardLeft();
    this->forwardRight();
}

void Wheels::back()
{
    this->backLeft();
    this->backRight();
}

void Wheels::stopLeft()
{
    this->curr_ls = 0;
    this->left_state = enums::STATE::STOP;
    SET_MOVEMENT(pinsLeft, LOW, LOW);
}

void Wheels::stopRight()
{
    this->right_state = enums::STATE::STOP;
    this->curr_rs = 0;
    SET_MOVEMENT(pinsRight, LOW, LOW);
}

void Wheels::stop()
{
    this->stopLeft();
    this->stopRight();
}

enums::STATE Wheels::getStateLeft()
{
  return this->left_state;
}

enums::STATE Wheels::getStateRight()
{
  return this->right_state;
}

int Wheels::getSpeedLeft()
{
    return this->curr_ls;
}

int Wheels::getSpeedRight()
{
    return this->curr_rs;
}

void Wheels::goForward(int cm)
{
    int ls = this->getSpeedLeft();
    int rs = this->getSpeedRight();
    setSpeed(DEFINED_SPEED);

    this->forward();
    delay(cm/20*1000);
    this->stop();

    this->setSpeedLeft(ls);
    this->setSpeedRight(rs);
}

void Wheels::goBack(int cm)
{
    int ls = this->getSpeedLeft();
    int rs = this->getSpeedRight();
    setSpeed(DEFINED_SPEED);

    this->back();
    delay(cm/20*1000);
    this->stop();

    this->setSpeedLeft(ls);
    this->setSpeedRight(rs);
}
