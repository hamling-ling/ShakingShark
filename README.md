Shaking Shark
=========

#Overview

This project aims to make a electric toy device that selectably plays jaws theme sound phrase.
Whitch phrase to play depends on how fast the device is shaken. It gives sound effect for kids 
play of acting shark. Not like ordinaly melody player, physical accerelative movement of the 
device turns out the sound effect and it enhances fun of killer shark attack play for kids.

This project includes followings
- Electric circuit diagram(T.B.D)
- Microcontroller source code with AtmelStudio 6 solution

#Hardware Requirement
Following components are required to use this program.
See schematic diagram for detail.

Electrical Circuit
* Atmega168 or ATTiny45
* Accerelometer ADXL202
* Buzzer or Speaker
* Buttery

#How to use

1. Download AtmelStudio
2. open the atsln file and buid it
3. Use your avr writer to write the generated hex file
4. place the programmed AVR to your circuit

#How it work?

This matrics describes what phrase to play depending on input level.
What makes input level different is an accerelometer read. Continue shaking the device
make it plays next phrase.
Initial state is p0 which is mute and plays nothing.
Then input lv1 detected, state transit to p1 and play that sound.


    |    phrase|mute   |ta--da  |ta--da*2|tada*4 |talala-|talari-|tararira|taratara |tada*4 |..chacha|
    |input     |p0     |p1      |p2      |p3     |p4     |p5     |p6      |p7       |p8     |p9      |
    |------------------------------------------------------------------------------------------------|
    |lv0       |-      |p0      |p0      |p0     |p0     |p0     |p0      |p0       |p0     |p0      |
    |lv1       |p1     |p2      |p3      |p4     |p4     |p4     |p4      |p4       |p4     |p4      |
    |lv2       |p3     |p3      |p3      |p4     |p5     |p6     |p7      |p8       |p9     |p4      |
    
    -:do nothing

