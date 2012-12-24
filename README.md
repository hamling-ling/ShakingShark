Shaking Shark
=========

#Introduction

This project aims to make a electric toy device that selectably plays jaws theme sound phrase.
Whitch phrase to play depends on how fast he device is shaken. It gives sound effect for kids 
play of acting shark. Not like ordinaly melody player, physical accerelative movement of the 
device turns out the sound effect. The mood of killer shark attack now turned out to a melody
and it enhances fun of shark play.

This project includes followings
- Electric circuit diagram(T.B.D)
- Microcontroller source code with AtmelStudio 6 solution

#Hardware Requirement
Following components are required to use this program.
* Atmega168 or ATTiny45
* Accerelometer ADXL202
* Buzzer or Speaker

#Transition Matricx

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

