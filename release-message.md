# Ultibo bluetooth-dev repository

# Requires:
* a computer that can write an sd card
* an sd card that you can erase - its contents will be destroyed
* an RPI3B or RPI3B+ or RPI ZEROW with a power supply
    * _**The RPI3B+ and RPI ZEROW are untested**_

# Optional:
* usb keyboard
* an hdmi tv and an hdmi cable
* a blutetooth smart phone

# Steps:
* with the computer
    * download the zip file
    * format the sd card as FAT32
        * this destroys the current contents of the sd card
    * unzip it to the sd card
* on the optional smart phone
    * install Beacon Simulator (by Hiribarren)
* insert the sd card into the pi
* connect the pi to the tv using the hdmi cable
* connect the pi to the optional usb keyboard
* turn on the tv
* apply power to the pi
* the activity led on the pi should flash once per second
* if using a tv, you should see a green border with large white regions with black text

# Operation:
* the Beacon Simulator scanner tab on the optional bluetooth smart phone should list the ultibo device
* the optional tv screen should display bluetooth messages received in the environment
