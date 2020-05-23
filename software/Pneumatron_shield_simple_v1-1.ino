int Pump = 6;
int Solenoid = 5;
//int Solenoid2 = 4;
//int Solenoid3 = 3;
//int Solenoid4 = 2;

long time_1 = 1;
long time_2 = 1;
long measure = 1;

#include "RTClib.h"
#include <SPI.h>
#include <SD.h>
#include <Wire.h>
#include <Adafruit_ADS1015.h>
Adafruit_ADS1115 ads(0x48);

float pressure = 0.0;
int Pino_CS = 10; 

RTC_DS1307 rtc;
File file;

void setup() {
   Serial.begin(9600);
   ads.begin();
   ads.setGain(GAIN_EIGHT);
// ads1015.setGain(GAIN_TWOTHIRDS);  // 2/3x gain +/- 6.144V  1 bit = 3mV (default)
// ads1015.setGain(GAIN_ONE);     // 1x gain   +/- 4.096V  1 bit = 2mV
// ads1015.setGain(GAIN_TWO);     // 2x gain   +/- 2.048V  1 bit = 1mV
// ads1015.setGain(GAIN_FOUR);    // 4x gain   +/- 1.024V  1 bit = 0.5mV
// ads1015.setGain(GAIN_EIGHT);   // 8x gain   +/- 0.512V  1 bit = 0.25mV
// ads1015.setGain(GAIN_SIXTEEN); // 16x gain  +/- 0.256V  1 bit = 0.125mV
   
  pinMode(6, OUTPUT); 
  pinMode(5, OUTPUT);
  //pinMode(4, OUTPUT);
  //pinMode(3, OUTPUT);
  //pinMode(2, OUTPUT);
     
    Serial.println("Data logger com modulo PN532");
    Serial.println();

   Serial.println("starting SD...");
  if (!SD.begin(Pino_CS))
  {
    Serial.println("error to start SD!");
    return;
  }
  Serial.println("SD card OK");
  Serial.println();

  if (! rtc.begin())
  {
    Serial.println("RTC not found!");
    while (1);
  }
  if (! rtc.isrunning())
  {
    Serial.println("RTC is not working");
    rtc.adjust(DateTime(F(__DATE__), F(__TIME__)));
    //A linha abaixo permite ajustar manualmente a data e hora do RTC
    //Formato: DateTime(ano, mes, dia, hora, minuto, segundo)
    //rtc.adjust(DateTime(2014, 1, 21, 3, 0, 0));
  }
  rtc.adjust(DateTime(F(__DATE__), F(__TIME__)));

  file = SD.open("log.txt", FILE_WRITE);
  file.print("date");
  file.print(' ');
  file.print("hour");
  file.print(' ');
  file.print("sequence");
  file.print(' ');
  file.print("rep");
  file.print(' ');
  file.print("time");
  file.print(' ');
  file.print("pressure");
  file.println("");
  file.close();

}

void loop() {
//solenoid #1
      for(int i = 0; i < 300; i++)// change time here (* 0.5 seconds)
    { 
     float pressure = ads.readADC_Differential_0_1();
     pressure = ((pressure)*(0.0183)+0.297);
             
     Serial.print(pressure, 5);
     Serial.println(" ");
                  
        if (pressure > 50) // you can change the pressure here (kPa)
        {
        digitalWrite(Pump, LOW);
        digitalWrite(Solenoid, LOW);  
        }
        else if (pressure < 10) // you can change the pressure here (kPa)
        {
        digitalWrite(Pump, HIGH);
        digitalWrite(Solenoid, HIGH);
        }
  file = SD.open("log.txt", FILE_WRITE);
  DateTime now = rtc.now();
  Serial.print(now.day() < 10 ? "0" : "");
  file.print(now.day(), DEC);
  file.print('/');
  file.print(now.month() < 10 ? "0" : "");
  file.print(now.month(), DEC);
  file.print('/');
  file.print(now.year(), DEC);
  file.print(' ');
  file.print(now.hour() < 10 ? "0" : "");
  file.print(now.hour(), DEC);
  file.print(':');
  file.print(now.minute() < 10 ? "0" : "");
  file.print(now.minute(), DEC);
  file.print(':');
  file.print(now.second() < 10 ? "0" : "");
  file.print(now.second(), DEC);
  file.print(' ');
  file.print(time_1);
  file.print(' ');
  file.print(measure);
  file.print(' ');
  file.print(time_2);
  file.print(' ');
  file.print(pressure, 5);
  file.println("");
  file.close();
        
        delay(500);
        time_1++;
        time_2++;
    }
    digitalWrite(Solenoid, HIGH);
    delay(1000);
    digitalWrite(Solenoid, LOW);
    delay(1000);
    digitalWrite(Solenoid, HIGH);
    delay(1000);
    digitalWrite(Solenoid, LOW);
    delay(1000);
    digitalWrite(Solenoid, HIGH);
    delay(1000);
    digitalWrite(Solenoid, LOW);   
    delay(745000); // TESTAR TEMPO. ESTÁ CONTANDO 8 SEGUNDO A MAIS, PARECE... 745000 ms = 12min 25sec between measurements (considering the measurments will take another 2 min 35 sec = 15 min in total)
                   //835000 ms = 13min 55sec between measurements (considering the measurments will take another 1 min 5 sec = 15 min in total)
    measure++;
    time_2 = 1;
    
    }
