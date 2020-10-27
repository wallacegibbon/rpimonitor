%-----------------------------------------------------------------------------
% 15  14 13   12  11  10    9     8     7     6     5     4     3     2  1  0
%-----------------------------------------------------------------------------
% RST -  BRNG PG1 PG0 BADC4 BADC3 BADC2 BADC1 SADC4 SADC3 SADC2 SADC1 M3 M2 M1
%-----------------------------------------------------------------------------
% RST: 1 - reset INA219
% BRNG: bus voltage range, 0 - 16V;  1 - 32V
% PG: voltage for shunt register, 40mv, 80mv, 160mv, 320mv
% BADC: bus voltage adc, 9/10/11/12 bits, 1/2/4/8/16/32/64/128 samples
% SADC: shunt voltage adc, just like BADC
% M: mode

% Some info about INA219:
%
% 1. The Power Register(03h) is internally set to be 20 times the programmed
%    Current_LSB.
%
% 2. The Calibration Register is calculated based on the following equation:
%
%    Calibration_Register = trunc(0.04096 / (Current_LSB * Rshunt))
%
%    - 0.04096 is an internal fixed value used to ensure scaling is maintained
%      properly
%
% 3. Current_LSB = (Maximum_Expected_Current) / pow(2, 15)
%
% 4. Power_LSB = 20 * Current_LSB
%
% 5. Current_Register = Current_Register * Bus_Voltage_Register / 5000
%

% Config REGISTER (R/W)
-define(INA219_REG_CONFIG, 16#00).
% SHUNT VOLTAGE REGISTER (R)
-define(INA219_REG_SHUNTVOLTAGE, 16#01).
% BUS VOLTAGE REGISTER (R)
-define(INA219_REG_BUSVOLTAGE, 16#02).
% POWER REGISTER (R)
-define(INA219_REG_POWER, 16#03).
% CURRENT REGISTER (R)
-define(INA219_REG_CURRENT, 16#04).
% CALIBRATION REGISTER (R/W)
-define(INA219_REG_CALIBRATION, 16#05).

% set bus voltage range to 16V
-define(INA219_VOLTAGE_RANGE_16V, 16#00).
% set bus voltage range to 16V
-define(INA219_VOLTAGE_RANGE_32V, 16#01).

% shunt prog. gain set to  1, 40 mV range
-define(INA219_DIV_1_40MV, 16#00).
% shunt prog. gain set to /2, 80 mV range
-define(INA219_DIV_2_80MV, 16#01).
% shunt prog. gain set to /4, 160 mV range
-define(INA219_DIV_4_160MV, 16#02).
% shunt prog. gain set to /8, 320 mV range
-define(INA219_DIV_8_320MV, 16#03).

%  9bit,   1 sample,     84us
-define(INA219_ADCRES_9BIT_1S, 16#00).
% 10bit,   1 sample,    148us
-define(INA219_ADCRES_10BIT_1S, 16#01).
% 11 bit,  1 sample,    276us
-define(INA219_ADCRES_11BIT_1S, 16#02).
% 12 bit,  1 sample,    532us
-define(INA219_ADCRES_12BIT_1S, 16#03).

% 12 bit,  2 samples,  1.06ms
-define(INA219_ADCRES_12BIT_2S, 16#09).
% 12 bit,  4 samples,  2.13ms
-define(INA219_ADCRES_12BIT_4S, 16#0A).
% 12bit,   8 samples,  4.26ms
-define(INA219_ADCRES_12BIT_8S, 16#0B).
% 12bit,  16 samples,  8.51ms
-define(INA219_ADCRES_12BIT_16S, 16#0C).
% 12bit,  32 samples, 17.02ms
-define(INA219_ADCRES_12BIT_32S, 16#0D).
% 12bit,  64 samples, 34.05ms
-define(INA219_ADCRES_12BIT_64S, 16#0E).
% 12bit, 128 samples, 68.10ms
-define(INA219_ADCRES_12BIT_128S, 16#0F).

% power down
-define(INA219_MODE_POWERDOWN, 16#00).
% shunt voltage triggered
-define(INA219_MODE_SVOLT_TRIGGERED, 16#01).
% bus voltage triggered
-define(INA219_MODE_BVOLT_TRIGGERED, 16#02).
% shunt and bus voltage triggered
-define(INA219_MODE_SANDBVOLT_TRIGGERED, 16#03).
% ADC off
-define(INA219_MODE_ADCOFF, 16#04).
% shunt voltage continuous
-define(INA219_MODE_SVOLT_CONTINUOUS, 16#05).
% bus voltage continuous
-define(INA219_MODE_BVOLT_CONTINUOUS, 16#06).
% shunt and bus voltage continuous
-define(INA219_MODE_SANDBVOLT_CONTINUOUS, 16#07).

