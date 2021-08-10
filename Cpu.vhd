-- Cpu.vhd
--
--  Cpu
--    [clock_in] => clock input pin 
--    [reset_in] => reset input pin
--
--  Ertugrul Kalkan
--  ert.klkn@gmail.com
--  01/2020
--

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity Cpu is
  port(
    clock_in : in std_logic;
    reset_in : in std_logic
  );
end entity;

architecture struct of Cpu is

  type ram_array_t is array(0 to 255) of std_logic_vector(15 downto 0);

  -- signal Ram : ram_array_t := (others => x"0000");
  signal Ram : ram_array_t := (
--  "op.code" & "rM"  &  "rN" &  "rD", -- [addr] operation
--  "DATA_BH"  & "DATA_BL",            -- [addr]

    "0000000" & "000" & "000" & "000", -- 0x0000 NOP
    "00000000" & "00000000",           -- 0x0001

    "0000001" & "000" & "000" & "000", -- 0x0002 MOVDR AX, 0x0017  (AX = 0x0017)
    "00000000" & "00010111",           -- 0x0003

    "0000010" & "000" & "000" & "001", -- 0x0004 MOVR BX, AX       (BX = 0x0017)
    "00000000" & "00000000",           -- 0x0005

    "0000011" & "000" & "000" & "000", -- 0x0006 LOAD AX, [0x0015] (AX = 0x11AA)
    "00000000" & "00010101",           -- 0x0007

    "0000100" & "001" & "000" & "010", -- 0x0008 LOADR CX, [BX]    (CX = 0x0024)
    "00000000" & "00000000",           -- 0x0009

    "0001000" & "000" & "010" & "000", -- 0x000A ADDX AX, CX       (AX = 0x11CE)
    "00000000" & "00000000",           -- 0x000B

    "0000110" & "000" & "000" & "000", -- 0x000C ADD AX, 15        (AX = 0x11DD)
    "00000000" & "00001111",           -- 0x000D

    "0000101" & "000" & "000" & "001", -- 0x000E STORE BX, 0x0011  ([0x0011] = 0x0017)
    "00000000" & "00010001",           -- 0x000F

    "0000101" & "000" & "000" & "000", -- 0x0010 STORE AX, 0x0017
    "00000000" & "00000000",           -- 0x0011      -> this addr will be set to 0x0017

    "0001110" & "000" & "000" & "000", -- 0x0012 JMP [0x0020]
    "00000000" & "00100000",           -- 0x0013

    "0000000" & "000" & "000" & "000", -- 0x0014 NOP
    "00010001" & "10101010",           -- 0x0015

    "0000000" & "000" & "000" & "000", -- 0x0016 NOP
    "00000000" & "00100100",           -- 0x0017

    "0000000" & "000" & "000" & "000", -- 0x0018 NOP
    "00000000" & "00000000",           -- 0x0019

    "0000000" & "000" & "000" & "000", -- 0x001A NOP
    "00000000" & "00000000",           -- 0x001B

    "0000000" & "000" & "000" & "000", -- 0x001C NOP
    "00000000" & "00000000",           -- 0x001D

    "0000000" & "000" & "000" & "000", -- 0x001E NOP
    "00000000" & "00000000",           -- 0x001F

    "0000001" & "000" & "000" & "000", -- 0x0020 MOVDR AX, 0x0003  (AX = 0x0003)
    "00000000" & "00000011",           -- 0x0021

    "0000001" & "000" & "000" & "001", -- 0x0022 MOVDR BX, 0x000C  (BX = 0x000C)
    "00000000" & "00001100",           -- 0x0023

    "0000001" & "000" & "000" & "010", -- 0x0024 MOVDR CX, 0x0006  (CX = 0x0006)
    "00000000" & "00000110",           -- 0x0025

    "0001100" & "000" & "001" & "011", -- 0x0026 OR DX, AX, BX     (DX = AX or BX = 0x000F)
    "00000000" & "00000000",           -- 0x0027

    "0001101" & "011" & "010" & "011", -- 0x0028 XOR DX, CX        (DX = DX xor CX = 0x0009)
    "00000000" & "00000000",           -- 0x0029

    "0001011" & "011" & "000" & "011", -- 0x002A AND DX, AX        (DX = DX xor CX = 0x0001)
    "00000000" & "00000000",           -- 0x002B

    "0001111" & "000" & "000" & "011", -- 0x002C INC DX            (DX = 0x0002)
    "00000000" & "00000000",           -- 0x002D

    "0010000" & "000" & "000" & "011", -- 0x002E DEC DX            (DX = 0x0001)
    "00000000" & "00000000",           -- 0x002F

    "0010001" & "000" & "010" & "000", -- 0x0030 MULX AX, AX, CX   (AX = AX * CX = 0x0012)
    "00000000" & "00000000",           -- 0x0031

    others => 
    "00000000" & "00000000"
  );  -- RAM

  type reg_array_t is array(0 to 7) of std_logic_vector(15 downto 0);
  signal Reg : reg_array_t := (others => x"0000");

  signal program_counter : std_logic_vector(15 downto 0) := x"0000";
  signal instruction_register : std_logic_vector(15 downto 0) := x"0000";
  signal data_register : std_logic_vector(15 downto 0) := x"0000";

  -- program counter branching flag to prevent increasing new program counter value
  signal branch : std_logic := '0';

  -- multipication
  signal mul32 : std_logic_vector(31 downto 0) := x"00000000";
  signal mul_flag : std_logic := '0';

begin -- struct
  instruction_register <= Ram(to_integer(unsigned(program_counter)));
  data_register <= Ram(to_integer(unsigned(program_counter)) + 1);

  instruction: process(clock_in)
  begin
    if(rising_edge(clock_in)) then

--  instruction map:
--
--    0 1 2 3 4 5 6   7 0 1   2 3 4   5 6 7
--  +---------------+-------+-------+-------+
--  |     OPCODE    |  rM   |  rN   |  rD   |
--  +---------------+-------+-------+-------+
--   15      ...   downto       ...      0
--
--  opcode: 15 downto 9
--  rM:     8  downto 6
--  rN:     5  downto 3
--  rD:     2  downto 0

      case instruction_register(15 downto 9) is -- opcode

        -- NO OPERATION
        -- NOP
        when "0000000" =>
          null;   -- nop

        -- register[rD] = data_register
        -- MOVDR
        when "0000001" =>
          Reg(to_integer(unsigned(instruction_register(2 downto 0)))) <=
            data_register;

        -- register[rD] = register[rM]
        -- MOVR
        when "0000010" =>
          Reg(to_integer(unsigned(instruction_register(2 downto 0)))) <=
            Reg(to_integer(unsigned(instruction_register(8 downto 6))));

        -- register[rD] = ram[data_register]
        -- LOAD
        when "0000011" =>
          Reg(to_integer(unsigned(instruction_register(2 downto 0)))) <=
            Ram(to_integer(unsigned(data_register)))  ;

        -- register[rD] = ram[(register[rM])]
        -- LOADR
        when "0000100" =>
          Reg(to_integer(unsigned(instruction_register(2 downto 0)))) <=
            Ram(to_integer(unsigned(Reg(to_integer(unsigned(instruction_register(8 downto 6)))))));

        -- ram[data_register] = register[rD]
        -- STORE
        when "0000101" =>
          Ram(to_integer(unsigned(data_register))) <=
            Reg(to_integer(unsigned(instruction_register(2 downto 0))));

        -- register[rD] = register[rM] + data_register  {signed}
        -- ADD
        when "0000110" =>
          Reg(to_integer(unsigned(instruction_register(2 downto 0)))) <=
            std_logic_vector(signed(Reg(to_integer(unsigned(instruction_register(8 downto 6))))) +
              signed(data_register));

        -- register[rD] = register[rM] + data_register  {unsigned}
        -- UADD
        when "0000111" =>
          Reg(to_integer(unsigned(instruction_register(2 downto 0)))) <=
            std_logic_vector(unsigned(Reg(to_integer(unsigned(instruction_register(8 downto 6))))) +
              unsigned(data_register));

        -- register[rD] = register[rM] + register[rN]   {signed}
        -- ADDX
        when "0001000" =>
          Reg(to_integer(unsigned(instruction_register(2 downto 0)))) <=
            std_logic_vector(signed(Reg(to_integer(unsigned(instruction_register(8 downto 6))))) +
              signed(Reg(to_integer(unsigned(instruction_register(5 downto 3))))));

        -- register[rD] = register[rM] + register[rN]   {unsigned}
        -- UADDX
        when "0001001" =>
          Reg(to_integer(unsigned(instruction_register(2 downto 0)))) <=
            std_logic_vector(unsigned(Reg(to_integer(unsigned(instruction_register(8 downto 6))))) +
              unsigned(Reg(to_integer(unsigned(instruction_register(5 downto 3))))));

        -- register[rD] = not register[rM]
        -- NOT
        when "0001010" =>
          Reg(to_integer(unsigned(instruction_register(2 downto 0)))) <=
            not Reg(to_integer(unsigned(instruction_register(8 downto 3))));

        -- register[rD] = register[rM] and register[rN]
        -- AND
        when "0001011" =>
          Reg(to_integer(unsigned(instruction_register(2 downto 0)))) <=
            Reg(to_integer(unsigned(instruction_register(8 downto 6)))) and
              Reg(to_integer(unsigned(instruction_register(5 downto 3))));

        -- register[rD] = register[rM] or register[rN]
        -- OR
        when "0001100" =>
          Reg(to_integer(unsigned(instruction_register(2 downto 0)))) <=
            Reg(to_integer(unsigned(instruction_register(8 downto 6)))) or
              Reg(to_integer(unsigned(instruction_register(5 downto 3))));

        -- register[rD] = register[rM] xor register[rN]
        -- XOR
        when "0001101" =>
          Reg(to_integer(unsigned(instruction_register(2 downto 0)))) <=
            Reg(to_integer(unsigned(instruction_register(8 downto 6)))) xor
              Reg(to_integer(unsigned(instruction_register(5 downto 3))));

        -- program_counter = data_register
        -- JMP
        when "0001110" =>
          program_counter <= data_register;

        -- register[rD] = register[rD] + 1
        -- INC
        when "0001111" =>
          Reg(to_integer(unsigned(instruction_register(2 downto 0)))) <=
            std_logic_vector(unsigned(Reg(to_integer(unsigned(instruction_register(2 downto 0))))) + 1);

        -- register[rD] = register[rD] - 1
        -- DEC
        when "0010000" =>
          Reg(to_integer(unsigned(instruction_register(2 downto 0)))) <=
            std_logic_vector(unsigned(Reg(to_integer(unsigned(instruction_register(2 downto 0))))) - 1);

        -- register[rD] = register[rM] * register[rN]
        -- MULX
        when "0010001" =>
          mul32 <= std_logic_vector(
            signed(Reg(to_integer(unsigned(instruction_register(8 downto 6)))))
            * signed(Reg(to_integer(unsigned(instruction_register(5 downto 3)))))
          );
          mul_flag <= '1';

        when others =>
          null;
      end case;

      if (branch = '0') then
        program_counter <= std_logic_vector(unsigned(program_counter) + 2);
      else
        branch <= '0';
      end if;
    end if;

    if(falling_edge(clock_in)) then

      if(instruction_register(15 downto 9) = "0001110") then
        branch <= '1'; -- prevent increasing the pc if its jump operation
      end if;

      if(mul_flag = '1') then
        Reg(to_integer(unsigned(instruction_register(2 downto 0)))) <= mul32(15 downto 0);
        mul_flag <= '0';
      end if;

      if(reset_in = '1') then
        program_counter <= x"0000";
        branch <= '0';
        Reg <= (others => x"0000");
      end if;
    end if;
  end process;
end struct;