-- tb_Cpu.vhd
--
--  Ertugrul Kalkan
--  ert.klkn@gmail.com
--  01/2020
--

library ieee;
use ieee.std_logic_1164.all;

entity tb_Cpu is
  -- port();
end tb_Cpu;

architecture Behavioral of tb_Cpu is
  component Cpu is
    port(
      clock_in : in std_logic;
      reset_in : in std_logic
    );
  end component;

  signal clock : std_logic := '0';
  signal reset : std_logic := '0';

  constant clock_period : time := 10 ns;

begin
  CPUTB: Cpu port map(
    clock_in => clock,
    reset_in => reset
  );

  clock <= not clock after clock_period/2;
  
  process
  begin

    reset <= '0';
    wait for clock_period * 20;

    assert false report "end";
    wait;
  end process;
end Behavioral;