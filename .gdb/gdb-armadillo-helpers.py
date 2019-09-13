"""GDB helper functions.
Tested with GNU gdb (GDB) Red Hat Enterprise Linux (7.2-92.el6)
"""
import os
import gdb
import numpy as np

class ArmaApply(gdb.Command):
  def __init__(self):
    gdb.Command.__init__(self, "arma-apply", gdb.COMMAND_DATA, gdb.COMPLETE_SYMBOL, True)

  def invoke(self, arg, from_tty):
    arg_list = gdb.string_to_argv(arg)
    if len(arg_list) == 1 or len(arg_list) == 2:
      return self._apply_function(*arg_list)
    else:
      print("Error! Usage: arma-apply pointer np.function" )
      return

  def _apply_function(self, pointer, function_string=None):
    """Apply a function to an Armadillo array pointer. Print by default."""
    array = get_array(pointer)
    # print(pointer, "array of shape: ", array.shape)
    if function_string is None:
      print(array)
    else:
      function = eval(function_string)
      print(function(array))

def get_array(pointer):
  pointer = str(pointer)
  n_rows = execute_output("print %s->n_rows" % pointer)
  n_cols = execute_output("print %s->n_cols" % pointer)
  gdb.execute("set print repeats 0")
  gdb.execute("set print elements 0")
  gdb.execute("set pagination off")
  array_string = execute_output("print (*%s->mem)@%s@%s" % (pointer, n_rows, n_cols))
  for k, v in [("{", "["), ("}", "]")]:
    array_string = array_string.replace(k, v)
  return np.array(eval(array_string)).T

ArmaApply()

def execute_output(command):
  ##
  # Execute a GDB command with output capture
  #
  # @param command (str) GDB command
  #
  # @return command output (str)
  #

  # create temporary file for the output
  filename = os.getenv('HOME') + os.sep + 'gdb_output_' + str(os.getpid())

  # set gdb logging
  gdb.execute("set logging file " + filename)
  gdb.execute("set logging overwrite on")
  gdb.execute("set logging redirect on")
  gdb.execute("set logging on")

  # execute command
  try:
      gdb.execute(command)
  except:
      pass

  # restore normal gdb behaviour
  gdb.execute("set logging off")
  gdb.execute("set logging redirect off")

  # read output and close temporary file
  outfile = open(filename, 'r')
  output = outfile.read()
  outfile.close()

  # delete file
  os.remove(filename)

  # split lines
  # output = output.splitlines()

  return output.split("=")[1][1:]
