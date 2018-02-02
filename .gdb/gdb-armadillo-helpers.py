"""GDB helper functions.
Tested with GNU gdb (GDB) Red Hat Enterprise Linux (7.2-92.el6)
"""
import os
import gdb
import numpy as np

class PrintArma(gdb.Command):
  """Print an Armadillo matrix."""
  def __init__(self):
    gdb.Command.__init__(self, "print-arma", gdb.COMMAND_DATA, gdb.COMPLETE_SYMBOL, True)

  def invoke(self, arg, from_tty):
    arg_list = gdb.string_to_argv(arg)
    if len(arg_list) == 0 or len(arg_list) > 1:
      print("Error! Usage: print-arma pointer_to_arma_matrix")
      return
    else:
      self._print_matrix(arg)

  def _print_matrix(self, ptr):
    ptr = str(ptr)
    n_rows = execute_output("print %s->n_rows" % ptr)
    n_cols = execute_output("print %s->n_cols" % ptr)
    matrix_string = execute_output("print (*%s->mem)@%s@%s" % (ptr, n_rows, n_cols))
    for k, v in [("{", "["), ("}", "]")]:
      matrix_string = matrix_string.replace(k, v)
    matrix_string = np.array(eval(matrix_string)).T
    print(matrix_string)

##
# Execute a GDB command with output capture
#
# @param command (str) GDB command
#
# @return command output (str)
#
def execute_output(command):

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

PrintArma()
